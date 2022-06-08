{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Cardano.Marketplace.Cli where

import Cardano.Api
import Cardano.Api.Byron (Address (ByronAddress))
import Cardano.Api.Shelley (Address (ShelleyAddress), AsType (AsAlonzoEra), Lovelace (Lovelace), ProtocolParameters, fromPlutusData, fromShelleyStakeReference, scriptDataToJsonDetailedSchema, shelleyPayAddrToPlutusPubKHash, toPlutusData, toShelleyStakeAddr, toShelleyStakeCredential)
import qualified Cardano.Api.Shelley as Shelley
import Cardano.Kuber.Api
import Cardano.Kuber.Data.Parsers (parseAssetIdText, parseAssetNQuantity, parseScriptData, parseTxIn, parseValueText, scriptDataParser)
import Cardano.Kuber.Util hiding (toHexString)
import Cardano.Ledger.Alonzo.Tx (TxBody (txfee))
import qualified Cardano.Ledger.BaseTypes as Shelley (Network (..))
import Cardano.Marketplace.Common.ConsoleWritable
import Cardano.Marketplace.Common.TextUtils
import Cardano.Marketplace.V1.Core
import Cardano.Marketplace.V1.RequestModels
import Cardano.Marketplace.V1.ServerRuntimeContext
import Codec.Serialise (serialise)
import Control.Exception (throwIO)
import Control.Monad (void)
import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Builder as Text
import qualified Data.ByteString.Char8 as BS8
import Data.ByteString.Lazy.Char8 (toStrict)
import Data.Char (toLower)
import Data.Data (Data, Typeable)
import Data.Functor ((<&>))
import Data.List (intercalate, isSuffixOf, sort)
import qualified Data.Map as Map
import Data.Maybe (fromMaybe, mapMaybe)
import qualified Data.Set as Set
import Data.Text (Text, strip)
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as TL
import Plutus.Contracts.V1.SimpleMarketplace (SimpleSale (..), simpleMarketplacePlutus)
import qualified Plutus.Contracts.V1.SimpleMarketplace as SMP
import Plutus.V1.Ledger.Api (ToData (toBuiltinData))
import qualified Plutus.V1.Ledger.Api as Plutus
import System.Console.CmdArgs
import System.Directory (doesFileExist, getCurrentDirectory, getDirectoryContents)

-- getAddrEraFromSignKey signKey =
--   skeyToAddrInEra signKey . getNetworkId <$> chainInfoFromEnv

data Modes
  = Cat -- Cat script binary
  | Sell
      { item :: String,
        cost :: Integer -- cost in Lovelace
      }
  | Buy
      { txin :: Text,
        datum :: String
      }
  | Ls -- List utxos for market
  | Withdraw
      { txin :: Text,
        datum :: String
      }
  | Mint
  | CreateCollateral
  deriving (Show, Data, Typeable)

runCli :: IO ()
runCli = do
  op <-
    cmdArgs $
      modes
        [ Cat &= help "Cat script binary",
          Sell
            { item = def &= typ "Asset" &= argPos 0,
              cost = def &= typ "Price" &= argPos 1
            }
            &= details ["  Place an asset on sale", "  Eg. sell 8932e54402bd3658a6d529da707ab367982ae4cde1742166769e4f94.Token \"2000000\""],
          Buy
            { txin = "" &= typ "TxIn" &= argPos 0,
              datum = "" &= typ "Datum" &= argPos 1
            } &= details ["Buy an asset on sale after finiding out txIn from market-cli ls.", "  Eg. buy '8932e54402bd3658a6d529da707ab367982ae4cde1742166769e4f94#0' '{\"fields\":...}'"],
          Ls &= help "List utxos for market",
          Withdraw
            { txin = "" &= typ "TxIn" &= argPos 0,
              datum = "" &= typ "Datum" &= argPos 1
            } &= details ["Withdraw an asset on sale after finiding out txIn from market-cli ls.", "  Eg. withdraw '8932e54402bd3658a6d529da707ab367982ae4cde1742166769e4f94#0' '{\"fields\":...}'"],
          Mint &= help "Mint a new asset",
          CreateCollateral &= help "Create a new collateral utxo."
        ]
        &= program "market-cli"
  ctx <- chainInfoFromEnv
  let marketAddr = marketAddressShelley (getNetworkId ctx)
  case op of
    Ls -> do
      utxos <- queryMarketUtxos ctx marketAddr
      putStrLn $ "Market Address : " ++ T.unpack (serialiseAddress marketAddr)
      putStrLn $ toConsoleText "  " utxos
    Cat -> do
      let scriptInCbor = serialiseToCBOR simpleMarketplacePlutus
      putStrLn $ toHexString scriptInCbor
    Sell itemStr cost -> do
      sKey <- getDefaultSignKey
      let addrShelley = skeyToAddr sKey (getNetworkId ctx)
          sellerAddrInEra = getAddrEraFromSignKey ctx sKey
      item <- parseAssetNQuantity $ T.pack itemStr
      let lockedValue = valueFromList [item, (AdaAssetId, 2_000_000)]
          saleDatum = constructDatum addrShelley cost
          txOperations =
            txPayToScript (marketAddressInEra $ getNetworkId ctx) lockedValue (hashScriptData saleDatum)
              <> txWalletAddress sellerAddrInEra
      txBodyE <- txBuilderToTxBodyIO ctx txOperations
      txBody <- case txBodyE of
        Left err -> error $ "Error in creating transaction " ++ show err
        Right txBody -> return txBody
      tx <- signAndSubmitTxBody (getConnectInfo ctx) txBody [sKey]
      putStrLn $ "Sale Transaction submitted sucessfully with transaction hash " ++ getTxIdFromTx tx

      putStrLn "\nDatum to be used for buying :"
      putStrLn (BS8.unpack $ toStrict $ Aeson.encode $ scriptDataToJsonDetailedSchema saleDatum)

      putStrLn $ "\nMarket Address : " ++ T.unpack (serialiseAddress marketAddr)
    Buy txInStr datum -> do
      dcInfo <- withDetails ctx
      let datumObj = unMaybe "Error : Invalid datum json string." $ Aeson.decode $ TL.encodeUtf8 $ TL.pack datum
      scriptData <- scriptDataParser datumObj
      let simpleSale@SimpleSale {sellerAddress, priceOfAsset} = unMaybe "Failed to convert datum to SimpleSale" $ Plutus.fromData $ toPlutusData scriptData
      print scriptData
      print simpleSale

      sKey <- getDefaultSignKey
      txIn <- parseTxIn txInStr
      UTxO uMap <- queryMarketUtxos ctx marketAddr

      let txOut = unMaybe "Error couldn't find the given txin in market utxos." $ Map.lookup txIn uMap

      if not $ matchesDatumhash (hashScriptData scriptData) txOut
        then error "Error : The given txin doesn't match the datumhash of the datum."
        else do
          let nwId = getNetworkId ctx
              buyerAddr = getAddrEraFromSignKey ctx sKey
              sellerAddrInEra = plutusAddressToAddressInEra nwId sellerAddress
              sellerPayOperation = txPayTo sellerAddrInEra (ensureMinAda sellerAddrInEra (lovelaceToValue $ Lovelace priceOfAsset) (dciProtocolParams dcInfo))
              redeemUtxoOperation = txRedeemUtxo txIn txOut marketScriptToScriptInAnyLang scriptData (fromPlutusData $ Plutus.toData SMP.Buy)
              txOperations =
                sellerPayOperation
                  <> redeemUtxoOperation
                  <> txWalletAddress buyerAddr
          txBodyE <- txBuilderToTxBodyIO dcInfo txOperations
          txBody <- case txBodyE of
            Left fe -> throwIO fe
            Right txBody -> pure txBody
          tx <- signAndSubmitTxBody (getConnectInfo ctx) txBody [sKey]
          putStrLn $ "Buy Transaction submitted sucessfully with transaction hash " ++ getTxIdFromTx tx
          putStrLn "Done"
      where
        matchesDatumhash sDataHash (TxOut _ (TxOutValue _ value) (TxOutDatumHash _ hash)) = hash == sDataHash
        matchesDatumhash _ _ = False

        hasAsset asset (TxOut _ (TxOutValue _ value) _) = selectAsset value asset > 0
        hasAsset _ _ = False

        marketScriptToScriptInAnyLang = ScriptInAnyLang (PlutusScriptLanguage PlutusScriptV1) (PlutusScript PlutusScriptV1 simpleMarketplacePlutus)

        ensureMinAda :: AddressInEra AlonzoEra -> Value -> ProtocolParameters -> Value
        ensureMinAda addr value pParams =
          if diff > 0
            then value <> lovelaceToValue diff
            else value
          where
            diff = minLovelace - currentLovelace
            minLovelace = calculateMinimumLovelace ShelleyBasedEraAlonzo addr value pParams
            currentLovelace = selectLovelace value
    Withdraw txIn datum -> do
      -- skey <- getCurrentSkey
      -- scriptData <- parseScriptData assetid
      -- let modal = WithdrawReqModel{
      --     withdrawDatum=scriptData,
      --     withdrawUtxo=Nothing,
      --     withdrawAddress=Nothing,
      --     withdrawAsset=Nothing,
      --     withdrawCollateral=Nothing
      --   }
      -- TxResponse tx datums <-withdrawCommand rCtx  modal
      -- putStrLn $ "Submited Tx :"++ tail (init $ show $ getTxId $ getTxBody tx)
      putStrLn "Done"
    Mint -> do
      skey <- getDefaultSignKey
      simpleMintTest ctx skey
    CreateCollateral -> do
      skey <- getDefaultSignKey
      let addrInEra = getAddrEraFromSignKey ctx skey
          txOperations = txPayTo addrInEra (lovelaceToValue $ Lovelace 5_000_000) <> txWalletAddress addrInEra
      txBodyE <- txBuilderToTxBodyIO ctx txOperations
      txBody <- case txBodyE of
        Left err -> error $ "Error in creating transaction " ++ show err
        Right txBody -> return txBody
      tx <- signAndSubmitTxBody (getConnectInfo ctx) txBody [skey]
      putStrLn $ "Collateral Transaction submitted sucessfully with transaction hash " ++ getTxIdFromTx tx
      print "Done"
