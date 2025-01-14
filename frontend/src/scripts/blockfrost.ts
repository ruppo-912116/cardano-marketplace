const blockfrostKey = "testnetfw496vwbVTrYv5tFDIpTDLqcelb947po"
const blockfrostUrl = "https://cardano-testnet.blockfrost.io/api/v0"

export function listMarket() {
    return getBlockfrost("/addresses/" + 'addr_test1wp04uqjsfjeaqgrzq60y8aajuqhcaa3ar4jp227k5w2v8hs4p9my9' + "/utxos?order=desc")
}

export function getAssetDetail(asset: string) {
    return getBlockfrost("/assets/" + asset)
}

export function getDatum(hash: string) {
    return getBlockfrost("/scripts/datum/" + hash)
}

function getBlockfrost(path) {
    const url= blockfrostUrl + path
    return fetch(url, {
        headers: {project_id: blockfrostKey}
    }).then(res => {
        if (res.status === 200) {
            return res.json()
        } else {
            return res.text().then(txt => {
                let err
                let json: any
                try {
                    json = JSON.parse(txt)
                    if (json) {
                        err = Error(`BlockfrostApi [Status ${res.status}] : ${json.message ? json.message : txt}`)
                        err.json = json
                    } else {
                        err = Error(`BlockfrostApi [Status ${res.status}] : ${txt}`)
                        err.text = txt
                    }
                } catch (e) {
                    err = Error(`BlockfrostApi [Status ${res.status}] : ${txt}`)
                    err.text = txt
                }
                err.response=res
                err.url=url
                err.status_code=res.status
                throw(err)
            })
        }
    })
}
