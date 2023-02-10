namespace startLib

module Say =
    let hello name =
        printfn "Hello %s" name

module JSer =
    open System.Text.Json

    let getjson value =
        let json = JsonSerializer.Serialize(value)
        value, json
