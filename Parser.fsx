module DnsParser

#load "Types.fsx"

open DnsTypes

let private getUInt offset (message : byte array) =
    message.[offset] |> uint16 <<< 8 
    ||| (message.[offset + 1] |> uint16)

let private getInt offset (message : byte array) = 
    let mutable result = 0
    for i = 0 to 3 do
        result <- result ||| (message.[offset + i] |> int <<< (8 * (3 - i)))
    
    result

let private isBitsSet offset mask (message : byte array) =
    message.[offset] &&& mask = mask    

let private parseHeader (message : byte array) =
    let opcode = 
        let code = message.[2] &&& 0b01111000uy >>> 3
        match code with
        | 0uy -> Query
        | 1uy -> InverseQuery
        | 2uy -> Status
        | c -> failwithf "Unknown opcode %i" c
    
    let responseCode =
        let code = message.[3] &&& 0b00001111uy
        match code with
        | 0uy -> NoError
        | 1uy -> FormatError
        | 2uy -> ServerFailure
        | 3uy -> NameError
        | 4uy -> NotImplemented
        | 5uy -> Refused
        | c -> failwithf "Unkown response code %i" c

    {
        Id = message |> getUInt 0
        MessageType = if message.[2] &&& 0b10000000uy = 128uy then Response else Question
        Opcode = opcode
        IsAuthoritative = message |> isBitsSet 2 0b00000100uy
        IsTruncated = message |> isBitsSet 2 0b00000010uy
        IsRecursionDesired = message |> isBitsSet 2 0b00000001uy
        RecursionAvailable = message |> isBitsSet 3 0b10000000uy
        ResponseCode = responseCode
        QuestionsCount = message |> getUInt 4
        AnswersCount = message |> getUInt 6
        NameServersCount = message |> getUInt 8
        AdditionalResourcesCount = message |> getUInt 10
    }

let private parseType = 
    function
    | 1us -> A
    | 2us -> NS
    | 3us -> MD
    | c -> failwithf "Unkown type value %i" c    

let private parseClass =
    function
    | 1us -> IN
    | 2us -> CS
    | 3us -> CH
    | 4us -> HS
    | c -> failwithf "Unknown class value %i" c    

let parseMessage (message : byte array) =
    let header = parseHeader message
    let mutable pos = 12
    let mutable currentLength = message.[pos]
    let questions = 
        [|
            for __ = 1us to header.QuestionsCount do 
                let name = 
                    seq {
                        while currentLength > 0uy do
                            yield
                                message.[(pos + 1)..(pos + (currentLength |> int))]
                                |> Array.map char |> System.String
                            pos <- pos + (currentLength |> int) + 1
                            currentLength <- message.[pos]
                    }
                    |> Seq.reduce (sprintf "%s.%s")

                let type' = 
                    let code = message |> getUInt (pos + 1)
                    match code with 
                    | 1us -> A
                    | 2us -> NS
                    | 3us -> MD
                    | c -> failwithf "Unkown type value %i" c

                let class' =
                    let code = message |> getUInt (pos + 3)
                    match code with
                    | 1us -> IN
                    | 2us -> CS
                    | 3us -> CH
                    | 4us -> HS
                    | c -> failwithf "Unknown class value %i" c

                yield { Name = name; Type = type'; Class = class' }
        |]
    { Header = header; Answers = [||]; Questions = questions }