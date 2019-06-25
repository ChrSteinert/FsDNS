module DnsParser

#load "Types.fsx"

open DnsTypes

let private getUInt16 offset (message : byte array) =
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
        Id = message |> getUInt16 0
        MessageType = if message.[2] &&& 0b10000000uy = 128uy then Response else Question
        Opcode = opcode
        IsAuthoritative = message |> isBitsSet 2 0b00000100uy
        IsTruncated = message |> isBitsSet 2 0b00000010uy
        IsRecursionDesired = message |> isBitsSet 2 0b00000001uy
        RecursionAvailable = message |> isBitsSet 3 0b10000000uy
        ResponseCode = responseCode
        QuestionsCount = message |> getUInt16 4
        AnswersCount = message |> getUInt16 6
        NameServersCount = message |> getUInt16 8
        AdditionalResourcesCount = message |> getUInt16 10
    }

let private parseType = 
    function
    | 1us   -> A
    | 2us   -> NS
    | 3us   -> MD
    | 4us   -> MF
    | 5us   -> CNAME
    | 6us   -> SOA
    | 7us   -> MB
    | 8us   -> MG
    | 9us   -> MG
    | 10us  -> NULL
    | 11us  -> WKS
    | 12us  -> PTR
    | 13us  -> HINFO
    | 14us  -> MINFO
    | 15us  -> MX
    | 16us  -> TXT
    | 252us -> AXFR
    | 253us -> MAILB
    | 254us -> MAILA
    | 255us -> All
    | c -> failwithf "Unkown type value %i" c    

let private parseClass =
    function
    | 1us -> IN
    | 2us -> CS
    | 3us -> CH
    | 4us -> HS
    | c -> failwithf "Unknown class value %i" c    

let rec private getNameAndNewPos offset (message : byte array) =
    printfn "Reading name at %i" offset
    let isPointer = message |> isBitsSet offset 0b11000000uy
    if isPointer then
        let pointer = message |> getUInt16 offset &&& 0b0011111111111111us |> int
        getNameAndNewPos pointer message |> fst, offset + 1
    else 
        let mutable pos = offset
        printfn "pos %i" pos
        let mutable length = message.[pos]
        seq {
            while length > 0uy do
                yield 
                    message.[(pos + 1)..(pos + (length |> int))]
                    |> Array.map char |> System.String
                pos <- pos + (length |> int) + 1
                length <- message.[pos]
        }
        |> Seq.reduce (sprintf "%s.%s"), pos

let parseMessage (message : byte array) =
    let header = parseHeader message
    let mutable pos = 12
    let questions = 
        [|
            for __ = 1us to header.QuestionsCount do 
                let name = 
                    let name, pos' = getNameAndNewPos pos message
                    pos <- pos'
                    name

                let type' = 
                    message 
                    |> getUInt16 (pos + 1)
                    |> parseType

                let class' =
                    message 
                    |> getUInt16 (pos + 3)
                    |> parseClass

                yield { Name = name; Type = type'; Class = class' }
        |]
    
    pos <- pos + 5
    let answers = 
        [|
            for __ = 1us to header.AnswersCount do
                let name = 
                    let name, pos' = getNameAndNewPos pos message
                    pos <- pos'
                    name
                
                printfn "Reading type at %i" (pos + 1)
                let type' = 
                    message 
                    |> getUInt16 (pos + 1)
                    |> parseType

                printfn "Reading class at %i" (pos + 3)
                let class' =
                    message 
                    |> getUInt16 (pos + 3)
                    |> parseClass
                   
                let ttl = 
                    message 
                    |> getInt (pos + 5)

                let data = 
                    let length = message |> getUInt16 (pos + 9)
                    pos <- pos + (length |> int) + 2
                    let data = message.[(pos - 1 - (length |> int))..(pos - 1)]
                    { Length = length; Data = data }
                    
                yield { Name = name; Type = type'; Class = class'; TimeToLive = ttl; Data = data }
        |]

    
    { Header = header; Answers = answers; Questions = questions }