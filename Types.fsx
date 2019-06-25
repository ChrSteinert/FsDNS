module DnsTypes

type Label = string

type Name = string

type RRType = 
    | A
    | NS
    | MD
    | MF
    | CNAME
    | SOA
    | MB
    | MG
    | MR
    | NULL
    | WKS
    | PTR
    | HINFO
    | MINFO
    | MX
    | TXT
    | AXFR
    | MAILB
    | MAILA
    | All

type RRClass = 
    | IN
    | CS
    | CH
    | HS
    | Any

type TTL = int32    
type RData = 
    {
        Length : uint16
        Data : byte array
    }

type ResourceRecord =
    {
        Name : Name
        Type : RRType
        Class : RRClass
        TimeToLive : TTL
        Data : RData
    }

type MessageType = Question | Response
type Opcode = Query | InverseQuery | Status
type ResponseCode = NoError | FormatError | ServerFailure | NameError | NotImplemented | Refused
type Header = 
    {
        Id : uint16
        MessageType : MessageType
        Opcode : Opcode
        IsAuthoritative : bool
        IsTruncated : bool
        IsRecursionDesired : bool
        RecursionAvailable : bool
        ResponseCode : ResponseCode 
        QuestionsCount : uint16
        AnswersCount : uint16
        NameServersCount : uint16
        AdditionalResourcesCount : uint16
    }

type Question = 
    {
        Name : string
        Type : RRType
        Class : RRClass
    }
    
type Answer = ResourceRecord

type Message =
    {
        Header : Header
        Questions : Question array
        Answers : Answer array
    }    