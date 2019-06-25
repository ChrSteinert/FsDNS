// #load "Types.fsx"
#load "Parser.fsx"
#load @"paket-files\ChrSteinert\BytesTable\BytesTable.fsx"

// fsi.AddPrinter BytesTable.BytesTable.getBytesTable

open System

open DnsParser

let query = // Query for A c.bing.com
    "HNoBAAABAAAAAAAAAWMEYmluZwNjb20AAAEAAQ=="
    |> Convert.FromBase64String

let response = // Answer for c.bing.com
    "HNqBgAABAAQAAAAAAWMEYmluZwNjb20AAAEAAcAMAAUAAQAABk4AIApjLWJpbmctY29tBmEtMDAw
    MQhhLW1zZWRnZQNuZXQAwCgABQABAAAAEAACwDPAMwABAAEAAAAQAATMT8XIwDMAAQABAAAAEAAE
    DWsVyA=="
    |> Convert.FromBase64String



DnsParser.parseMessage query
DnsParser.parseMessage response

response.[29] &&& 0b00111111uy
0b11000000