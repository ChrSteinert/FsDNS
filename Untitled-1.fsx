// #load "Types.fsx"
#load "Parser.fsx"
#load @"paket-files\ChrSteinert\BytesTable\BytesTable.fsx"

// fsi.AddPrinter BytesTable.BytesTable.getBytesTable

open System

open DnsParser

let message = 
    "ELOBgAABAAIAAAAAA2FwaQlyZW1lbWJlYXIDY29tAAABAAHADAABAAEAAACVAARoEUpswAwAAQABAAAAlQAEaBFJbA=="
    |> Convert.FromBase64String

message

DnsParser.parseMessage message