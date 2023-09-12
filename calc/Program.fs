open System
open FSharp.Text.Lexing
open Eval

let parseStmt (input:string) =
    let lexbuf = LexBuffer<char>.FromString input
    let output = Parser.stmt Lexer.token lexbuf
    output
    
let parseExpr (input:string) =
    let lexbuf = LexBuffer<char>.FromString input
    let output = Parser.expr Lexer.token lexbuf
    output

let calc () = 
    printfn "Press Ctrl+c to Exit"

    while true do
        printf "Evaluate > "
        let input = Console.ReadLine()
        try
            let result = parseStmt input
            printfn "%A" result
        with ex -> printfn "%s" (ex.ToString())
   

[<EntryPoint>]
let main argv =
    calc ()
    0
