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

let runExpr () = 
    printfn "Press Ctrl+c to Exit"

    while true do
        printf "Evaluate > "
        let input = Console.ReadLine()
        try
            let result = parseExpr input
            printfn "%A" (result |> Eval.exprEval)
        with ex -> printfn "%s" (ex.ToString())


let runStmt () = 
    printfn "Press Ctrl+c to Exit"

    while true do
        printf "Evaluate > "
        let input = Console.ReadLine()
        try
            let result = parseStmt input
            result |> Eval.stmtEval
        with ex -> printfn "%s" (ex.ToString())
   

[<EntryPoint>]
let main argv =
    runStmt ()
    0
