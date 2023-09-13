open System
open FSharp.Text.Lexing
open Eval
open Utils

let parseInput (input:string) =
    let lexbuf = LexBuffer<char>.FromString input
    let stmts = Parser.input Lexer.token lexbuf
    stmts
    
let parseExpr (input:string) =
    let lexbuf = LexBuffer<char>.FromString input
    let expr = Parser.expr Lexer.token lexbuf
    expr


let runExpr () = 
    printfn "Press Ctrl+c to Exit"

    while true do
        printf "Evaluate > "
        let input = Console.ReadLine()
        try
            let result = parseExpr input
            printfn "%A" (result |> Eval.exprEval)
        with ex -> printfn "%s" (ex.ToString())


let runStmts () = 
    printfn "Press Ctrl+c to Exit."
    printfn "The input cycle ends with the 'END' keyword."

    while true do
        printf "Evaluate > \n"
        let input = readLines ""
        try
            parseInput input |> printfn "%A"
            //let stmts = parseInput input
            //stmts |> List.iter (fun x -> Eval.stmtEval x)
        with ex -> printfn "%s" (ex.ToString())


[<EntryPoint>]
let main argv =
    runStmts ()
    0
