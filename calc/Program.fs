open System
open FSharp.Text.Lexing

let evaluate (input:string) =
    let lexbuf = LexBuffer<char>.FromString input
    let output = Parser.input Lexer.token lexbuf
    string output
    

let calc () = 
    printfn "Press Ctrl+c to Exit"

    while true do
        printf "Evaluate > "
        let input = Console.ReadLine()
        try
            let result = evaluate input
            printfn "%s" result
        with ex -> printfn "%s" (ex.ToString())
   

[<EntryPoint>]
let main argv =
    calc ()
    0
