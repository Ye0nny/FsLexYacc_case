module TestParse

open System
open Xunit
open FsCheck
open FsCheck.Xunit

open Ast

open FSharp.Text.Lexing

let parseStmt (input:string) =
    let lexbuf = LexBuffer<char>.FromString input
    let output = Parser.stmt Lexer.token lexbuf
    output
    
let parseExpr (input:string) =
    let lexbuf = LexBuffer<char>.FromString input
    let output = Parser.expr Lexer.token lexbuf
    output

let priority op =
    match op with
    | Plus
    | Minus -> 10
    | Mul
    | Div -> 20

let opToString op =
    match op with
    | Plus -> "+"
    | Minus -> "-"
    | Mul -> "*"
    | Div -> "/"

[<Property(Verbose=true)>]
let ``test Priority`` a (op1:Op) b (op2:Op) c =
    let input = sprintf "%d %s %d %s %d" a (opToString op1) b (opToString op2) c
    let actual = parseExpr input
    let af = float a
    let bf = float b
    let cf = float c
    let expected = 
        if priority op1 >= priority op2 then 
            Binary(op2, Binary(op1, Num af, Num bf), Num cf)
        else
            Binary(op1, Num af, Binary(op2, Num bf, Num cf))
    actual = expected

let test () =
    ``test Priority`` 0 Minus 0 Minus 0
