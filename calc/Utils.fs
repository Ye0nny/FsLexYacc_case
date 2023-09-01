module Utils

open System
open FSharp.Text.Lexing

let lexeme buf = 
    LexBuffer<_>.LexemeString buf


