module Utils

open System
open FSharp.Text.Lexing

let lexeme buf = 
    LexBuffer<_>.LexemeString buf

/// 여러줄을 입력받는 함수
/// 'END'로 입력 종료
let rec readLines (acc: string) =
    let input = Console.ReadLine()
    if input = "END" then
        acc
    else
        readLines (acc + input + "\r\n")


