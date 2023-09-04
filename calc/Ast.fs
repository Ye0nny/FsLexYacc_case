module Ast

type Expr =
    | Num of float
    | Plus of Expr * Expr
    | Minus of Expr * Expr
    | Mul of Expr * Expr
    | Div of Expr * Expr


let rec _evaluate (p1:Expr) =
    let result =
        match p1 with
        | Num(x) -> x
        | Plus(Num(x), Num(y)) -> x + y
        | Minus(Num(x), Num(y)) -> x - y
        | Mul(Num(x), Num(y)) -> x * y
        | Div(Num(x), Num(y)) -> x / y
        | Plus(x, y) -> (_evaluate x) + (_evaluate y)
        | Minus(x, y) -> (_evaluate x) - (_evaluate y)
        | Mul(x, y) -> (_evaluate x) * (_evaluate y)
        | Div(x, y) -> (_evaluate x) / (_evaluate y)

    result

let eval (p1:Expr) =
    printfn "%.10A" p1

    let result = _evaluate (p1)
    printfn "%.10A" result
