module Ast

type Var = string

type Expr =
    | Num of float
    | Var of Var
    | Plus of Expr * Expr
    | Minus of Expr * Expr
    | Mul of Expr * Expr
    | Div of Expr * Expr

type Stmt =
    | Assign of Var * Expr


let rec _exprEval (p1:Expr) =
    let result =
        match p1 with
        | Num(x) -> x
        | Plus(Num(x), Num(y)) -> x + y
        | Minus(Num(x), Num(y)) -> x - y
        | Mul(Num(x), Num(y)) -> x * y
        | Div(Num(x), Num(y)) -> x / y
        | Plus(x, y) -> (_exprEval x) + (_exprEval y)
        | Minus(x, y) -> (_exprEval x) - (_exprEval y)
        | Mul(x, y) -> (_exprEval x) * (_exprEval y)
        | Div(x, y) -> (_exprEval x) / (_exprEval y)
        | _ -> failwith "[Error] exprEval| Failed. "

    result

let exprEval (p1:Expr) =
    let result = _exprEval (p1)
    result

let stmtEval (p1:Stmt) =
    let r1, r2 =
        match p1 with
        | Assign(x, y) -> x, _exprEval(y)

    printfn "%A = %A" r1 r2
