module Ast

type Expr =
    | Num of float
    | Var of string
    | Plus of Expr * Expr
    | Minus of Expr * Expr
    | Mul of Expr * Expr
    | Div of Expr * Expr

type Stmt =
    | Assign of Expr * Expr


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
    printfn "%.10A" p1

    let result = _exprEval (p1)
    printfn "%.10A" result

let stmtEval (p1:Stmt) =
    printfn "%A" p1

    let r1, r2 =
        match p1 with
        | Assign(x, y) -> x, _exprEval(y)

    printfn "%A = %A" r1 r2
