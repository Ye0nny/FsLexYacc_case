module Ast

open System.Collections.Generic

type Var = string

type Expr =
    | Num of float
    | Var of Var
    | Plus of Expr * Expr
    | Minus of Expr * Expr
    | Mul of Expr * Expr
    | Div of Expr * Expr




type Env<'T1, 'T2> when 'T1:equality () =
    let emptyDict = new Dictionary<'T1, 'T2>()

    member x.valueTable = emptyDict

    member x.add(k:'T1, v:'T2) =
        if x.valueTable.ContainsKey(k) then
            x.valueTable.[k] <- v
        else
            x.valueTable.Add(k, v)

    member x.del(k:'T1) =
        x.valueTable.Remove(k)

    member x.lookup(k:'T1) =
        match x.valueTable.TryGetValue(k) with
        | true, v -> Some(v)
        | false, _ -> None

    member x.toList():('T1*'T2) list =
        let result = 
            x.valueTable 
            |> Seq.map (|KeyValue|)
            |> Seq.toList
        result

module Env =
    let ofList (li:('T1*'T2) list) =
        let newEnv = Env<'T1,'T2>()
        li |> Seq.iter (fun (k,v) -> newEnv.valueTable.Add(k, v)) 
        newEnv



let env = Env<string, Expr>()

let rec _exprEval (p1:Expr) =
    let result =
        match p1 with
        | Num(x) -> x
        | Var(x) ->
            match env.lookup(x) with
            | Some(v) -> _exprEval(v)
            | None -> failwith ("[Error] exprEval| " + x + " is not found.")
        | Plus(Num(x), Num(y)) -> x + y
        | Minus(Num(x), Num(y)) -> x - y
        | Mul(Num(x), Num(y)) -> x * y
        | Div(Num(x), Num(y)) -> x / y
        | Plus(x, y) -> (_exprEval x) + (_exprEval y)
        | Minus(x, y) -> (_exprEval x) - (_exprEval y)
        | Mul(x, y) -> (_exprEval x) * (_exprEval y)
        | Div(x, y) -> (_exprEval x) / (_exprEval y)
        //| _ -> failwith "[Error] exprEval| Failed. "

    result

let exprEval (p1:Expr) =
    let result = _exprEval (p1)
    result

