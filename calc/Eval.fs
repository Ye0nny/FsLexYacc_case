module Eval

open System.Collections.Generic
open Ast

type Env<'T1, 'T2> when 'T1:equality () =
    let emptyDict = new Dictionary<'T1, 'T2>()

    member x.valueTable = emptyDict

    member x.add(k:'T1, v:'T2) =
        if x.valueTable.ContainsKey(k) then
            x.valueTable.[k] <- v
        else
            x.valueTable.Add(k, v)

    member x.del(k:'T1) =
        x.valueTable.Remove(k) |> ignore

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
        li |> Seq.iter (fun (k,v) -> newEnv.add(k, v)) 
        newEnv


module Eval =
    let env = Env<string, Expr>()

    let rec _exprEval (p1:Expr) =
        let result =
            match p1 with
            | Num(x) -> x
            | Var(x) ->
                match env.lookup(x) with
                | Some(v) -> _exprEval(v)
                | None -> failwith ("[Error] exprEval| " + x + " is not found.")
            | Binary(Op.Plus, Num(x), Num(y)) -> x + y
            | Binary(Op.Minus, Num(x), Num(y)) -> x - y
            | Binary(Op.Mul, Num(x), Num(y)) -> x * y
            | Binary(Op.Div, Num(x), Num(y)) -> x / y
            | Binary(Op.Plus, x, y) -> (_exprEval x) + (_exprEval y)
            | Binary(Op.Minus ,x, y) -> (_exprEval x) - (_exprEval y)
            | Binary(Op.Mul, x, y) -> (_exprEval x) * (_exprEval y)
            | Binary(Op.Div, x, y) -> (_exprEval x) / (_exprEval y)
            //| _ -> failwith "[Error] exprEval| Failed. "
        result

    let exprEval (p1:Expr) =
        let result = _exprEval (p1)
        result

    let stmtEval (stmt:Stmt) =
        match stmt with
        | Assign (var, expr) -> env.add(var, expr)
        | Print expr -> printfn "%A" (expr |> exprEval)
        | ExprStmt expr -> expr |> exprEval |> ignore

