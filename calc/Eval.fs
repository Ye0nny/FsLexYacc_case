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

let env = Env<string, Expr>()


let calcOp op x y =
    match op with
    | Op.Plus -> x + y
    | Op.Minus -> x - y
    | Op.Mul -> x * y
    | Op.Div -> x / y

let rec _exprEval (expr:Expr) =
    let result =
        match expr with
        | Int(x) -> x
        | Float(x) -> x |> int // truncate
        | Bool(v) -> if v then 1 else 0
        | Var(var) ->
            match env.lookup(var) with
            | Some(e) -> _exprEval(e)
            | None -> failwith ("[Error] exprEval| " + var + " is not found.")
        | Binary(op, Int(x), Int(y)) -> calcOp op x y
        | Binary(op, expr1, expr2) -> calcOp op (_exprEval expr1) (_exprEval expr2)
        //| _ -> failwith "[Error] exprEval| Failed. "
    result

module Eval =
    let exprEval (expr:Expr) =
        let result = _exprEval expr
        result

    let stmtEval (stmt:Stmt) =
        match stmt with
        | Assign (var, expr) -> env.add(var, expr)
        | Print expr -> printfn "%A" (expr |> exprEval)
        | ExprStmt expr -> expr |> exprEval |> ignore

