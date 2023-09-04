module Ast

type Expr =
    | Num of float
    | Plus of Expr * Expr
    | Minus of Expr * Expr
    | Mul of Expr * Expr
    | Div of Expr * Expr
    | Mod of Expr * Expr

