module Ast

type Var = string

type Op =
    | Plus
    | Minus
    | Mul
    | Div

type Expr =
    | Int of int
    | Float of float
    | Bool of bool
    | Var of Var
    | Binary of Op * Expr * Expr

type Stmt =
    | Assign of Var * Expr
    | Print of Expr
    | ExprStmt of Expr


