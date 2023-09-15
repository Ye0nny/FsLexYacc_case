module Ast

type Var = string

type Op =
    | Plus
    | Minus
    | Mul
    | Div
    | Lt
    | Le
    | Gt
    | Ge
    | Eq
    | Ne
    | And
    | Or

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
    | If of Expr * Stmt * Stmt option
    | BlockStmt of Stmt list

