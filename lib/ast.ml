open Lexer

type var = string

type form =
  | Define of var * sexp
  | Expr of sexp

and sexp =
  | Bool of bool
  | Num of int
  | Ident of string
  | Lambda of var * sexp
  | Call of primop * sexp list
  | App of sexp * sexp list

type prog = form list

let print_primop (p:primop) : string =
  match p with
  | Plus   -> "+"
  | Minus  -> "-" 
  | Times  -> "*" 
  | Divide -> "/"
  | And    -> "and"
  | Or     -> "or"    
  | Not    -> "not"
  | Less   -> "less"
  | Eq     -> "eq"
  | If     -> "if"

let rec print_form (f:form) : string = 
  match f with
  | Define (v, e) -> Printf.sprintf "Define(%s, %s)" v (print_sexp e)
  | Expr e -> print_sexp e

and print_sexp (e:sexp) =
  match e with
  | Bool b -> if b then "Bool(True)" else "Bool(False)"
  | Num n -> Printf.sprintf "Num(%d)" n
  | Ident s -> Printf.sprintf "Ident(%s)" s
  | Lambda (v, e) -> Printf.sprintf "Lambda(%s, %s)" v (print_sexp e)
  | Call (p, es) -> Printf.sprintf "Call(%s, %s)" (print_primop p) (print_sexp_list es)
  | App (e, es) -> Printf.sprintf "App(%s, %s)" (print_sexp e) (print_sexp_list es)

and print_sexp_list (es:sexp list) =
  "[" ^ (match es with
  | [] -> "]"
  | [e] -> Printf.sprintf "%s]" (print_sexp e)
  | e::es -> Printf.sprintf "%s, %s" (print_sexp e) (print_sexp_list es))

let rec print_prog (p:prog) =
  match p with
  | [] -> ""
  | [f] -> print_form f
  | f::fs -> Printf.sprintf "%s\n%s" (print_form f) (print_prog fs)
