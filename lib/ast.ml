
(** [var] is a type synonym for [string] *)
type var = string

(** [primop] is an enumeration of the available primitive operations. *)
type primop =
  | Plus
  | Minus
  | Times
  | Divide
  | Eq
  | Less
  | If
  | And
  | Or
  | Not

(** A [form] is either a top-level definition or an expression to be evaluated .*)
type form =
  | Define of var * sexp
  | Expr of sexp

(** A [sexp] is an expression to be evaluated. *)
and sexp =
  | Bool of bool
  | Num of int
  | Ident of string
  | Lambda of var * sexp
  | Call of primop * sexp list
  | App of sexp * sexp

(** A [prog] is just a list of [form]. *)
type prog = form list


(* 
    String representations of ASTs. 

    Each of the AST types above has a corresponding function to convert it
    to a string.  Useful for printing out the AST for debugging purposes. 
*)

(** [string_of_primop p] is the string representation of a primop [p]. *)
let string_of_primop (p:primop) : string =
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

(** [string_of_form f] is the string representation of a form [f]. *)
let rec string_of_form (f:form) : string = 
  match f with
  | Define (v, e) -> Printf.sprintf "(define %s %s)" v (string_of_sexp e)
  | Expr e -> string_of_sexp e

(** [string_of_sexp e] is the string representation of expression [e]. *)
and string_of_sexp (e:sexp) : string =
  match e with
  | Bool b -> if b then "#t" else "#f"
  | Num n -> Printf.sprintf "%d" n
  | Ident s -> Printf.sprintf "%s" s
  | Lambda (v, e) -> Printf.sprintf "(lambda (%s) %s)" v (string_of_sexp e)
  | Call (p, es) -> Printf.sprintf "(%s %s)" (string_of_primop p) (string_of_sexp_list es)
  | App (e1, e2) -> Printf.sprintf "(%s %s)" (string_of_sexp e1) (string_of_sexp e2)

(** 
    [string_of_sexp_list es] is the string representation of 
    expression list [es]. 
*)
and string_of_sexp_list (es:sexp list) : string =
  match es with
  | [] -> ""
  | e::es -> Printf.sprintf "%s %s" (string_of_sexp e) (string_of_sexp_list es)

(** 
    [string_of_prog p] is the string representation of program [p]. It is essentially
    the same as [string_of_sexp_list p] except that consecutive [sexp] are separated 
    by newlines instead of spaces.
*)
let rec string_of_prog (p:prog) : string =
  match p with
  | [] -> ""
  | [f] -> string_of_form f
  | f::fs -> Printf.sprintf "%s\n%s" (string_of_form f) (string_of_prog fs)
