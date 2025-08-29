open Lexer
open Ast

(* Parser internal state *)

let tokens : (token list) ref = ref []

(* Simple API for parser internal state *)

let peek () = List.hd !tokens

let drop ()  = 
  tokens := List.tl !tokens

let init (tks: token list) =
  tokens := tks

let raise_parse_error exp =
  let act = string_of_token (peek ()) in
  let msg = Printf.sprintf "PARSE ERROR: Expected %s but got %s." exp act in
  failwith msg

(* Consuming tokens of a specified shape. *)

let eat (tk:token) = 
  if peek () = tk then 
    drop ()
  else 
    raise_parse_error (string_of_token tk)

let eat_ident () : string =
  match peek () with 
  | TkIdent s -> drop (); s
  | _         -> raise_parse_error "identifier"

let eat_num () : int =
  match peek () with
  | TkNum n -> drop (); n
  | _       -> raise_parse_error "number"

let eat_bool () : bool =
  match peek () with
  | TkBool b -> drop (); b
  | _       -> raise_parse_error "boolean"

let eat_primop () : primop =
  match peek () with
  | TkPrimOp p -> drop (); p
  | _          -> raise_parse_error "primop"

(* Parsing functions corresponding to each nonterminal. *)

let rec prog () : prog = 
  match peek () with
  | TkEnd -> []
  | TkLParen | TkIdent _ | TkNum _ | TkBool _ -> 
      let frm = form () in
      let prg = prog () in 
      frm :: prg
  | _ -> raise_parse_error "program"

and form () : form =
  match peek () with
  | TkLParen -> 
      eat TkLParen; 
      let f = exprOrDefn () in 
      eat TkRParen; 
      f
  | TkIdent _ | TkNum _ | TkBool _ -> Expr (atom ())
  | _ -> raise_parse_error "form"

and exprOrDefn () : form =
  match peek () with
  | TkDefine -> 
      eat TkDefine;
      let s = eat_ident () in
      let e = sexpr () in
      Define (s, e)
  | TkLParen | TkIdent _ | TkLambda | TkPrimOp _ -> Expr (expr ())
  | _ -> raise_parse_error "expression or definition"

and atom () : sexp =
  match peek () with
  | TkIdent _ -> 
      let s = eat_ident () in
      Ident s
  | TkNum _ -> 
      let n = eat_num () in 
      Num n
  | TkBool _ -> 
      let b = eat_bool () in
      Bool b
  | _ -> raise_parse_error "atom"

and sexpr () : sexp =
  match peek () with
  | TkLParen -> 
      eat TkLParen;
      let e = expr () in
      eat TkRParen; e
  | TkIdent _ | TkNum _ | TkBool _ -> atom ()
  | _ -> raise_parse_error "s-expression"

and sexpr_list () : sexp list =
  match peek () with
  | TkLParen | TkIdent _ | TkNum _ | TkBool _ -> 
      let e = sexpr () in
      let es = sexpr_list () in
      e :: es
  | TkRParen -> []
  | _ -> raise_parse_error "s-expression list"

and expr () : sexp =
  match peek () with
  | TkLParen | TkIdent _ | TkNum _ | TkBool _ -> 
      let e1 = sexpr () in
      let e2 = sexpr () in
      App (e1, e2)
  | TkLambda ->
      eat TkLambda;
      eat TkLParen;
      let s = eat_ident () in
      eat TkRParen;
      let e = sexpr () in
      Lambda (s, e)
  | TkPrimOp _ ->
      let p = eat_primop () in
      let es = sexpr_list () in
      Call (p, es)
  | _ -> raise_parse_error "expression"

(* API of the parser *)

(** [parse_prog s] returns the AST for the program written in string [s] *)
let parse_prog (s:string) : prog =
  tokens := lex s;
  prog ()