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

let eat_lit () : sexp =
  match peek () with
  | TkLit LNil -> drop (); Nil
  | TkLit (LBool b) -> drop (); Bool b
  | TkLit (LNum n)  -> drop (); Num n
  | _ -> raise_parse_error "literal"

let eat_ident () : string =
  match peek () with 
  | TkIdent s -> drop (); s
  | _         -> raise_parse_error "identifier"

let eat_primop () : primop =
  match peek () with
  | TkPrimOp p -> drop (); p
  | _          -> raise_parse_error "primop"

(* Parsing functions corresponding to each nonterminal. *)

let rec prog () : prog = 
  match peek () with
  | TkEnd -> []
  | TkLParen | TkLit _ | TkIdent _ -> 
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
  | TkIdent _ | TkLit _ -> Expr (atom ())
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
  | TkLit _ -> eat_lit ()
  | TkIdent _ -> 
      let s = eat_ident () in
      Ident s
  | _ -> raise_parse_error "atom"

and ident_list () : var list =
  match peek () with
  | TkIdent _ -> 
      let s = eat_ident () in
      let ts = ident_list () in
      s :: ts
  | TkRParen -> []
  | _ -> raise_parse_error "ident list"

and sexpr () : sexp =
  match peek () with
  | TkLParen -> 
      eat TkLParen;
      let e = expr () in
      eat TkRParen; e
  | TkIdent _ | TkLit _ -> atom ()
  | _ -> raise_parse_error "s-expression"

and sexpr_list () : sexp list =
  match peek () with
  | TkLParen | TkIdent _ | TkLit _ -> 
      let e = sexpr () in
      let es = sexpr_list () in
      e :: es
  | TkRParen -> []
  | _ -> raise_parse_error "s-expression list"

and expr () : sexp =
  match peek () with
  | TkLParen | TkIdent _ | TkLit _ -> 
      let s = sexpr () in
      let ss = sexpr_list () in
      App (s, ss)
  | TkLambda ->
      eat TkLambda;
      eat TkLParen;
      let ss = ident_list () in
      eat TkRParen;
      let e = sexpr () in
      Lambda (ss, e)
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