open Lexer
open Ast

exception ParseFailure

(* Parser state *)
let tokens : (token list) ref = ref []

let peek () = List.hd !tokens

let eat (tk:token) = 
  if peek () = tk then 
    tokens := List.tl !tokens  
  else 
    raise ParseFailure

let eat_ident () : string =
  match peek () with 
  | TkIdent s -> eat (peek ()); s
  | _         -> raise ParseFailure

let eat_num () : int =
  match peek () with
  | TkNum n -> eat (peek ()); n
  | _       -> raise ParseFailure

let eat_bool () : bool =
  match peek () with
  | TkBool b -> eat (peek ()); b
  | _        -> raise ParseFailure

let eat_primop () : primop =
  match peek () with
  | TkPrimOp p -> eat (peek ()); p
  | _          -> raise ParseFailure

let init (tks: token list) =
  tokens := tks

(* Parsing Functions *)

let rec prog () : prog = 
  match peek () with
  | TkEnd -> []
  | TkLParen | TkIdent _ | TkNum _ | TkBool _ -> 
      let frm = form () in
        let prg = prog () in 
          frm :: prg
  | _ -> raise ParseFailure

and form () : form =
  match peek () with
  | TkLParen -> 
      eat TkLParen; 
      let f = exprOrDefn () in 
        eat TkRParen; 
        f
  | TkIdent _ | TkNum _ | TkBool _ -> Expr (atom ())
  | _ -> raise ParseFailure

and exprOrDefn () : form =
  match peek () with
  | TkDefine -> 
      eat TkDefine;
      (match peek () with
      | TkIdent s -> 
          eat (TkIdent s);
          let e = sexpr () in
            Define (s, e)
      | _ -> raise ParseFailure)
  | TkLParen | TkIdent _ | TkLambda | TkPrimOp _ -> Expr (expr ())
  | _ -> raise ParseFailure

and atom () : sexp =
  match peek () with
  | TkIdent s -> eat (TkIdent s); Ident s
  | TkNum n -> eat (TkNum n); Num n
  | TkBool b -> eat (TkBool b); Bool b
  | _ -> raise ParseFailure

and sexpr () : sexp =
  match peek () with
  | TkLParen -> 
      eat TkLParen;
        let e = expr () in
          eat TkRParen; e
  | TkIdent _ | TkNum _ | TkBool _ -> atom ()
  | _ -> raise ParseFailure

and sexpr_list () : sexp list =
  match peek () with
  | TkLParen | TkIdent _ | TkNum _ | TkBool _ -> 
    let e = sexpr () in
      let es = sexpr_list () in
        e :: es
  | TkRParen -> []
  | _ -> raise ParseFailure

and expr () : sexp =
  match peek () with
  | TkLParen | TkIdent _ | TkNum _ | TkBool _ -> 
      let e = sexpr () in
        let es = sexpr_list () in
          App (e, es)
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
  | _ -> raise ParseFailure

  let parse (s:string) : prog =
    tokens := lex s;
    prog ()