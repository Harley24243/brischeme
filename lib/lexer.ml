open Ast

(** [token] is an enumeration of all possible tokens produced by the lexer *)
type token =
  | TkIdent of string
  | TkNum of int
  | TkBool of bool
  | TkLParen
  | TkRParen
  | TkDefine
  | TkLambda
  | TkPrimOp of primop
  | TkEnd

(* Lexer internal state *)

let idx = ref 0
let input = ref ""
let input_len = ref 0

(* Simple API for lexer internal state *)

let is_more () : bool = !idx < !input_len

let peek () : char =
  !input.[!idx]

let drop () =
  idx := !idx + 1

let raise_lex_error exp =
  let c = peek () in
  let msg = Printf.sprintf "LEX ERROR: Expected %s but found %c." exp c in
  failwith msg

(* Recognising character classes *)

let is_digit (c:char) : bool =
  match c with
  | '0' .. '9' -> true
  | _          -> false

let is_lower (c:char) : bool =
  match c with
  | 'a' .. 'z' -> true
  | _          -> false

let is_upper (c:char) : bool = 
  match c with
  | 'A' .. 'Z' -> true
  | _          -> false

let is_wspace (c:char) : bool =
  match c with
  | ' ' | '\n' | '\r' -> true
  | _                 -> false

(* Lexing functions *)

let lex_bool () : token =
  (* Assumes [peek () = '#'] *)
  drop (); 
  match peek () with
  | 't' -> 
      drop (); 
      TkBool true;
  | 'f' -> 
      drop ();
      TkBool false;
  | _ -> 
      raise_lex_error "t or f"

let lex_number () : token  =
  let lexeme = ref "" in
  while is_more () && is_digit (peek ()) do
    let c = peek () in
      drop ();
      lexeme := !lexeme ^ String.make 1 c
  done;
  TkNum (int_of_string !lexeme)

let lex_kw_or_id () : token =
  let lexeme = ref "" in
  let is_id_char c = 
        is_lower c || is_upper c 
  in
  while is_more () && is_id_char (peek ()) do
    let c = peek () in
    drop ();
    lexeme := !lexeme ^ String.make 1 c
  done;
  (* Check if the lexeme is a keyword, 
     otherwise it's an identifier. *)
  match !lexeme with
  | "define" -> TkDefine
  | "if"     -> TkPrimOp If
  | "not"    -> TkPrimOp Not
  | "and"    -> TkPrimOp And
  | "or"     -> TkPrimOp Or
  | "lambda" -> TkLambda
  | _        -> TkIdent !lexeme
  
let lex_init () =
  match peek () with
  | '=' -> 
    drop (); 
    TkPrimOp Eq;
  | '<' ->
    drop ();
    TkPrimOp Less;
  | '+' ->
    drop ();
    TkPrimOp Plus;
  | '-' ->
    drop ();
    TkPrimOp Minus;
  | '*' ->
    drop ();
    TkPrimOp Times
  | '/' ->
    drop ();
    TkPrimOp Divide
  | '(' ->
    drop ();
    TkLParen
  | ')' ->
    drop ();
    TkRParen;
  | '#' -> lex_bool ()
  | c when is_digit c -> lex_number ()
  | c when is_lower c -> lex_kw_or_id ()
  | _ -> raise_lex_error "valid character"

(* API of the lexer *)

(** 
    [lex s] returns the token list obtained by scanning [s].
    @raises [Failure] if [s] fails to scan.
*)
let lex (s:string) : token list =
  input := s;
  input_len := String.length s;
  idx := 0;
  let output = ref [] in
  while is_more () do 
    if is_wspace (peek ()) then
      drop ()
    else
      let tk = lex_init () in
      output := tk :: !output
  done;
  List.rev (TkEnd :: !output)

(* Conversion of token to a string, for debugging purposes. *)

(** [string_of_token tk] returns the string representation of [tk]. 
    Note: this is not necessarily the lexeme from which [tk] was obtained. *)
let string_of_token tk =
  match tk with
  | TkIdent s -> s
  | TkNum n -> string_of_int n
  | TkBool true -> "#t"
  | TkBool false -> "#f"
  | TkLParen -> "("
  | TkRParen -> ")"
  | TkDefine -> "define"
  | TkLambda -> "lambda"
  | TkPrimOp p -> string_of_primop p
  | TkEnd -> "EOF"