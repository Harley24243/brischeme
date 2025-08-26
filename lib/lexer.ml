
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

exception LexFailure

let idx = ref 0
let input = ref ""
let input_len = ref 0

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

  let is_more () : bool = !idx < !input_len

let peek () : char =
  !input.[!idx]

let eat (c: char) =
  if c == peek() then
    idx := !idx + 1
  else 
    raise LexFailure

let lex_number () : token =
  let lexeme = ref "" in
  while is_more () && is_digit (peek ()) do
    let c = peek () in
      eat c;
      lexeme := !lexeme ^ String.make 1 c
  done;
  TkNum (int_of_string !lexeme)

let lex_kw_or_id () : token =
  let lexeme = ref "" in
  let 
    is_id_char c = 
      is_lower c || is_upper c
  in 
    while is_more () && is_id_char (peek ()) do
      let c = peek () in
        eat c;
        lexeme := !lexeme ^ String.make 1 c
    done;
    match !lexeme with
    | "define" -> TkDefine
    | "if" -> TkPrimOp If
    | "not" -> TkPrimOp Not
    | "and" -> TkPrimOp And
    | "or" -> TkPrimOp Or
    | "lambda" -> TkLambda
    | _      -> TkIdent !lexeme

let lex (s:string) : token list =
  input := s;
  input_len := String.length s;
  idx := 0;
  let tokens = ref [] in
  while is_more () do
    match peek() with
    | '=' -> 
      eat '='; 
      tokens := TkPrimOp Eq :: !tokens
    | '<' ->
      eat '<';
      tokens := TkPrimOp Less :: !tokens
    | '+' ->
      eat '+';
      tokens := TkPrimOp Plus :: !tokens
    | '-' ->
      eat '-';
      tokens := TkPrimOp Minus :: !tokens
    | '*' ->
      eat '*';
      tokens := TkPrimOp Times :: !tokens
    | '/' ->
      eat '/';
      tokens := TkPrimOp Divide :: !tokens
    | '(' ->
      eat '(';
      tokens := TkLParen :: !tokens
    | ')' ->
      eat ')';
      tokens := TkRParen :: !tokens
    | '#' ->
      eat '#';
      (match peek() with
      | 't' -> tokens := TkBool true :: !tokens
      | 'f' -> tokens := TkBool false :: !tokens
      | _ -> raise LexFailure)
    | c when is_digit c -> tokens := lex_number () :: !tokens
    | c when is_lower c -> tokens := lex_kw_or_id () :: !tokens
    | c when is_wspace c -> eat c
    | _ -> raise LexFailure
  done;
  List.rev (TkEnd :: !tokens)
