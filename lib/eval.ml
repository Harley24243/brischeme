
open Ast

exception EvalFailure of string

type sval =
  | VNum of int
  | VBool of bool
  | VClo of var * sexp * env

and env = (var * sval) list

let rec eval_sexp (e:env) (s:sexp) : sval =
  match s with
  | Bool b -> VBool b
  | Num n -> VNum n
  | Ident x -> 
      (match List.assoc_opt x e with
      | None -> raise (EvalFailure "Identifier not in scope.")
      | Some v -> v)
  | Lambda (x, t) -> VClo (x, t, e)
  | Call (If, [s1; s2; s3]) ->
      (match eval_sexp e s1 with
      | VBool b -> 
          if b then eval_sexp e s2 else eval_sexp e s3
      | _ -> raise (EvalFailure "Guard of if-expression must be a boolean."))
  | Call (If, _) -> raise (EvalFailure "If must have exactly three arguments.")
  | Call (p, ss) -> 
      let vs = List.map (eval_sexp e) ss in
        (match p, vs with
        | Plus, [VNum n1; VNum n2] -> VNum (n1 + n2)
        | Minus, [VNum n1; VNum n2] -> VNum (n1 - n2)
        | Times, [VNum n1; VNum n2] -> VNum (n1 * n2)
        | Divide, [VNum n1; VNum n2] -> VNum (n1 / n2)
        | And, [VBool b1; VBool b2] -> VBool (b1 && b2)
        | Or, [VBool b1; VBool b2] -> VBool (b1 || b2)
        | Eq, [v1; v2] -> VBool (v1 = v2)
        | Less, [VNum n1; VNum n2] -> VBool (n1 < n2)
        | _ -> raise (EvalFailure "Primitive operator without correct arguments."))
  | App (s1, [s2]) ->
        (match eval_sexp e s1 with
        | VClo (x, bdy, e') -> 
            let v2 = eval_sexp e s2 in
              let e'' = (x, v2) :: e' @ e in
                eval_sexp e'' bdy
        | _ -> raise (EvalFailure "Operator of application must be a lambda."))
  | _ -> raise (EvalFailure (string_of_sexp s))

let eval_form (e:env) (f:form) : sval * env =
  match f with
  | Define (x, s) -> 
      let v = eval_sexp e s in 
        (v, (x, v) :: e)
  | Expr s ->
      (eval_sexp e s, e)

let rec eval_prog (e:env) (p:prog) : sval list * env =
  match p with
  | [] -> ([], e)
  | f::fs -> 
      let v, e' = eval_form e f in 
        let vs, e'' = eval_prog e' fs in
          (v::vs, e'')
