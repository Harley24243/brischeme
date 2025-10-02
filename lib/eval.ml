open Ast

(** 
    An [store] is just a list of pairs [(x, v)] with [x] a variable and [v] an
    expression.  We require that [v] is moreover a value, but this is not
    enforced by the types. 
*)
type store = (string * sexp) list

(** [is_value s] is true just if [s] is a value *)
let is_value (s:sexp) : bool =
  match s with
  | Num _ | Bool _ | Lambda _ -> true
  | _                         -> false


let rec sum_num_values (vs : sexp list) : int =
  match vs with
  | [] -> 0
  | (Num n) :: ws -> n + sum_num_values ws
  | _ -> failwith "Sexp in the list is not a number value."

(**
    [subst [(x_1, v_1); ...; (x_n, v_n)] t] returns the expression obtained 
    from [t] after substituting every occurrence of [x_i] by [v_i].  

    NOTE: substitution may incur variable capture!  Strange behaviour may result.
    Take COMS30040: Types and Lambda Calculus in year 3 to make sense of this.
*)
let rec subst (sub: (string * sexp) list) (t:sexp) : sexp =
  match t with
  | Ident y when List.mem_assoc y sub -> List.assoc y sub
  | Ident _ -> t
  | Call (p, args) -> Call (p, List.map (subst sub) args)
  | App (s, ss) -> App (subst sub s, List.map (subst sub) ss)
  | Lambda (ys, s) -> Lambda (ys, subst sub s)
  | _ -> t (* This is bad style, but it will make the exercises easier *)

(** [raise_eval_error s] raises [Failure] with a runtime error message *)
let raise_eval_error s =
  let str = string_of_sexp s in
  let msg = "RUNTIME ERROR: Evaluation undefined for " ^ str ^ "." in
  failwith msg
    

(** 
    [step_sexp e s] evaluates a step of expression [s] in store [e] according 
    to the operational semantics.

    @raises [Failure] if [s] cannot make a step in store [e].
*)
let rec step_sexp (e:store) (s:sexp) : sexp =
  match s with

  (* Top-level identifiers *)
  | Ident x when List.mem_assoc x e -> List.assoc x e
  
  (* If is a rather special primitive operator, because we don't evaluate 
     its arguments -- i.e. the branches -- until we know which one is taken *)
  | Call (If, [Bool true; s2; _]) -> s2
  | Call (If, [Bool false; _; s3]) -> s3
  | Call (If, [s1; s2; s3]) when not (is_value s1) -> 
      let s1' = step_sexp e s1 in
      Call (If, [s1'; s2; s3])
  
  (* Primitive operators *)
  | Call (p, ss) when not (List.for_all is_value ss) -> 
      let ss' = step_sexp_list e ss in
      Call (p, ss')
  | Call (Not, [Bool b]) -> Bool (not b)
  | Call (Plus, vs) -> Num (sum_num_values vs)
  | Call (Minus, [Num n1; Num n2]) -> Num (n1 - n2)
  | Call (Times, [Num n1; Num n2]) -> Num (n1 * n2)
  | Call (Divide, [Num n1; Num n2]) -> Num (n1 / n2)
  | Call (And, [Bool b1; Bool b2]) -> Bool (b1 && b2)
  | Call (Or, [Bool b1; Bool b2]) -> Bool (b1 || b2)
  | Call (Less, [Num n1; Num n2]) -> Bool (n1 < n2)
  | Call (Eq, [v1; v2]) -> Bool (v1 = v2)

  (* Application of user defined functions *)
  | App (s, ss) when not (is_value s) ->
      let s' = step_sexp e s in
      App (s', ss)
  | App (Lambda (xs, t), ss) when not (List.for_all is_value ss) ->
      let ss' = step_sexp_list e ss in
      App (Lambda (xs, t), ss')
  | App (Lambda (xs, t), vs) -> 
      let subst_pairs = List.combine xs vs in
      subst subst_pairs t

  (* Runtime exception *)
  | _ -> raise_eval_error s

(**
  [step_sexp_list e ss] evaluates, from left to right, a step of a 
  list of expressions, i.e. the result is a new list of expressions [ss']:
    - if [ss] contains no expression that is not a value, then [ss' = ss].
    - otherwise [ss'] is obtained by making a step in the leftmost 
      non-value expression in [ss], all others remain fixed.
*)
and step_sexp_list (e:store) (ss:sexp list) : sexp list =
  match ss with
  | [] -> []
  | s :: tt when not (is_value s) -> step_sexp e s :: tt
  | v :: tt -> v :: step_sexp_list e tt

(** 
    [step_form e f] makes a step of form [f] with respect to store [e]. 
    @raises [Failure] if evaluation gets stuck.
*)
let step_form (e:store) (f:form) : form =
    match f with
    | Expr s when not (is_value s) -> 
        let s' = step_sexp e s in
        Expr s'
    | Define (x, s) when not (is_value s) ->
        let s' = step_sexp e s in
        Define (x, s')
    | _ -> failwith "Impossible: step_form of a value."
