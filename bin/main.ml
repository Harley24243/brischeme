open Brischeme.Ast
open Brischeme.Parser
open Brischeme.Eval

(** 
    [ep_form e f] evaluates the form [f] in store [e], returning
    an updated store.  Additionally, if [f] is an expression then 
    the value of the expression is printed to the console as a side effect. 
*)
let rec ep_form (e:store) (f:form) : store =
  match f with
  | Expr v when is_value v -> 
      print_endline (string_of_sexp v); 
      e
  | Define (x, v) when is_value v -> 
      (x, v) :: (List.remove_assoc x e)
  | _ -> ep_form e (step_form e f)

(** 
    [repl e] implements a read-eval-print-loop (REPL), looping until 
    the end-of-file signal is given by e.g. the user pressing ctrl-d.  
    The evaluation part of the loop is with respect to the initial 
    store [e]. 
*)
let rec repl (e:store) =
  try 
    print_string "> ";
    let inp = read_line () in
    let forms = parse_prog inp in
    (* evaluate and print each form in turn, returning the final store [e'] *)
    let e' = List.fold_left ep_form e forms in
    repl e'
  with
  | End_of_file ->
      print_newline ()
  | Failure msg -> 
      print_endline msg;
      repl e

(* This is effectively the entry point of the program. *)
let () = 
  print_endline "Brischeme";
  repl []
  

  
