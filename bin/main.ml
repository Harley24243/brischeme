open Scheme.Parser
open Scheme.Eval

(** [string_of_sval v] returns a string representation of [v], where  
    [v] is typically a value computed by the semantics *)
let string_of_sval (v:sval) =
  match v with
  | VNum n -> string_of_int n
  | VBool b -> if b then "#t" else "#f"
  | VClo _ -> "<function>"

(** [repl e] implements a read-eval-print-loop (REPL), looping until 
    the end-of-file signal is given by e.g. the user pressing ctrl-d.  
    The evaluation part of the loop is with respect to the initial 
    environment [e]. *)
let rec repl (e:env) =
  try 
      print_string "> ";
      let inp = read_line () in
        let ast = parse inp in
          (* The program [ast] is essentially a list of "forms", which are
             definitions or expressions to be evaluated.  Here [vs] is the 
             list of values obtained from evaluating each of the expressions
             in turn and [e'] is the updated environment that takes into account 
             the new definitions. *)   
          let vs, e' = eval_prog e ast in
            List.iter (fun v -> print_endline (string_of_sval v)) vs;
            repl e'
  with
  | End_of_file -> print_endline "\nExiting"

(** This is effectively the entry point of the program. *)
let () = 
  print_endline "Brischeme";
  repl []
  

  
