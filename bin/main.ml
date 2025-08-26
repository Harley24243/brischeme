open Scheme.Parser
open Scheme.Eval

let string_of_sval v =
  match v with
  | VNum n -> string_of_int n
  | VBool b -> if b then "#t" else "#f"
  | VClo _ -> "<function>"

let rec repl (e:env) =
  try 
      print_string "> ";
      let inp = read_line () in
      (* print ast *)
        let ast = parse inp in
          let vs, e' = eval_prog e ast in
            List.iter (fun v -> print_endline (string_of_sval v)) vs;
            repl e'
  with
  | End_of_file -> print_endline "\nExiting"

let () = 
  print_endline "Brischeme";
  repl []
  

  
