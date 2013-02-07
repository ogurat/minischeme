(*
http://www.sato.kuis.kyoto-u.ac.jp/~igarashi/class/isle4-05w/text/

ocaml syntax.cmo lexer.cmo core.cmo parser.cmo main.cmo
*)
open Core
open Parser
open Lexing



let lexin = 
  let fn = ref [] in
  Arg.parse [] (fun s -> fn := s :: !fn) "";
  (Lexing.from_channel (open_in (List.hd !fn)))
let a = Parser.definitions Lexer.main lexin
let global_env = make_define_env a

let run () =
  Core.eval_program
    global_env
    (Parser.toplevel Lexer.main (Lexing.from_channel stdin))

let rec read_eval_print () =
  print_string "=> ";
  flush stdout;
  (
    try
      Core.printval (run ());
    with
      Core.UnboundVar s ->   Printf.printf "unbound: %s" s
    | Parsing.Parse_error -> print_string "parse error"
    | Failure s ->           Printf.printf "failure: %s" s
  );
  print_newline ();
  read_eval_print ()

let _ = read_eval_print ()
