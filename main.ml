(*
http://www.sato.kuis.kyoto-u.ac.jp/~igarashi/class/isle4-05w/text/

ocaml syntax.cmo lexer.cmo core.cmo parser.cmo main.cmo
*)
open Core
open Parser
open Lexing



let parse s =
  let Syntax.Prog p = (Parser.toplevel Lexer.main (Lexing.from_string s)) in p

let eval s =
  let env = make_define_env [] in
  let ex = (Parser.toplevel Lexer.main (Lexing.from_string s)) in
  printval (eval_program env ex)



let make_genv _ =
let lexin = 
  let fn = ref [] in
  Arg.parse [] (fun s -> fn := s :: !fn) "";
  (Lexing.from_channel (open_in (List.hd !fn)))
in
let a = Parser.definitions Lexer.main lexin in
  make_define_env a

let run () =
  Core.eval_program
     (make_genv ())
    (Parser.toplevel Lexer.main (Lexing.from_channel stdin))

let rec read_eval_print () =
  print_string "=> ";
  flush stdout;
  (
    try
      print_string (Core.printval (run ()));
    with
      Core.UnboundVar s ->   Printf.printf "unbound: %s" s
    | Parsing.Parse_error -> print_string "parse error"
    | Failure s ->           Printf.printf "failure: %s" s
  );
  print_newline ();
  read_eval_print ()

(*
let _ = read_eval_print ()
*)
