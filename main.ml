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
  printval (eval_exp env (parse s))


(*
let make_genv_ _ =
let lexin = 
  let fn = ref [] in
  Arg.parse [] (fun s -> fn := s :: !fn) "";
  (Lexing.from_channel (open_in (List.hd !fn)))
in
let a = Parser.definitions Lexer.main lexin in
  make_define_env a
*)


let withfile name proc =
  let f = open_in name in
  let a = proc f in
  close_in f;  a


let make_defs f =
  Parser.definitions Lexer.main (Lexing.from_channel f)


let main name =
  let env = make_define_env (withfile name make_defs) in
let run () =
  eval_program
     env
    (Parser.toplevel Lexer.main (Lexing.from_channel stdin))
in
let rec read_eval_print () =
  print_string "=> ";
  flush stdout;
  (
    try
      print_string (printval (run ()));
    with
      Core.UnboundVar s ->   Printf.printf "unbound: %s" s
    | Parsing.Parse_error -> print_string "parse error"
    | Failure s ->           Printf.printf "failure: %s" s
  );
  print_newline ();
  read_eval_print ()
in
read_eval_print ()


(*
let _ =
  let fn = ref [] in
  Arg.parse [] (fun s -> fn := s :: !fn) "";
  main (List.hd !fn)
 *)
