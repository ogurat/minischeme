(*
http://www.sato.kuis.kyoto-u.ac.jp/~igarashi/class/isle4-05w/text/

ocaml syntax.cmo lexer.cmo core.cmo parser.cmo main.cmo
*)
open Core
open Parser
open Lexing



let parseTop =
  Parser.toplevel Lexer.main

let parse s =
  let Syntax.Prog p = parseTop (Lexing.from_string s) in p


let withfile proc name =
  let f = open_in name in
  try
    let a = proc f in
    close_in f; a
  with Failure msg -> close_in f; raise (Failure msg)


let make_defs f =
  Parser.definitions Lexer.main (Lexing.from_channel f)

let eval s =
  let env = make_define_env (withfile make_defs "../test.scm") in
  printval (eval_exp env (parse s))

let show_exp  =
  (withfile make_defs)

let def_exp name =
   List.assoc name

let main name =
  let env = make_define_env (withfile make_defs name) in
  let rec repl () =
    print_string "=> ";
    flush stdout;
    (
      try
	let exp = (parseTop (Lexing.from_channel stdin)) in
	print_string (printval (eval_program env exp));
      with
	Core.UnboundVar s ->   Printf.printf "unbound: %s" s
      | Parsing.Parse_error -> print_string "parse error"
      | Failure s ->           Printf.printf "failure: %s" s
    );
    print_newline ();
    repl ()
  in
  repl ()


let _ =
  let fn = ref [] in
  Arg.parse [] (fun s -> fn := s :: !fn) "";
  if List.length !fn > 0 then
    main (List.hd !fn)

