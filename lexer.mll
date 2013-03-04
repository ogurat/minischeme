{
let reservedWords = [
  (* Keywords *)
(*  ("+", Parser.PLUS); ("-", Parser.MINUS); ("*", Parser.MUL); *)
(*  ("add1", Parser.ADD1); ("sub1", Parser.SUB1); *)
(*  ("=", Parser.EQ); ("<", Parser.LT); (">", Parser.GT); *)
  ("if", Parser.IF);
  ("and", Parser.AND); ("or", Parser.OR);
(*  ("not", Parser.NOT); *)
  ("define", Parser.DEFINE);
  ("let", Parser.LET);
  ("cond", Parser.COND);
  ("lambda", Parser.LAMBDA);
  ("letrec", Parser.LETREC);
  ("set", Parser.SET);
  ("begin", Parser.BEGIN);
(*  ("setcar!", Parser.SETCAR);  ("setcdr!", Parser.SETCDR); *)
  ("quote",  Parser.QUOTE);
  ("else", Parser.ELSE);
] 
}

rule main = parse
  (* ignore spacing and newline characters *)
  [' ' '\009' '\012' '\n']+     { main lexbuf }

(*| ';' [^ '\012' '\n']* ['\012' '\n']+ { main lexbuf } *)
| ';' { comment lexbuf }

| ['0'-'9']+
    { Parser.INTV (int_of_string (Lexing.lexeme lexbuf)) }

| "(" { Parser.LPAREN }
| ")" { Parser.RPAREN }
| "#t"  { Parser.SHARP true }
| "#f"  { Parser.SHARP false }
| "=>" { Parser.ARROW } 

| ['a'-'z'] ['a'-'z' '_' '0'-'9' '-' '!' '?' '\'']*
| ['+' '-' '*' '=' '<' '>']
    { let id = Lexing.lexeme lexbuf in
      try 
        List.assoc id reservedWords
      with
      _ -> Parser.ID id
     }
(*
| '#' ['t' 'f'] 
      { let id = (Lexing.lexeme lexbuf) in
         Parser.SHARP (match id with | "#t" -> true | "#f" -> false) }
*)
| '\'' { Parser.QUOTEMARK }
| eof { Parser.EOF }


and comment = parse

  [^ '\012' '\n']* ['\012' '\n']+  { main lexbuf }
| _ { comment lexbuf }  
