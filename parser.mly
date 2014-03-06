%{
open Syntax
%}

%token LPAREN RPAREN
/* %token PLUS MINUS MUL ADD1 SUB1 */
/* %token EQ LT GT  NOT SETCAR SETCDR */
%token DEFINE LET IF AND OR COND LAMBDA LETREC SET BEGIN 
%token NOT
%token QUOTE QUOTEMARK
%token ELSE ARROW
%token EOF

%token <int> INTV
%token <Syntax.id> ID
%token <bool> SHARP

%start toplevel
%type <Syntax.program> toplevel

%start definitions
%type <Syntax.definitions> definitions

%start expression
%type <Syntax.exp> expression
%%


expression :
    Exp { $1 }

toplevel :
    Exp { Prog $1 }

definitions :
    DefList EOF { $1 }

Exp :
  | INTV { IntExp $1 }
  | ID { VarExp $1 }
  | SHARP { BoolExp $1 }
  | LPAREN FormExp RPAREN { $2 }
  | QUOTEMARK Sexp { QuoteExp ($2) }

FormExp :
/* | PrimOp Explist  { PrimExp ($1, $2) } */
  | IF Exp Exp Exp  { IfExp ($2, $3, $4) }
  | AND Explist  { parseAnd $2 }
/* | AND Explist { AndExp ($2) } */
  | OR Explist  { OrExp ($2) }
  | Define { DefineExp $1 }
/* | DEFINE ID Exp { DefineExp ($2, $3) } */
/* | DEFINE LPAREN ID IDlist RPAREN Body { DefineExp ($3, LambdaExp ($4, $6)) } */
  | LET LPAREN Bindings RPAREN Body { parseLet $3 $5 }
/* | LET LPAREN Bindings RPAREN Body { LetExp ($3, $5) } */
  | LET ID LPAREN Bindings RPAREN Body { parseNamedLet $2 $4 $6 }
/* | LET ID LPAREN Bindings RPAREN Body  { NamedLetExp ($2, $4, $6) } */
  | COND Condlist  { parseCond $2 }
  | LambdaExp  { $1 }
/*  | LAMBDA LPAREN IDlist RPAREN Body  { LambdaExp ($3, $5) } */
  | Exp Explist { ApplyExp ($1, $2) }
  | LETREC LPAREN LambdaBindings RPAREN Body { LetrecExp ($3, $5) }
  | SET ID Exp  { AssignExp ($2, $3) }
  | BEGIN Body { $2 }
/*  | BEGIN Body { BeginExp $2 } */
  | QUOTE Sexp  { QuoteExp ($2) }

Sexp :
  | INTV  { Int $1 }
  | SHARP { Bool $1 }
  | ID    { Var $1 }
  | LPAREN Sexplist RPAREN { List ($2) }

Sexplist :
  | { [] }
  | Sexp Sexplist { $1 :: $2 }

/*
PrimOp :
    PLUS { Plus }  | MINUS { Minus } | MUL { Mul }
  | ADD1 { Add1 }  | SUB1 { Sub1 }
  | EQ { Eq} | LT { Lt } | GT { Gt }
  | SETCAR { Setcar } | SETCDR { Setcdr }
  | NOT { Not } 
  | AND { And }  | OR { Or }
*/

Bindings:
  | { [] }
  | LPAREN ID Exp RPAREN Bindings { ($2, $3) :: $5 }

LambdaExp :
  | LAMBDA LPAREN IDlist RPAREN Body { LambdaExp ($3, $5) }

LambdaBindings :
  | { [] }
  | LPAREN ID LPAREN LambdaExp RPAREN RPAREN LambdaBindings { ($2, $4) :: $7 }


IDlist :
  | { [] }
  | ID IDlist { $1 :: $2 }
Body :
  | Exp { $1 }
  | Exp Body { SeqExp ($1, $2) }
/*
Body :
  | Exp      { S $1 } 
  | Exp Body { P ($1, $2) }
*/
Explist :
  | { [] }
  | Exp Explist { $1 :: $2 }
    
Define :
  | DEFINE ID Exp  { ($2, $3) }
  | DEFINE LPAREN ID IDlist RPAREN Body { ($3, LambdaExp ($4, $6)) }

DefList :
  |  { [] }
  | LPAREN Define RPAREN DefList { $2 :: $4 }

Condlist :
  | { [] }
  | LPAREN Conditem RPAREN Condlist { $2 :: $4 }

Conditem :
  | Exp Body { Case ($1, $2) }
  | Exp ARROW Exp { Arrow ($1, $3) }
  | ELSE Body  { Else ($2) }
