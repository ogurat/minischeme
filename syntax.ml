(* minischeme syntax.ml *)

(* abstract syntax *)
type id = string

(*
type prim =
  Plus | Minus | Mul | Add1 | Sub1
| Eq | Lt | Gt
| Not 
| Cons | List | Car | Cdr | Null
 (* 演習の意図としてはkeyword *)
| Setcar | Setcdr
*)

type sexp =
     Int of int
  |  Bool of bool
  |  Var of id
  |  List of sexp list


type exp = 
    IntExp of int
  | BoolExp of bool
  | VarExp of id
  | UnitExp
(*  | PrimExp of prim * exp list *)
  | QuoteExp of sexp
  | IfExp of exp * exp * exp
(*  | AndExp of exp list *)
  | OrExp of exp list
  | DefineExp of bind
  | LambdaExp of lambdaexp
  | ApplyExp of exp * exp list
(*  | LetExp of (id * exp) list * exp *)
  | NamedLetExp of id * (id * exp) list * exp
  | LetrecExp of (id * exp) list * exp
(*  | LetrecExp of (id * lambdaexp) list * exp *)
  | CondExp of conditem list
  | AssignExp of id * exp
(*  | BeginExp of exp *)
  | SeqExp of exp * exp
and lambdaexp = id list * exp
(*
and bodyexp = | S of exp | P of exp * bodyexp
 *)
and conditem =
  | Case of exp * exp
  | Arrow of exp * exp
  | Else of exp
and bind = id * exp

type program =  Prog of exp

type definitions = bind list



let parseLet binds body =
  let ids, args = List.split binds in
  ApplyExp (LambdaExp (ids, body), args)

let parseNamedLet name binds body =
  let ids, args = List.split binds in
  let fn = LambdaExp(ids, body) in
  let l = LetrecExp ([(name, fn)], VarExp name) in
  ApplyExp (l, args) 

let parseNamedLet' names binds body =
  NamedLetExp (names, binds, body)

let parseAnd args =
  let rec make = function
    | []  -> BoolExp true
    | [x] -> x
    | x :: rest -> IfExp (x, make rest, BoolExp false)
  in make args

let parseCond vs = 
  let rec make = function
    | [] -> UnitExp
    | case :: rest ->
      (match case with
         | Case (cond, body) ->
             IfExp (cond, body, make rest)
         | Arrow (cond, fn) -> (* temp を隠してしまう*)
	     let apv = ApplyExp (fn, [VarExp "temp"]) in
	     let body = IfExp (VarExp "temp", apv, make rest) in
             parseLet [("temp", cond)]  body
         | Else body -> body)
  in make vs



(*

let rec explist_to_seq = function
    [x] -> x
  | x :: rest -> SeqExp (x, explist_to_seq rest)
 *)
(*
let explist_to_begin = function
    [x] -> x
  | x :: rest -> BeginExp (P(x, explist_to_body rest))
 *)
(*
let explist_to_beginn = function
    [x] -> x
  | x -> Beginn x
*)
(*
let rec body_to_explist = function
  | S x -> [x]
  | P (x, rest) -> x :: body_to_explist rest
 *)
(*
let body_to_begin x =
   match (body_to_explist x) with
     [x] -> x
   | x -> Beginn x
*)

