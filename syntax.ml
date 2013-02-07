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
(*  | LetExp of (id * exp) list * bodyexp *)
(*  | NamedLetExp of id * (id * exp) list * bodyexp *)
  | LetrecExp of (id * exp) list * bodyexp
(*  | LetrecExp of (id * lambdaexp) list * bodyexp *)
  | CondExp of conditem list
  | AssignExp of id * exp
  | BeginExp of bodyexp
and lambdaexp = id list * bodyexp
and bodyexp = | S of exp | P of exp * bodyexp

and conditem =
  | Case of exp * bodyexp
  | Arrow of exp * exp
  | Else of bodyexp
and bind = id * exp

type program =  Prog of exp

type definitions = bind list



let parseLet bs body =
  let ids, args = List.split bs in
  let fn = LambdaExp (ids, body) in
  ApplyExp (fn, args)

let parseNamedLet name bs body =
  let ids, args = List.split bs in
  let l = LetrecExp ([(name, LambdaExp(ids, body))], S (VarExp name)) in
  ApplyExp (l, args) 

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
             IfExp (cond, (BeginExp body), make rest)
         | Arrow (cond, fn) -> (* temp を隠してしまう*)
	     let apv = ApplyExp (fn, [VarExp "temp"]) in
	     let body = IfExp (VarExp "temp", apv, make rest) in
             parseLet [("temp", cond)] (S body)
         | Else body -> BeginExp body)
  in make vs




let rec explist_to_body = function
    [x] -> S x
  | x :: rest  -> P (x, explist_to_body rest)

let explist_to_begin = function
    [x] -> x
  | x :: rest -> BeginExp (P(x, explist_to_body rest))
(*
let explist_to_beginn = function
    [x] -> x
  | x -> Beginn x
*)
let rec body_to_explist = function
  | S x -> [x]
  | P (x, rest) -> x :: body_to_explist rest
(*
let body_to_begin x =
   match (body_to_explist x) with
     [x] -> x
   | x -> Beginn x
*)

