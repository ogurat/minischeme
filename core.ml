(* minischeme core.ml *)
open Syntax

(* Expressed values *)
type exval = 
    IntV of int
  | BoolV of bool
  | SymbolV of id
  | ProcV of id list * exp * env
  | PrimV of (exval list -> exval)  (* パラメータは　dnvalとしない *)
  | PairV of dnval * dnval
  | EmptyListV
  | UnitV
  | UnboundV

(* Denoted values *)
and dnval = exval ref

and env = 
    EmptyEnv
  | ExtendEnv of id list * dnval array * env


(* pretty printing *)
let pplist =
  let rec pprest = function
      [] -> ""
    | a :: b ->  " " ^ a ^ pprest b in
  function
      [] -> ""
    | x :: rest -> "(" ^  x ^ pprest rest ^ ")"

let rec printval = function
    IntV i -> string_of_int i
  | BoolV i -> (if i then "#t" else "#f")
  | SymbolV s -> s
  | ProcV (args, body, env) ->
       "#proc:" ^ (pplist args)
  | PrimV _ -> "primitive"
  | PairV (a, b) -> "(" ^ printval !a ^ pppair !b ^ ")"
  | EmptyListV ->  "()"
  | UnitV ->  "#void"
  | UnboundV -> "*unbound*"

and pppair = function(* PairVの第2要素 *)
    EmptyListV -> "" 
  | PairV (a, b) ->  " " ^ printval !a ^ pppair !b
  | arg ->  (" . ") ^ printval arg
   

(* ---------------- environment -------------------------- *)

exception UnboundVar of string

let empty_env () = EmptyEnv

(* Syntax.id list -> dnval list -> env -> env *)
let extend_env ids (dnvals: dnval list) (env: env) =
  (* assumes List.length ids = List.length dnvals *)
  ExtendEnv (ids, Array.of_list dnvals, env)

let extend_env_exval ids exvals env =
  let dnvals = List.map (fun x -> ref x) exvals in
  ExtendEnv (ids, Array.of_list dnvals, env)


let rec lookup id =
   let rec list_pos c = function
       [] -> None
     | m :: rest -> if id = m then Some c else list_pos (c + 1) rest in
   function
     EmptyEnv -> raise (UnboundVar id)
   | ExtendEnv (ids, dnvals, rest) -> 
       match list_pos 0 ids with
         | Some x -> dnvals.(x)
         | None   -> lookup id rest



(* --------------- interpreter core ---------------------- *)


let rec eval_exp env = function
    IntExp i -> IntV i
  | BoolExp v -> BoolV v
  | VarExp sym -> !(lookup sym env)
  | QuoteExp exp -> eval_sexp exp
  | UnitExp -> UnitV
(*  | PrimExp (p, es) ->
      let args = eval_prim_operands env es in
      apply_prim p args *)
  | IfExp (pred, e1, e2) -> 
      eval_exp env 
         (match eval_exp env pred with
            BoolV false -> e2 | _ -> e1)
(*
  | AndExp args ->
      let rec make result = function
        | [] -> result
        | a :: rest ->
            (match result with 
              | BoolV false -> result | _ -> make (eval_exp env a) rest)
      in make (BoolV true) args
*)
  | OrExp args ->
      let rec loop result = function
        | [] -> result
        | a :: rest ->
            (match result with 
              | BoolV false ->  loop (eval_exp env a) rest | _ -> result)
      in loop (BoolV false) args
(*  | Define (id, exp) ->
      let v = eval_exp env exp in
      let env' = extend_env_rec [id] [exp] env in *)
  | LambdaExp (ids, body) ->
      ProcV (ids, body, env)
  | ApplyExp (op, operands) -> (* operands: exp list 引数 *)
      let proc = eval_exp env op
      and args = List.map (eval_exp env) operands in
      eval_apply proc args
(*  | LetExp (bs, body) ->
      let ids, args = List.split bs in
      let args' = List.map (eval_exp env) args in
      let env' = extend_env2 ids args' env in
      eval_exp env' body *)
  | NamedLetExp (id, binds, body) -> (* 2014/2/16 oscheme/eval.mlからコピー  *)
      let (ids, args) = List.split binds in
      let fn = LambdaExp (ids, body) in
      let a = extend_env_rec_exp env [id,fn] in
      eval_apply (eval_exp a (VarExp id)) (List.map (eval_exp env) args)
  | CondExp vs ->
      let rec loop = function
	| [] -> UnitV
        | i :: rest ->
            (match i with
              | Case (cond, body) ->
                  (match (eval_exp env cond) with
                    | BoolV false -> loop rest
                    | v           -> eval_exp env body)
              | Arrow (cond, fn) ->
		  (match (eval_exp env cond) with
                    | BoolV false -> loop rest
                    | v           -> eval_apply (eval_exp env fn) [v])
	      | Else (body) -> eval_exp env body)
      in loop vs
  | LetrecExp (binds, body) ->
      let newenv = extend_env_rec_exp env binds in
      eval_exp newenv body
  | AssignExp (id, exp) ->
      let idref = lookup id env in
      begin idref := eval_exp env exp; !idref end
(*
  | BeginExp body ->
      eval_exp env body
 *)
  | SeqExp (x, y) ->
      eval_exp env x; eval_exp env y

       
and eval_sexp = function
    Int x -> IntV x
  | Bool x -> BoolV x
  | Var x  -> SymbolV x
  | List x ->
    let rec loop = function
      | [] -> EmptyListV
      | (x::xs) -> PairV (ref (eval_sexp x), ref (loop xs)) in
    loop x

and eval_apply (proc: exval) (args: exval list) =
  (match proc with
    | ProcV (ids, body, envproc) -> (* envproc: lambdaを評価したときの環境 *)
        (* パラメータの数のチェックが必要 *)
        if List.length ids = List.length args then
          eval_exp (extend_env_exval ids args envproc) body
	else 
          failwith "# of parameters and arguments don't match"
    | PrimV closure ->
        closure args
    | _ -> failwith "Applying a non-procedure value")

(*
and eval_body env =
  eval_exp env
 *)
(*
and eval_body env = function
  | S e -> eval_exp env e
  | P (e,rest) -> eval_exp env e;
                  eval_body env rest
 *)

and extend_env_rec_exp env binds =
  let syms, explist = List.split binds in
  let vec = Array.make (List.length syms) (ref UnboundV) in
  let newenv = ExtendEnv (syms, vec, env) in
  let rec loop f i = function
      [] -> ()
    | x :: ls -> f i x; loop f (i + 1) ls in
  loop (fun i exp -> vec.(i) <- ref (eval_exp newenv exp)) 0 explist;
  newenv



(* initial env *)
(*
let global_env =

  let v = [ "false", BoolV false; "true",BoolV true] in
  let (ids, vs) = List.split v in

  extend_env_exval ids vs
    (empty_env())
*)
(*
let global_env =
  extend_env
    ["false"; "true"]
    [ref (BoolV false); ref (BoolV true)]
    global_env
*)

let eqp x y =
  match (x, y) with
  | (BoolV a, BoolV b) ->  a = b
  | (SymbolV s, SymbolV s2) when s = s2 ->  true
  | (EmptyListV, EmptyListV) ->  true
(*  | VectorV a, VectorV b when a == b ->  true *)
  | _ ->  false

let eqvp x y =
  match (x, y) with
  | a,b when (eqp a b) -> true
  | (IntV x, IntV y) when x = y -> true
  | _ ->  false

let rec equalp x y =
  match (x, y) with
  | PairV (a, b), PairV (c, d) -> (equalp !a !c) && (equalp !b !d)
(*  | VectorV a, VectorV b -> false *)
  | a,b when (eqvp a b) ->  true
  | _ ->  false

let global_env =
  let prims = [
  "+",(
    let rec loop = function
      | [IntV i] -> i
      | IntV a :: tl -> a + loop tl 
      | _ -> failwith "Arity mismatch: +"
    in fun args -> IntV (loop args));
  "-",(fun args ->
    let rec f = function
        [IntV i; IntV j] -> i - j
      | _ -> failwith "Arity mismatch: -"
    in IntV (f args));
  "*",(
    let rec loop = function
        [IntV i] -> i
      | IntV a :: tl -> a * loop tl 
      | _ -> failwith "Arity mismatch: *"
    in fun args -> IntV (loop args));
  "add1", (function
      [IntV i] -> IntV (i + 1)
    | _ -> failwith "Arity mismatch: add1");
  "sub1", (function
    | [IntV i] -> IntV (i - 1)
    | _ -> failwith "Arity mismatch: sub1");
  "=", (function
    | [IntV i; IntV j] -> BoolV (i = j)
    | _ -> failwith "Arity mismatch: =");
  "<", (function
      [IntV i; IntV j] -> BoolV (i < j)
    | _ -> failwith "Arity mismatch: <");
  ">", (function
      [IntV i; IntV j] -> BoolV (i > j)
    | _ -> failwith "Arity mismatch: >");
  "not", (function 
      [BoolV false] -> BoolV true
    | [_]           -> BoolV false
    | _ -> failwith "Arity mismatch: not");
  "eq?", (function
    | [a;b] -> BoolV (eqp a b)
    | _ -> failwith "Arity mismatch: eq?");
  "eqv?", (function
    | [a;b] -> BoolV (eqvp a b)
    | _ -> failwith "Arity mismatch: eqv?");
  "equal?", (function
    | [a;b] -> BoolV (equalp a b)
    | _ -> failwith "Arity mismatch: equal?");
  "cons", (function 
    | [a; b] -> PairV (ref a, ref b)
    | _ ->  failwith "Arity mismatch: cons");
  "car", (function
    | [PairV (a, _)] -> !a
    | _ -> failwith "Arity mismatch: car");
  "cdr", (function
    | [PairV (_, b)] -> !b 
    | _ -> failwith "Arity mismatch: cdr");
  "set-car!", (function (* setcar *)
      [PairV (a, _) as y; x] -> begin a := x; y end
    | _ -> failwith "Arity mismatch: setcar");
  "set-cdr!", (function (* setcdr *)
      [PairV (_, b) as y; x] -> begin b := x; y end
    | _ -> failwith "Arity mismatch: setcdr");
  "null?", (function 
      [EmptyListV] -> BoolV true
    | [_] -> BoolV false
    | _ -> failwith "Arity mismatch: null?");
  "list", (fun args -> 
    let rec make = function
        [] -> EmptyListV
      | a :: b -> PairV (ref a, ref (make b))
    in make args);
  "map", (function
      [proc;l] ->
        let rec map = (function
          | EmptyListV -> EmptyListV
          | PairV(x, rest) ->
               PairV (ref (eval_apply proc [!x]), ref (map !rest))
          | _ -> failwith "not pair: map")
	in map l
    | _ -> failwith "Arity mismatch: map");
  "foldl", (function
      [proc;init;l] ->
        let rec fold accum = (function
          | EmptyListV -> accum
          | PairV(x, rest) ->
              fold (eval_apply proc [!x;accum]) !rest
          | _ -> failwith "not pair: foldl")
        in fold init l
    | _ -> failwith "Arity mismatch: foldl");
  "foldr", (function
      [proc;init;l] ->
        let rec fold = (function
          | EmptyListV -> init
          | PairV(x, rest) ->
              let a = fold !rest in
              eval_apply proc [!x;a]
          | _ -> failwith "not pair: foldr")
        in fold l
    | _ -> failwith "Arity mismatch: foldl");
  "write", (function
      [v] -> printval v; UnitV
    | _ -> failwith "Arity mismatch: write");
  "display", (function
      [v] -> printval v; UnitV
    | _ -> failwith "Arity mismatch: display");
  ] in
  let (ids, vs) = List.split prims in
  let vs' = List.map (fun x -> ref (PrimV x)) vs in
  extend_env ids vs' (empty_env ())


let extend_env_exp env binds =
  let syms, exps = List.split binds in
  let vs = List.map (eval_exp env) exps in
  extend_env_exval syms vs env

let make_define_env : bind list -> env =
  extend_env_rec_exp global_env


let eval_program env (Prog e) = eval_exp env e

