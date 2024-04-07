open Types

(* Provided functions - DO NOT MODIFY *)

(* Adds mapping [x:v] to environment [env] *)
let extend env x v = (x, ref v) :: env

(* Returns [v] if [x:v] is a mapping in [env]; uses the
   most recent if multiple mappings for [x] are present *)
let rec lookup env x =
  match env with
  | [] -> raise (DeclareError ("Unbound variable " ^ x))
  | (var, value) :: t -> if x = var then !value else lookup t x

(* Creates a placeholder mapping for [x] in [env]; needed
   for handling recursive definitions *)
let extend_tmp env x = (x, ref (Int 0)) :: env

(* Updates the (most recent) mapping in [env] for [x] to [v] *)
let rec update env x v =
  match env with
  | [] -> raise (DeclareError ("Unbound variable " ^ x))
  | (var, value) :: t -> if x = var then value := v else update t x v

(* Part 1: Evaluating expressions *)

(* Evaluates MicroCaml expression [e] in environment [env],
   returning an expression, or throwing an exception on error *)
let rec eval_expr env expr = 
  match expr with
  | Int _ | Bool _ | String _ -> expr
  | ID var -> lookup env var
  | Fun (param, body) -> Closure (env, param, body)
  | Not e ->
      (match eval_expr env e with
       | Bool b -> Bool (not b)
       | _ -> raise (TypeError "Expected type bool"))
  | Binop (op, e1, e2) ->
      let v1 = eval_expr env e1 in
      let v2 = eval_expr env e2 in
      (match op, v1, v2 with
       | Add, Int x, Int y -> Int (x + y)
       | Sub, Int x, Int y -> Int (x - y)
       | Mult, Int x, Int y -> Int (x * y)
       | Div, Int x, Int y ->
           if y = 0 then raise DivByZeroError else Int (x / y)
       | Greater, Int x, Int y -> Bool (x > y)
       | Less, Int x, Int y -> Bool (x < y)
       | GreaterEqual, Int x, Int y -> Bool (x >= y)
       | LessEqual, Int x, Int y -> Bool (x <= y)
       | Concat, String x, String y -> String (x ^ y)
       | Equal, x, y -> 
           (match x, y with
            | Int i, Int j -> Bool (i = j)
            | Bool b1, Bool b2 -> Bool (b1 = b2)
            | String s1, String s2 -> Bool (s1 = s2)
            | _, _ -> raise (TypeError "Cannot compare different types"))
       | NotEqual, x, y -> 
           (match x, y with
            | Int i, Int j -> Bool (i <> j)
            | Bool b1, Bool b2 -> Bool (b1 <> b2)
            | String s1, String s2 -> Bool (s1 <> s2)
            | _, _ -> raise (TypeError "Cannot compare different types"))
       | Or, Bool x, Bool y -> Bool (x || y)
       | And, Bool x, Bool y -> Bool (x && y)
       | _, _, _ -> raise (TypeError "Invalid operands")) (*needs to specify error*)
  |If (guard, e1, e2) ->
      (match eval_expr env guard with
       | Bool true -> eval_expr env e1
       | Bool false -> eval_expr env e2
       | _ -> raise (TypeError "Expected type bool"))
  |Let (var, recursive, e1, e2) -> 
      let new_env =
        if recursive then
          let placeholder = ref (Bool false) in
          (var, placeholder) :: env
        else env in
      let v1 = eval_expr env e1 in
      (match new_env with
       | (var', placeholder) :: rest -> placeholder := v1
       | _ -> ()); eval_expr ((var, ref v1) :: env) e2
  | App (e1, e2) ->
      (match eval_expr env e1 with
       | Closure (env', param, rest) ->
           let arg = eval_expr env e2 in
           let env'' = extend env param arg in 
           eval_expr env'' rest
       | _ -> raise (TypeError "Not a function"))
  |_ -> failwith "lol"

(* Let ("f", false, Fun ("x", Fun ("y", Binop (Add, ID "x", ID "y"))),
  App (App (ID "f", (Int 1)), (Int 2))) *) 

(* Evaluates MicroCaml mutop directive [m] in environment [env],
   returning a possibly updated environment paired with
   a value option; throws an exception on error *)
let eval_mutop env m = failwith "unimplemented"
