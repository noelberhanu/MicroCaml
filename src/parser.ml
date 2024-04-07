open Types
open Utils

(* Provided functions - DO NOT MODIFY *)

(* Matches the next token in the list, throwing an error if it doesn't match the given token *)
let match_token (toks : token list) (tok : token) =
  match toks with
  | [] -> raise (InvalidInputException (string_of_token tok))
  | h :: t when h = tok -> t
  | h :: _ ->
      raise
        (InvalidInputException
           (Printf.sprintf "Expected %s from input %s, got %s"
              (string_of_token tok)
              (string_of_list string_of_token toks)
              (string_of_token h)))

(* Matches a sequence of tokens given as the second list in the order in which they appear, throwing an error if they don't match *)
let match_many (toks : token list) (to_match : token list) =
  List.fold_left match_token toks to_match

(* Return the next token in the token list as an option *)
let lookahead (toks : token list) =
  match toks with [] -> None | h :: t -> Some h

(* Return the token at the nth index in the token list as an option*)
let rec lookahead_many (toks : token list) (n : int) =
  match (toks, n) with
  | h :: _, 0 -> Some h
  | _ :: t, n when n > 0 -> lookahead_many t (n - 1)
  | _ -> None

(* Part 2: Parsing expressions *)

let rec parse_expr toks = 
  let _ = print_string "hd:";print_string (string_of_token (List.hd toks)); print_string"\n" in
  match lookahead toks with 
  |Some Tok_Let -> parse_letexpr (match_token toks Tok_Let)
  |Some Tok_If -> parse_ifexpr (match_token toks Tok_If)
  |Some Tok_Fun -> parse_functionexpr (match_token toks Tok_Fun)
  |_ ->  parse_orexpr toks 


and parse_letexpr toks = 
  let (t, m) = parse_rec toks in
  match lookahead t with 
  |Some Tok_ID s -> let t_id = match_token t (Tok_ID s) in
    let t_equal = match_token t_id Tok_Equal in
    let (t', m1) = parse_expr t_equal in
    let _ = print_string "expr1:";print_string (string_of_expr m1);print_string"\n" in
    let tok = match_token t' Tok_In in
    let (t'', m2) = parse_expr tok in 
    (t'',Let(s,m,m1,m2))
  |_ -> failwith "parse_letexpr"


and parse_rec toks = 
  let t = lookahead toks in
  match t with 
  |Some Tok_Rec -> let matcher = match_token toks (Tok_Rec) in (matcher, true)
  |_ -> (toks, false)

and parse_functionexpr toks = match lookahead toks with 
|Some Tok_ID s -> let t = match_token toks (Tok_ID s) in 
                  let a = match_token t Tok_Arrow in
                  let (t', m) = parse_expr a in 
                  (t',Fun (s,m))
|_ -> failwith "parse"

and parse_ifexpr toks = 
  let (t,m) = parse_expr toks in
  match lookahead t with 
  |Some Tok_Then -> let t_then = match_token t Tok_Then in
                    let (t', m1) = parse_expr t_then in
                    let t_else = match_token t' Tok_Else in
                    let (t'', m2) = parse_expr t_else in
                    (t'', If (m, m1, m2))
  |_ -> failwith "parse"

and parse_orexpr toks = 
  let (t, m) = parse_andexpr toks in 
  match lookahead t with 
  |Some Tok_Or -> let matcher = match_token t (Tok_Or) in
                  let (t', m1) = parse_orexpr matcher in (t',Binop(Or, m, m1))
  |_ -> (t,m) 

and parse_andexpr toks = 
  let (t,m) = parse_equalityexpr toks in
  match lookahead t with
    |Some Tok_And -> let matcher = match_token t (Tok_And) in
                     let (t', m1) = parse_andexpr matcher in (t',Binop(And, m, m1))
    |_ -> (t,m)
  
and parse_equalityexpr toks = 
  let (t,m) = parse_relationalexpr toks in 
  match lookahead t with
  |Some Tok_Equal -> let matcher = match_token t (Tok_Equal) in
                     let (t', m1) = parse_equalityexpr matcher in
                     (t', Binop(Equal, m, m1))
  |Some Tok_NotEqual -> let matcher = match_token t (Tok_NotEqual) in
                     let (t', m1) = parse_equalityexpr matcher in
                     (t',Binop(NotEqual, m, m1))
  |_ -> (t,m)


and parse_relationalexpr toks = 
  let (t,m) = parse_additiveexpr toks in 
  match lookahead t with 
  |Some Tok_Less -> let matcher = match_token t (Tok_Less) in
                    let (t', m1) = parse_relationalexpr matcher in
                    (t',Binop(Less, m, m1))
  |Some Tok_Greater -> let matcher = match_token t (Tok_Greater) in
                       let (t', m1) = parse_relationalexpr matcher in
                       (t', Binop(Greater, m, m1))
  |Some Tok_GreaterEqual ->let matcher = match_token t (Tok_GreaterEqual) in
                           let (t', m1) = parse_relationalexpr matcher in
                           (t', Binop(GreaterEqual, m, m1))
  |Some Tok_LessEqual -> let matcher = match_token t (Tok_LessEqual) in
                         let (t', m1) = parse_relationalexpr matcher in
                         (t', Binop(LessEqual, m, m1))
  | _ -> (t,m)

and parse_additiveexpr toks = 
  let (t,m) = parse_multiplicativeexpr toks in
  match lookahead t with 
  |Some Tok_Add -> let matcher = match_token t (Tok_Add) in
                   let (t', m1) = parse_additiveexpr matcher in 
                   (t',Binop(Add, m, m1))
  |Some Tok_Sub -> let matcher = match_token t (Tok_Sub) in
                   let (t', m1) = parse_additiveexpr matcher in 
                   (t',Binop(Sub, m, m1))
  |_ -> (t,m)

and parse_multiplicativeexpr toks = 
  let (t,m) = parse_concatexpr toks in
  match lookahead t with
  |Some Tok_Mult -> let matcher = match_token t (Tok_Mult) in
                   let (t', m1) = parse_multiplicativeexpr matcher in 
                   (t',Binop(Mult, m, m1))
  |Some Tok_Div -> let matcher = match_token t (Tok_Div) in
                   let (t', m1) = parse_multiplicativeexpr matcher in 
                   (t',Binop(Div, m, m1))
  |_ -> (t,m)

and parse_concatexpr toks = 
  let (t,m) = parse_unaryexpr toks in
  match lookahead t with 
  |Some Tok_Concat -> let matcher = match_token t (Tok_Concat) in
                      let (t', m1) = parse_concatexpr matcher in
                      (t',Binop(Concat, m, m1))
  |_ -> (t,m)

and parse_unaryexpr toks = 
  let t = lookahead toks in 
  match t with 
  |Some Tok_Not -> let matcher = match_token toks (Tok_Not) in
              let (t', m1) = parse_unaryexpr matcher in
              (t',Not(m1))
  |_ -> parse_appexpr toks

and parse_appexpr toks = 
  let (t, m) = parse_selectexpr toks in
  match lookahead t with 
  |Some Tok_Int x-> let (t',m1) = parse_primaryexpr t in (t', App(m,m1)) 
  |Some Tok_Bool b -> let (t',m1) = parse_primaryexpr t in (t', App(m,m1)) 
  |Some Tok_String s -> let (t',m1) = parse_primaryexpr t in (t', App(m,m1)) 
  |Some Tok_ID s -> let (t',m1) = parse_primaryexpr t in (t', App(m,m1)) 
  |Some Tok_LParen -> let (t',m1) = parse_primaryexpr t in (t', App(m,m1)) 
  |Some Tok_LCurly -> let (t',m1) = parse_primaryexpr t in (t', App(m,m1)) 
  |_ -> (t,m)


and parse_selectexpr toks = 
  let (t, m) = parse_primaryexpr toks in
  (match lookahead t with 
  |Some Tok_Dot -> (let matcher = match_token t (Tok_Dot) in
                   (match lookahead matcher with 
                   |Some Tok_ID s -> (match_token matcher (Tok_ID s),Select(Lab s, m))
                   |_ -> failwith "parse_select"))
  |_ -> (t,m))


and parse_primaryexpr toks = 
  match lookahead toks with
  |Some Tok_Int x -> (match_token toks (Tok_Int x), Int x)
  |Some Tok_Bool b -> (match_token toks (Tok_Bool b),Bool b)
  |Some Tok_String s -> (match_token toks (Tok_String s),String s)
  |Some Tok_ID s -> (match_token toks (Tok_ID s),ID s)
  |Some Tok_LParen -> let matcher = match_token toks (Tok_LParen) in
                      let (t,m) = parse_expr matcher in 
                      let matcher1 = match_token t (Tok_RParen) in (matcher1,m)
  |_ -> parse_recordexpr toks




and parse_recordexpr toks = 
  let matcher = match_token toks Tok_LCurly in
  match lookahead matcher with 
  |Some Tok_RCurly -> (match_token matcher Tok_RCurly,Record[])
  |_ -> let (t,m) = parse_recordbodyexpr matcher [] in 
        let matcher1 = match_token t (Tok_RCurly) in
        (matcher1, Record m)

and parse_recordbodyexpr toks acc = 
  (let id = lookahead toks in
  (match id with 
  |Some Tok_ID s -> 
  let matcher = match_token toks (Tok_ID s) in
  let eq_matcher = match_token matcher Tok_Equal in
  let (t,m) = parse_expr eq_matcher in

    (*List remove*)
    (match lookahead t with 
    |Some Tok_Semi ->  parse_recordbodyexpr (match_token t Tok_Semi) ([(Lab s, m)] @ acc) 
    |_ -> (t, List.rev([(Lab s, m)]@acc)) )
  
  |_ -> failwith "parse"))

(* Part 3: Parsing mutop *)

let rec parse_mutop toks = 
  match toks with 
  |Tok_DoubleSemi :: t -> (t,NoOp)
  |Tok_Def :: t -> let (t', m) = parse_defmutop t in
                   let matcher = match_token t' Tok_DoubleSemi in
                   (matcher, m)
  |_ -> let (t',m) = parse_exprmutop toks in
             let matcher = match_token t' Tok_DoubleSemi in
             (matcher, m)


and parse_defmutop toks = 
  match toks with 
  | Tok_ID s :: t -> let matcher = match_token t Tok_Equal in
                     let (t',m1) = parse_expr matcher in 
                     let semi = match_token t' Tok_DoubleSemi in (semi, Def(s,m1))
  |_  -> failwith "parse_defmutop"

and parse_exprmutop toks =
  let (t,m) = parse_expr toks in
  let matcher = match_token t Tok_DoubleSemi in (matcher, Expr m)
  