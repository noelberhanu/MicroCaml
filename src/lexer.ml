open Types

(* Part 1: Lexer - IMPLEMENT YOUR CODE BELOW *)

let re_rparen = Str.regexp ")" 
let re_lparen = Str.regexp "(" 
let re_rcurly = Str.regexp "}" 
let re_lcurly = Str.regexp "{"  
let re_dot = Str.regexp "\\." 
let re_equal = Str.regexp "="
let re_notequal = Str.regexp "<>"
let re_greater = Str.regexp ">" 
let re_less = Str.regexp "<" 
let re_greaterequal = Str.regexp ">="  
let re_lessequal = Str.regexp "<="  
let re_or = Str.regexp "||"  
let re_and = Str.regexp "&&" 
let re_not = Str.regexp "not" 
let re_if = Str.regexp "if"
let re_then = Str.regexp "then"  
let re_else = Str.regexp "else"
let re_add = Str.regexp "+" 
let re_sub = Str.regexp "-"
let re_mult = Str.regexp "*"
let re_div = Str.regexp "/"
let re_concat = Str.regexp "\\^"
let re_let = Str.regexp "let"
let re_rec = Str.regexp "rec"
let re_in = Str.regexp "in"
let re_def = Str.regexp "def"
let re_fun = Str.regexp "fun"
let re_arrow = Str.regexp "->"
let re_space = Str.regexp " "
let re_num = Str.regexp "[0-9]+" 
let re_num_neg = Str.regexp "(-[0-9]+)"
let re_doublesemi = Str.regexp ";;"
let re_semi = Str.regexp ";"
let re_true = Str.regexp "true"
let re_false = Str.regexp "false"
let re_string = Str.regexp "\"[^\"]*\""
let re_id = Str.regexp "[a-zA-Z][a-zA-Z0-9]*"


let rec tok pos s tokens = 
        if pos >= String.length s then tokens 
        else 
            if(Str.string_match re_string s pos) then 
            let token = Str.matched_string s in
            let len = String.length token in
            let new_s = String.sub token (1) (len - 2) in 
            tok (pos + len) s (Tok_String new_s :: tokens)
        else (*Tok_Rec*)
	     if(Str.string_match re_rec s pos) then 
            let token = Str.matched_string s in
            let len = Str.match_end() in

            if (Str.string_match re_id s len) then 
                let id = Str.matched_string s in
                let new_len = Str.match_end() in
                let new_id = token^id in
                tok (new_len) s (Tok_ID (new_id) :: tokens)
            else 
            tok len s (Tok_Rec :: tokens) 

        else (*Tok_Not*)
	        if(Str.string_match re_not s pos) then 
                let token = Str.matched_string s in
                let len = Str.match_end() in
                if (Str.string_match re_id s len) then
                    let id = Str.matched_string s in
                    let new_len = Str.match_end() in
                    let new_id = token^id in
                    tok (new_len) s (Tok_ID (new_id) :: tokens)
            else 
            tok (len) s (Tok_Not :: tokens)
        else 
            if(Str.string_match re_if s pos) then
                let token = Str.matched_string s in
                let len = Str.match_end() in
                if (Str.string_match re_id s len) then
                    let id = Str.matched_string s in
                    let new_len = Str.match_end() in
                    let new_id = token^id in
                    tok (new_len) s (Tok_ID (new_id) :: tokens)
                else
            tok (len) s (Tok_If :: tokens)
        else 
            if(Str.string_match re_then s pos) then 
                let token = Str.matched_string s in
                let len = Str.match_end() in
                if (Str.string_match re_id s len) then
                    let id = Str.matched_string s in
                    let new_len = Str.match_end() in
                    let new_id = token^id in
                    tok (new_len) s (Tok_ID (new_id) :: tokens)
                else
            tok (len) s (Tok_Then :: tokens)
        else 
            if(Str.string_match re_else s pos) then 
                let token = Str.matched_string s in
                let len = Str.match_end() in
                if (Str.string_match re_id s len) then
                    let id = Str.matched_string s in
                    let new_len = Str.match_end() in
                    let new_id = token^id in
                    tok (new_len) s (Tok_ID (new_id) :: tokens)
                else
            tok (len) s (Tok_Else :: tokens)
        else
            if(Str.string_match re_let s pos) then 
                let token = Str.matched_string s in
                let len = Str.match_end() in
                if (Str.string_match re_id s len) then
                    let id = Str.matched_string s in
                    let new_len = Str.match_end() in
                    let new_id = token^id in
                    tok (new_len) s (Tok_ID (new_id) :: tokens)
                else
            tok (len) s (Tok_Let :: tokens)
        else 
            if(Str.string_match re_def s pos) then 
                let token = Str.matched_string s in
                let len = Str.match_end() in
                if (Str.string_match re_id s len) then
                    let id = Str.matched_string s in
                    let new_len = Str.match_end() in
                    let new_id = token^id in
                    tok (new_len) s (Tok_ID (new_id) :: tokens)
                else
            tok (len) s (Tok_Def :: tokens)
        else 
            if(Str.string_match re_in s pos) then
                let token = Str.matched_string s in
                let len = Str.match_end() in
                if (Str.string_match re_id s len) then
                    let id = Str.matched_string s in
                    let new_len = Str.match_end() in
                    let new_id = token^id in
                    tok (new_len) s (Tok_ID (new_id) :: tokens)
                else 
            tok (len) s (Tok_In :: tokens) 
        else 
            if(Str.string_match re_fun s pos) then 
                let token = Str.matched_string s in
                let len = Str.match_end() in
                if (Str.string_match re_id s len) then
                    let id = Str.matched_string s in
                    let new_len = Str.match_end() in
                    let new_id = token^id in
                    tok (new_len) s (Tok_ID (new_id) :: tokens)
                else
            tok (len) s (Tok_Fun :: tokens)
        else
            if(Str.string_match re_true s pos) then
                let token = Str.matched_string s in
                let len = Str.match_end() in
                if (Str.string_match re_id s len) then
                    let id = Str.matched_string s in
                    let new_len = Str.match_end() in
                    let new_id = token^id in
                    tok (new_len) s (Tok_ID (new_id) :: tokens)
                else
            tok (len) s (Tok_Bool true :: tokens)
        else 
            if(Str.string_match re_false s pos) then
                let token = Str.matched_string s in
                let len = Str.match_end() in
                if (Str.string_match re_id s len) then
                    let id = Str.matched_string s in
                    let new_len = Str.match_end() in
                    let new_id = token^id in
                    tok (new_len) s (Tok_ID (new_id) :: tokens)
                else
            tok (len) s (Tok_Bool false :: tokens)
        else
            if(Str.string_match re_id s pos) then 
            let token = Str.matched_string s in
            let len = String.length token in
            
            tok (pos + len) s (Tok_ID token :: tokens)
        else
	        if(Str.string_match re_arrow s pos) then 
            tok (pos + 2) s (Tok_Arrow :: tokens)
        else
            if(Str.string_match re_num_neg s pos) then
            let token = (Str.matched_string s) in 
            let len = String.length token in
            tok (pos + len) s (Tok_Int (int_of_string (String.sub token (1) (len - 2))) :: tokens)
        else
            if(Str.string_match re_num s pos) then 
            let token = (Str.matched_string s) in 
            tok (pos + String.length token) s (Tok_Int (int_of_string token) :: tokens) 
        else
            if(Str.string_match re_greaterequal s pos) then 
            (*tokenize "1>=2" -> [Tok_Int 1; Tok_Greater; Tok_Equal; Tok_Int 2]*)
            tok (pos + 2) s (Tok_GreaterEqual :: tokens)
        else 
            if(Str.string_match re_lessequal s pos) then 
            (*tokenize "1>=2" -> [Tok_Int 1; Tok_Greater; Tok_Equal; Tok_Int 2]*)
            tok (pos + 2) s (Tok_LessEqual :: tokens)
        else
            if(Str.string_match re_or s pos) then 
            tok (pos + 2) s (Tok_Or :: tokens)
        else 
            if(Str.string_match re_and s pos) then 
            tok (pos + 2) s (Tok_And :: tokens)
        else 
            if(Str.string_match re_doublesemi s pos) then (*ISSUE*)
            tok (pos + 2) s (Tok_DoubleSemi :: tokens)
        else 
            if(Str.string_match re_semi s pos) then (*ISSUE*)
            tok (pos + 1) s (Tok_Semi :: tokens)
        else 
            if(Str.string_match re_space s pos) then 
            tok (pos + 1) s tokens
        else
            if(Str.string_match re_rparen s pos) then
            tok (pos + 1) s (Tok_RParen :: tokens)
        else 
            if(Str.string_match re_lparen s pos) then
            tok (pos + 1) s (Tok_LParen :: tokens)
        else 
            if(Str.string_match re_rcurly s pos) then
            tok (pos + 1) s (Tok_RCurly :: tokens)
        else 
            if(Str.string_match re_lcurly s pos) then
            tok (pos + 1) s (Tok_LCurly :: tokens)
        else 
            if(Str.string_match re_dot s pos) then
            tok (pos + 1) s (Tok_Dot :: tokens)
        else 
            if(Str.string_match re_equal s pos) then 
            tok (pos + 1) s (Tok_Equal :: tokens)
        else 
            if(Str.string_match re_notequal s pos) then 
            tok (pos + 2) s (Tok_NotEqual :: tokens) 
        else 
            if(Str.string_match re_greater s pos) then 
            tok (pos + 1) s (Tok_Greater :: tokens)
        else 
            if(Str.string_match re_less s pos) then  
            tok (pos + 1) s (Tok_Less :: tokens)
        else 
	    if(Str.string_match re_sub s pos) then 
            tok (pos + 1) s (Tok_Sub :: tokens)
        else 
            if(Str.string_match re_mult s pos) then 
            tok (pos + 1) s (Tok_Mult :: tokens)
        else 
            if(Str.string_match re_div s pos) then 
            tok (pos + 1) s (Tok_Div :: tokens)
        else
	    if(Str.string_match re_add s pos) then 
            tok (pos + 1) s (Tok_Add :: tokens) 
        else  
	    if(Str.string_match re_concat s pos) then 
            (*ISSUE* "1^2", "^^" -> error*)
            tok (pos + 1) s (Tok_Concat :: tokens)
	    else 
            failwith "wtv"




let tokenize input = List.rev (tok 0 input [])


     
