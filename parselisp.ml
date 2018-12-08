
 #use "topfind";;
#require "llvm";;
open Llvm;;

module SS = Set.Make(String);;

type var =
  |Int of int
  |String of string
  |List of var list
  
let cons car (List cdr)= List (car::cdr);;



let alpha_table= [|0;0XF7FFEC72;0XD7FFFFFF;0X57FFFFFF|];;

let alpha chr =
  let code = Char.code chr in 
    (Array.get alpha_table (code/32)) land (1 lsl (code mod 32))!=0;;

let leftBstr="(";;
let rightBstr=")";;

let apply_str="apply";;
let begin_str="begin";;
let define_str="define";;
let if_str="if";;
let lambda_str="lambda";;
let quote_str="quote";;

let keyword=SS.of_list [apply_str; begin_str;
                        define_str; if_str;
                        lambda_str; quote_str];;


let isSpace chr = String.contains " \t\r\n" chr ;;

let isLeftB chr = String.contains "([{" chr ;;

let isRightB chr = String.contains ")]}"  chr ;;

let rec lexAtom s i = 
  if i==String.length s then i
  else if (alpha (String.get s i)) then lexAtom s (i+1) 
  else i;;

let rec lexSexp s i =
  if i==String.length s then []
  else let chr = String.get s i in
    if isSpace chr then lexSexp s (i+1)
    else if isLeftB chr then leftBstr :: (lexSexp s (i+1))
    else if isRightB chr then rightBstr :: (lexSexp s (i+1))
    else let j = lexAtom s i in
      if i==j then ["Unknown symbol"]
      else (String.sub s i (j-i)) :: (lexSexp s j);;

let default_find elt set =
  if SS.mem elt set then (SS.find elt set, set)
  else (elt, SS.add elt set)

let rec _lexCode code i symbol_table tokens =
  if i==String.length code then (symbol_table, tokens)
  else let chr = String.get code i in
    if isSpace chr then _lexCode code (i+1) symbol_table tokens
    else if isLeftB chr then _lexCode code (i+1) symbol_table (leftBstr::tokens)
    else if isRightB chr then _lexCode code (i+1) symbol_table (rightBstr::tokens)
    else let j = lexAtom code i in
      if i==j then (symbol_table, ["Unknown symbol"])
      else let str = (String.sub code i (j-i)) in
           let (sym, table) = default_find str symbol_table in
             _lexCode code j table (sym::tokens);;

let lexCode code symbol_table = _lexCode code 0 symbol_table []



let rec parseL tokens =
  let rec parseS (token::last) =
    if token==leftBstr then
      let (sexp, tokens) = parseL last in (sexp, List.tl tokens)
    else (String token, last)
  in
    if tokens==[] then (List [],[])
    else if (List.hd tokens) == rightBstr then (List [], tokens)
    else let (sexp0, tokens0) = parseS tokens in
         let (sexp1, tokens1) = parseL tokens0 in
           (cons sexp0 sexp1, tokens1);;

let parseLexp codestr = let (sexp, _) = parseL (lexSexp codestr 0) in sexp;;

let rec flatten sexp b last=
match sexp with
|String s -> s::last
|List [] -> last
|List (x::xs) -> 
  if b then leftBstr :: (flatten x true (flatten (List xs) false (rightBstr::last)))
  else flatten x true (flatten (List xs) false last);;

let rec insert_space tokens =
 match tokens with
 |[] -> []
 |[token] -> [token]
 |t0::t1::last -> let tail = insert_space (t1::last) in
                  if (t0!=leftBstr && t1!=rightBstr) 
                    then t0::" "::tail
                    else t0::tail;;

let string_of_sexp sexp = String.concat "" (insert_space (flatten sexp true []));;

let test_code = 
"(define (fact n)
  (if (= n 0) 1
      (* n (fact (- n 1)))))

(define (exp a x n)
  (if (= n 0) a
  (if (even? n) (exp a (* x x) (/ n 2))
  (exp (* a x) x (- n 1))
)))";;
let test_tokens = lexCode test_code keyword;;
let test_Sexp = parseLexp test_code;;
let test_format = string_of_sexp test_Sexp;;
