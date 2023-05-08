
#use "Parser.ml";;

module CE = struct
   type exp = ..  (* We will decode string to int versus float at the end, not before. *)
   type exp += Const of string | Var of string | Neg of exp
      | Plus of exp * exp | Minus of exp * exp 
      | Times of exp * exp | Div of exp * exp 
end;;

module SL = struct
   type token = ..
   type token += Const of string | Var of string | Neg | Plus | Minus 
      | Times | Div | Fetch | Store | Pop
end;;


(**------------------now we do the extension things-------------------------*)

module CE = struct
   include CE            (* this is the earlier one *)
   type exp += Sqrt of exp
                  | Assign of exp * exp
                  | PreInc of exp
                  | PostInc of exp
                  | PreDec of exp
                  | PostDec of exp
                  | ArrayEntry of string * exp

   exception BadUBT of string


   let rec ubtree2exp ubt = match ubt with
      | Parser.EmptyUBT -> Const "0"
      | Parser.Leaf(Parser.Constlex c) -> Const c
      | Parser.Leaf(Parser.Varlex s) -> Var s
      | Parser.Leaf(_) -> raise (BadUBT (Parser.ubtree2string (ubt,4,4)))
      | Parser.UNode(op, subtree) -> (match op with
          | "-" -> Neg(ubtree2exp subtree)
          | "--@" -> PreDec(ubtree2exp subtree)
          | "++@" -> PreInc(ubtree2exp subtree)
          | "@++" -> PostInc(ubtree2exp subtree)
          | "@--" -> PostDec(ubtree2exp subtree)
          | _ -> raise (BadUBT ("Unlexed op " ^ op))
          
        )
      | Parser.BNode(left,op,right) -> (match op with 
          | "=" -> Assign(ubtree2exp left, ubtree2exp right)
          | "+" -> Plus(ubtree2exp left, ubtree2exp right)
          | "-" -> Minus(ubtree2exp left, ubtree2exp right)
          | "*" -> Times(ubtree2exp left, ubtree2exp right)
          | "/" -> Div(ubtree2exp left, ubtree2exp right)
          | _ -> raise (BadUBT op)
      )
      (* | _ -> raise (BadUBT (Parser.ubtree2string (ubt,4,4)))   Impossible *)

end;;

module SL = struct
   include SL
   type token += Sqrt
   type token += PreInc | PostInc | ArrayEntry   (* none actually needed *)

   exception WaitingForGodot;;

   (* This no longer has a curried "atostring" function argument
    *)
   let token2string token = match token with
      | Const str ->str 
      | Var str -> str
      | Neg -> "uminus"
      | Plus -> "+"
      | Minus -> "-"
      | Times -> "*"
      | Div -> "Div"
      | Fetch -> "fetch"
      | Store -> "store"
      | Pop -> "pop"
      | Sqrt -> "sqrt"
      | _ -> raise WaitingForGodot
;;

end;;




module Compile = struct
   exception InvalidEntry;;
   let e = "8";;   (* word size of 64-bit float in an array---note this is a string *)
   
   let rec pcompile (tree, isLvalue) = match tree with
      | CE.Const a -> [SL.Const a]
      | CE.Var s -> if isLvalue then [SL.Var s] else [SL.Var s; SL.Fetch]   (** key point of isLvalue **)
      | CE.Neg exp -> (pcompile (exp, isLvalue))@[SL.Neg]                  (** see printed key for subtle point **)
      | CE.Plus (lexp,rexp) -> (pcompile (lexp,false)) @ (pcompile (rexp,false)) @ [SL.Plus]
      | CE.Minus (lexp,rexp) -> (pcompile (lexp,false)) @ (pcompile (rexp,false)) @ [SL.Minus]
      | CE.Times (lexp,rexp) -> (pcompile (lexp,false)) @ (pcompile (rexp,false)) @ [SL.Times]
      | CE.Div (lexp,rexp) -> (pcompile (lexp,false)) @ (pcompile (rexp,false)) @ [SL.Div]
      | CE.Sqrt exp -> (pcompile (exp,false)) @ [SL.Sqrt]
   
      | CE.Assign(lexp,rexp) -> (pcompile(lexp, true))@(pcompile(rexp, false))@[SL.Store]
      (* PreInc presumes the result is treated only as an rvalue *)
      | CE.PreInc(CE.Var s)  -> [SL.Var s; SL.Var s; SL.Fetch; SL.Const "1"; SL.Plus; SL.Store; SL.Pop]
      | CE.PostInc(CE.Var s) -> [SL.Var s; SL.Fetch; SL.Var s; SL.Var s; SL.Fetch; SL.Const "1"; 
                                 SL.Plus; SL.Store; SL.Pop]
      | CE.PreDec(CE.Var s)  -> [SL.Var s; SL.Var s; SL.Fetch; SL.Const "1"; SL.Minus; SL.Store; SL.Pop]
      | CE.PostDec(CE.Var s) -> [SL.Var s; SL.Fetch; SL.Var s; SL.Var s; SL.Fetch; SL.Const "1"; 
                                 SL.Minus; SL.Store; SL.Pop]
   
      (* iexp is not typed as int exp but allowed to be exp *)
      | CE.ArrayEntry(arr, iexp) -> [SL.Var arr; SL.Const e]@(pcompile(iexp,false))@[SL.Times; SL.Plus]
                                           @(if isLvalue then [] else [SL.Fetch])
   
      | CE.PreInc(_) -> raise InvalidEntry   (* This considers ++(++x) to be invalid, like in C and Java *)
      | CE.PostInc(_) -> raise InvalidEntry  (* BUT, we could allow doign either to an ArrayEntry *)
      | CE.PreDec(_) -> raise InvalidEntry   
      | CE.PostDec(_) -> raise InvalidEntry
      | _ -> raise SL.WaitingForGodot
   ;;
   
   

   
   let rec tokenList2String ell = match ell with
      [] -> ""
      | [token] -> (SL.token2string token)
      | token::rest -> (SL.token2string token)^" "^tokenList2String rest;;
   
   
end;;


let postfix str = let exptree = CE.ubtree2exp (comp str) in
   Compile.pcompile (exptree,false);;

let pf str = print_endline(Compile.tokenList2String (postfix str));;

   
