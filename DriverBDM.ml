#use "Compile.ml";;
#use "PolyOps.ml";;
#use "EvalStackBDM.ml";;
#use "StorageBDM.ml";;




let store op stack storage = match op with SL.Const x -> storage#addRval x; stack#push x; print_endline ("after "^x); stack#printStack
| SL.Var x -> stack#push x; print_endline ("after "^x); stack#printStack
| SL.Fetch -> let variable = (match (stack#pop) with Some(y) -> y) in let value = storage#lookup variable in (match value with Some(x) -> stack#push x; print_endline ("after Fetch"); stack#printStack 
	| None -> print_endline ("Stack Underflow Error: Trying to access '"^variable^"' before initialization"); stack#init []; print_endline ("after Fetch"); stack#printStack)
| SL.Neg -> let value = (match stack#pop with Some(a) -> a) in storage#addRval value; stack#push (PolyOps.polyMinus("0",value)); print_endline ("after Neg"); stack#printStack
| SL.Plus ->  (match stack#pop2 with (two,one) -> let result = PolyOps.polyPlus(one,two) in storage#addRval result; stack#push result; print_endline ("after Plus"); stack#printStack)
| SL.Times -> (match stack#pop2 with (two,one) -> let result = PolyOps.polyTimes(one,two) in storage#addRval result; stack#push result; print_endline ("after Times"); stack#printStack)
| SL.Minus -> (match stack#pop2 with (two,one) -> let result = PolyOps.polyMinus(one,two) in storage#addRval result; stack#push result; print_endline ("after Minus"); stack#printStack)
| SL.Div -> (match stack#pop2 with (two,one) -> let result = PolyOps.polyDiv(one,two) in storage#addRval result; stack#push result; print_endline ("after Div"); stack#printStack)
| SL.Store -> (match stack#pop2 with (value, variable) -> (match (storage#lookup variable) with Some(x) -> storage#reassign variable value; stack#push value; print_endline ("after Store"); stack#printStack
	| None -> storage#assign (variable,value); stack#push value; print_endline ("after Store"); stack#printStack))(**bug was caused by mismatched parenthesis*)
| SL.Pop -> ()
;;




let rec evalPostfix pf stack storage = match pf with [] -> ()
| a::rest -> store a stack storage; evalPostfix rest stack storage
;;


(**new instance of eval stack created for every line*)
let compileCLine str storage = let stack = new eval_stack_bdm in let pf = (postfix str)@[SL.Pop] in let value = evalPostfix pf stack storage in ();;


(**one instance of storage created to maintain variable storage redundancy throughout lifetime of program and multiple lines of c code*)
let readLines filename =
  let ic = open_in filename in
     let tryRead () = try Some (input_line ic) with End_of_file -> None in
        let rec loop acc = match tryRead () with
          | Some s -> let line = String.trim(s)
                      in if (line = "" || line = "int main(){" || line = "}" || line = "return 0;" || line = "#include <stdio.h>") then loop acc     (*skip blank lines*)
                                      else loop (acc @ [line])
          | None -> close_in ic; acc
        in loop []
;;
(**readLines function from stack overflow: https://stackoverflow.com/questions/5774934/how-do-i-read-in-lines-from-a-text-file-in-ocaml*)


let rec processFile lineslist storage = match lineslist with [] -> storage#printChanges 
| line::rest -> compileCLine line storage; processFile rest storage
;;

let processor filename = let lines = readLines filename in let s = new storage_bdm in processFile lines s
;;



(**processor "your_test_file.c";;*)









