#use "Compile.ml";;
#use "PolyOps.ml";;
#use "EvalStackBDM.ml";;
#use "StorageBDM.ml";;


(**code updated with hashtable storage for variables*)


let store op stack storage = match op with SL.Const x -> storage#addRval x; stack#push x
| SL.Var x -> stack#push x
| SL.Fetch -> let variable = (match (stack#pop) with Some(y) -> y) in let value = storage#lookup variable in (match value with Some(x) -> stack#push x 
	| None -> print_endline ("Stack Underflow Error: Trying to access '"^variable^"' before initialization"); stack#init [])
| SL.Neg -> let value = (match stack#pop with Some(a) -> a) in storage#addRval value; stack#push (PolyOps.polyMinus("0",value))
| SL.Plus ->  (match stack#pop2 with (two,one) -> let result = PolyOps.polyPlus(one,two) in storage#addRval result; stack#push result)
| SL.Times -> (match stack#pop2 with (two,one) -> let result = PolyOps.polyTimes(one,two) in storage#addRval result; stack#push result)
| SL.Minus -> (match stack#pop2 with (two,one) -> let result = PolyOps.polyMinus(one,two) in storage#addRval result; stack#push result)
| SL.Div -> (match stack#pop2 with (two,one) -> let result = PolyOps.polyDiv(one,two) in storage#addRval result; stack#push result)
| SL.Store -> (match stack#pop2 with (value, variable) -> (match (storage#lookup variable) with Some(x) -> storage#reassign variable value; stack#push value
	| None -> storage#assign (variable,value); stack#push value))(**bug was caused by mismatched parenthesis*)
| SL.Pop -> ()
;;




let rec evalPostfix pf stack storage = match pf with [] -> ()
| a::rest -> store a stack storage; evalPostfix rest stack storage
;;


(**new instance of eval stack created for every line*)
let compileCLine str storage = let stack = new eval_stack_bdm in let pf = (postfix str)@[SL.Pop] in let value = evalPostfix pf stack storage in ();;


(**one instance of storage created to maintain variable storage redundancy throughout lifetime of program and multiple lines of c code*)
let processFile filename = let storage = new storage_bdm in let file = open_in filename in try while true do let line = input_line file in compileCLine line storage
done with End_of_file -> close_in file; storage#printChanges
;;

















(**let store op stack storage = match op with SL.Const x -> stack#push x
| SL.Var x -> stack#push x
| SL.Fetch -> let variable = (match (stack#pop) with Some(y) -> y) in let value = storage#lookup variable in (match value with Some(x) -> stack#push x 
	| None -> print_endline ("Error: Trying to acces '"^variable^"' before initialization"); stack#init [])
| SL.Neg -> let value = (match stack#pop with Some(a) -> a) in stack#push (PolyOps.polyMinus("0",value))
| SL.Plus -> let valuetwo = (match stack#pop with Some(a) -> a) in let valueone = (match stack#pop with Some(b) -> b) in stack#push (PolyOps.polyPlus(valueone,valuetwo))
| SL.Times -> let valuetwo = (match stack#pop with Some(a) -> a) in let valueone = (match stack#pop with Some(b) -> b) in stack#push (PolyOps.polyTimes(valueone,valuetwo))
| SL.Minus -> let valuetwo = (match stack#pop with Some(a) -> a) in let valueone = (match stack#pop with Some(b) -> b) in stack#push (PolyOps.polyMinus(valueone,valuetwo))
| SL.Div -> let valuetwo = (match stack#pop with Some(a) -> a) in let valueone = (match stack#pop with Some(b) -> b) in stack#push (PolyOps.polyDiv(valueone,valuetwo))
| SL.Store -> let value = (match stack#pop with Some(a) -> a) in let variable = (match stack#pop with Some(b) -> b) 
	in (match (storage#lookup variable) with Some(x) -> storage#reassign variable value; stack#push value
	| None -> storage#assign (variable,value); stack#push value) (**bug was caused by mismatched parenthesis*)
| SL.Pop -> ()
;;*)









