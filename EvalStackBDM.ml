class ['a] eval_stack_bdm = object (self)
	val mutable stack = ([] : 'a list) 
	(**val mutable var_stack = ([] : 'a list)*)

	method init ls = stack <- ls 

	method pop = match stack with [] -> print_endline "Error: Stack Underflow"; None 
  	| head::tail -> stack <- tail; Some(head)

  	method push (data : 'a) = stack <- data::stack

  	method peek = match stack with [] -> None 
  	| head::tail -> Some(head) 

  	method size = let rec sizeh sh = match sh with [] -> 0
  	| head::tail -> 1 + sizeh tail in sizeh stack

  	method printStack = let rec printStackh s = match s with [] -> () 
  	| head::tail -> print_endline head; printStackh tail  
  	in printStackh stack 

  	method pop2 = let firstpop = self#pop in let secondpop = self#pop in ((match firstpop with Some(x) -> x),(match secondpop with Some(y) -> y))


end;;

