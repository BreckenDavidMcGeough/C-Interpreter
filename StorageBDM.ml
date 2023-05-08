
class storage_bdm = object (self)

	val mutable stack_changes = ([] : string list) (**val so dont have to use self# when referencing variable in methods*)
	val mutable var_stack = Hashtbl.create(100)
	val mutable rval_stack = ([] : string list)

	method update varname value typ = match typ with 0 -> let change = varname^" stored with value "^value in stack_changes <- stack_changes@[change]
	| 1 -> let change = varname^" reassigned with value "^value in stack_changes <- stack_changes@[change]

	method addRval rval = rval_stack <- rval::rval_stack

	method checkRval variable = let rec checkh s = match s with [] -> false
	| head::tail -> if variable=head then true else checkh tail in checkh rval_stack

	method assign (data : string * 'a)  = match data with (a,b) -> (match self#checkRval a with false -> Hashtbl.add var_stack a b; self#update a b 0
		| true -> print_endline "Error: cannot make assignment to lvalue")

	method findOpt varname = try Some(Hashtbl.find var_stack varname) with Not_found -> None

  	method lookup varname = self#findOpt varname

    method reassign varname value = match self#checkRval varname with false -> let dummy = self#update varname value 1 
		in Hashtbl.replace var_stack varname value
		| true -> print_endline "Error: cannot make assignment to lvalue"

    method printChanges = let rec pCh s = match s with [] -> ()
    | a::rest -> print_endline a; pCh rest in pCh stack_changes

 end;;


