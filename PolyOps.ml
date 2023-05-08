
#use "Swi.ml";;

module PolyOps = struct

   exception BadArg of string

   (** Mixed int/float addition, type (string * string) -> string
    *)

   let polyPlus (lo, ro) = 
      let lio = Swi.intOpt lo and rio = Swi.intOpt ro 
      in match (lio, rio) with
         | (Some li, Some ri) -> string_of_int (li + ri)
         | (Some li, None) -> let rfo = Swi.floatOpt ro in
              (match rfo with
                 | Some rf -> string_of_float ((float_of_int li) +. rf)
                 | None -> raise (BadArg ro)
              )
         | (None, Some ri) -> let lfo = Swi.floatOpt lo in
              (match lfo with
                 | Some lf -> string_of_float (lf +. (float_of_int ri))
                 | None -> raise (BadArg lo)
              )
         | (None, None) -> let rfo = Swi.floatOpt ro and lfo = Swi.floatOpt lo in
              (match (lfo, rfo) with
                 | (Some lf, Some rf) -> string_of_float (lf +. rf)
                 | _ -> raise (BadArg (lo ^ "," ^ ro))
              )

   (** Mixed int/float subtraction, type (string * string) -> string
    *)
   let polyMinus (lo, ro) =
      let lio = Swi.intOpt lo and rio = Swi.intOpt ro
      in match (lio, rio) with
         | (Some li, Some ri) -> string_of_int (li - ri)
         | (Some li, None) -> let rfo = Swi.floatOpt ro in
              (match rfo with
                 | Some rf -> string_of_float ((float_of_int li) -. rf)
                 | None -> raise (BadArg ro)
              )
         | (None, Some ri) -> let lfo = Swi.floatOpt lo in
              (match lfo with
                 | Some lf -> string_of_float (lf -. (float_of_int ri))
                 | None -> raise (BadArg lo)
              )
         | (None, None) -> let rfo = Swi.floatOpt ro and lfo = Swi.floatOpt lo in
              (match (lfo, rfo) with
                 | (Some lf, Some rf) -> string_of_float (lf -. rf)
                 | _ -> raise (BadArg (lo ^ "," ^ ro))
              )

   (** Mixed int/float multiplication, type (string * string) -> string
    *)
   let polyTimes (lo, ro) =
      let lio = Swi.intOpt lo and rio = Swi.intOpt ro
      in match (lio, rio) with
         | (Some li, Some ri) -> string_of_int (li * ri)
         | (Some li, None) -> let rfo = Swi.floatOpt ro in
              (match rfo with
                 | Some rf -> string_of_float ((float_of_int li) *. rf)
                 | None -> raise (BadArg ro)
              )
         | (None, Some ri) -> let lfo = Swi.floatOpt lo in
              (match lfo with
                 | Some lf -> string_of_float (lf *. (float_of_int ri))
                 | None -> raise (BadArg lo)
              )
         | (None, None) -> let rfo = Swi.floatOpt ro and lfo = Swi.floatOpt lo in
              (match (lfo, rfo) with
                 | (Some lf, Some rf) -> string_of_float (lf *. rf)
                 | _ -> raise (BadArg (lo ^ "," ^ ro))
              )

   (** Mixed int/float division, type (string * string) -> string
       Does integer division when both arguments are ints
    *)
   let polyDiv (lo, ro) =
      let lio = Swi.intOpt lo and rio = Swi.intOpt ro
      in match (lio, rio) with
         | (Some li, Some ri) -> string_of_int (li / ri)
         | (Some li, None) -> let rfo = Swi.floatOpt ro in
              (match rfo with
                 | Some rf -> string_of_float ((float_of_int li) /. rf)
                 | None -> raise (BadArg ro)
              )
         | (None, Some ri) -> let lfo = Swi.floatOpt lo in
              (match lfo with
                 | Some lf -> string_of_float (lf /. (float_of_int ri))
                 | None -> raise (BadArg lo)
              )
         | (None, None) -> let rfo = Swi.floatOpt ro and lfo = Swi.floatOpt lo in
              (match (lfo, rfo) with
                 | (Some lf, Some rf) -> string_of_float (lf /. rf)
                 | _ -> raise (BadArg (lo ^ "," ^ ro))
              )

   (** Mixed int/float division, type (string * string) -> string
       Does integer division when both arguments are ints
    *)
   let polyFDiv (lo, ro) =
      let lio = Swi.intOpt lo and rio = Swi.intOpt ro
      in match (lio, rio) with
         | (Some li, Some ri) -> string_of_float ((float_of_int li) /. (float_of_int ri))
         | (Some li, None) -> let rfo = Swi.floatOpt ro in
              (match rfo with
                 | Some rf -> string_of_float ((float_of_int li) /. rf)
                 | None -> raise (BadArg ro)
              )
         | (None, Some ri) -> let lfo = Swi.floatOpt lo in
              (match lfo with
                 | Some lf -> string_of_float (lf /. (float_of_int ri))
                 | None -> raise (BadArg lo)
              )
         | (None, None) -> let rfo = Swi.floatOpt ro and lfo = Swi.floatOpt lo in
              (match (lfo, rfo) with
                 | (Some lf, Some rf) -> string_of_float (lf /. rf)
                 | _ -> raise (BadArg (lo ^ "," ^ ro))
              )






end;;

