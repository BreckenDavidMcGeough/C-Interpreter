
module Swi = struct 

   exception PastEnd


   let symb = "+-/*<>=!@#$%^&`~|?:"
   let alph = "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ"
   let digit = "0123456789"
   let num = digit^"."
   let alphanum = alph^num^"_'"
   let whitespace = " \t\n\r"

   (* These are like static methods *)
   let isSymbol c = String.contains_from symb 0 c
   let isAlpha c = String.contains_from alph 0 c
   let isDigit c =  String.contains_from digit 0 c
   let isNum c = String.contains_from num 0 c
   let isAlphanum c = String.contains_from alphanum 0 c
   let isWhitespace c = c = ' ' || c = '\n' || c = '\r' || c = '\t'
   let isPrintedChar c  = ' ' < c && c <= '}'

   let floatOpt str = try Some (float_of_string str) with Failure x -> None;;
   let intOpt str = try Some (int_of_string str) with Failure x -> None;;

   let endsWith(str,chr) = let n = String.length str
                           in if n = 0 then false else str.[n-1] = chr




   let rec swi gstr = object(self)
      val str = gstr
      val mutable i = 0
      val n = String.length(gstr)

      method geti = i
      method seti j = i <- j
      method pastEnd = i >= n
      method getc = if self#pastEnd then raise PastEnd else String.get str i;
      method nextc = if self#pastEnd
                     then raise PastEnd 
                     else let c = self#getc in i <- i+1; c
      
      (** Advance iterator to the next non-whitespace character (NWC). Return that char.
       *)
      method gotoFirstNWC = while isWhitespace self#getc do i <- i+1 done; self#getc


      method peekFirstNWC = let clone = {< >} in clone#seti i; clone#gotoFirstNWC

      (* May not need the rest since I/O is separate from lexing now *)
      method getItem isSkipChar isGoodChar =
         while isSkipChar self#getc do i <- i+1 done;
         let rec getGoodChars acc = if self#pastEnd then acc else let c = self#getc in
            if isGoodChar c
               then (i <- i+1; getGoodChars (acc^(String.make 1 c)))
               else acc
         in getGoodChars ""

      method getString = self#getItem isWhitespace isPrintedChar
      method getLine = self#getItem isWhitespace (function c -> c != '\n')



   end
end;;

