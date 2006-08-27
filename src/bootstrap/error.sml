structure Error =
struct
   (* error message *)
   exception InternalError of string

   (* source file * line no * column no * error message *)
   exception ParseError of string * int * int * string

   (* source file * line no * column no * error message *)
   exception TokenizeError of string * int * int * string

(* Not sure if I want this stuff down below, but for now we'll keep the strings
 * around for reference.
 *)
(*
   fun parseError (filename, line, col, msg) =
      ( print (filename ^ ":" ^ Int.toString line ^ "." ^ Int.toString col ^
               "Error: Parse error on input file.\n\t" ^ msg ^ "\n") ;
        OS.Process.exit 1 )
*)
end
