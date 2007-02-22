structure Error =
struct
   (* error message *)
   exception InternalError of string

   (* source file * position * error message *)
   exception ParseError of string * StreamPos.pos * string

   (* position * error message *)
   exception TokenizeError of StreamPos.pos * string

   (* A function to kill the compiler.  failure is a boolean for whether this
    * is an error case or not.  This function mainly exists to easily turn off
    * during development when we don't want to kill sml.
    *)
   fun quit failure = raise (Fail "mitchell encountered an error, dropping back to sml")
(*
   fun quit failure = if failure then OS.Process.exit OS.failure
                      else OS.Process.exit OS.success
*)
end
