structure Error =
struct
   (* error message *)
   exception InternalError of string

   (* source file * line no * column no * error message *)
   exception ParseError of string * int * int * string

   (* source file * line no * column no * error message *)
   exception TokenizeError of string * int * int * string
end
