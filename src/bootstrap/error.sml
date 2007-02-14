structure Error =
struct
   (* error message *)
   exception InternalError of string

   (* source file * position * error message *)
   exception ParseError of string * StreamPos.pos * string

   (* source file * position * error message *)
   exception TokenizeError of string * StreamPos.pos * string
end
