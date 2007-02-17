structure Error =
struct
   (* error message *)
   exception InternalError of string

   (* source file * position * error message *)
   exception ParseError of string * StreamPos.pos * string

   (* position * error message *)
   exception TokenizeError of StreamPos.pos * string
end
