structure BaseTy = struct
   (* The representation of a valid mitchell string (or identifier, etc.)
    * within the compiler.
    *)
   type mstring = UniChar.Data

   val mstringToString = UniChar.Data2String
end
