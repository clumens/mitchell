structure BaseTy = struct
   (* The representation of a valid mitchell string (or identifier, etc.)
    * within the compiler.
    *)
   type mstring = UTF8.wchar list

   val mstringToString = UTF8.toString
end
