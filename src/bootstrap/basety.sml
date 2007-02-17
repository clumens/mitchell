structure BaseTy = struct
   (* The representation of a valid mitchell string (or identifier, etc.)
    * within the compiler.
    *)
   type mstring = UTF8.wchar list

   (* Convert a mitchell string into an ML string that can be printed. *)
   fun mstringToString str =
      String.concat (map UTF8.toString str)
end
