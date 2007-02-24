(* Create a specialized form of the RedBlackMap that's keyed on strings
 * and is parameterized over whatever type you want to store in it.
 *)
structure StringMap = RedBlackMapFn(struct type ord_key = string
                                           val compare = String.compare end)
