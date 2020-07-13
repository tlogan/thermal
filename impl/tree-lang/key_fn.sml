functor Key_Fn (val tag : string) :
sig
  include ORD_KEY

  val zero : ord_key  

  val inc : ord_key -> ord_key   
  
  val to_string : ord_key -> string   

end =
struct

  datatype ord_key = Key of int

  fun compare (Key a, Key b) =
  (if a < b then
    LESS
  else if a = b then
    EQUAL
  else
    GREATER
  )

  val zero = Key 0

  fun inc (Key i) = Key (i + 1)

  fun to_string (Key i) = tag ^ "_" ^ (Int.toString i)

end