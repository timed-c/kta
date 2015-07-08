

type lower = int
type upper = int
type aint32 = (lower * upper) list

val int32max : int
  
val int32min : int
  
val abstractval : aint32
  
val make : int -> aint32
(** Make a new abstract integer from a concrete integer *)
  
val make_intervals : (lower * upper) list -> aint32
(** Make a new abstract integer from a list of integer intervals *)



 
  
  
