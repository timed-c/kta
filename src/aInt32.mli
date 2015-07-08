
open Ustring.Op

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


val pprint : aint32 -> ustring
(** [pprint v] returns the pretty printed string representation of the
    abstract value [v] *)

  
val add : aint32 -> aint32 -> aint32
(** [add x y] performs addition of the abstract values [x] and [y] *)
  


 
  
  
