

open LlvmAst
open Ustring.Op

exception Function_not_found of string * sid
exception Illegal_llvm_code

val const_int_val : int -> int -> llExp 
(** Expression [const_int_val w v] creates a constant integer value with bit
    width [w] and value [v]. *)


val get_fun : llModule -> llGloId -> llFunc
(** Expression [get_fun m f] returns the function named [f] from module [m]. 
    Raises exception [Function_not_found] if the function cannot be found. *)


val type_of_val : llExp -> llType 
(** Returns the type of a specific value *)


val const32 : int -> llConst
(** Creates a constant 32 bit integer value *)


val sign_ext_int64 : int64 -> int -> int64
(** [sign_ext_int64 v n] sign extends a value [v] assumed to 
    hold [n] number of bits. *)

 
val mask_int64 : int64 -> int -> int64
(** [mask_int64 v n] keeps the [n] least significant bits of value [v] and
    sets the rest of the bits to 0 *)

val default_constval : llType -> llConst
(** Returns the default constant value for a specific type, if it exists *)
