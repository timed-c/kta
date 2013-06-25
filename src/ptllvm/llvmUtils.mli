

open LlvmAst
open Ustring.Op

exception Function_not_found of string * sid
exception Illegal_llvm_code

val const_int_val : int -> int -> llVal 
(** Expression [const_int_val w v] creates a constant integer value with bit
    width [w] and value [v]. *)


val get_fun : llModule -> llGloId -> llFunc
(** Expression [get_fun m f] returns the function named [f] from module [m]. 
    Raises exception [Function_not_found] if the function cannot be found. *)


val type_of_val : llVal -> llType 
(** Returns the type of a specific value *)
