

open LlvmAst
open Ustring.Op
open Utils

exception Function_not_found of string * sid
exception Illegal_llvm_code of string

val const_int_val : int -> int -> llExp 
(** Expression [const_int_val w v] creates a constant integer value with bit
    width [w] and value [v]. *)


val get_fun : llModule -> llGloId -> llFunc
(** Expression [get_fun m f] returns the function named [f] from module [m]. 
    Raises exception [Function_not_found] if the function cannot be found. *)


val type_of_exp : llExp -> llType 
(** Returns the type of a specific expression *)


val sign_ext_int64 : int64 -> int -> int64
(** [sign_ext_int64 v n] sign extends a value [v] assumed to 
    hold [n] number of bits. *)

 
val mask_int64 : int64 -> int -> int64
(** [mask_int64 v n] keeps the [n] least significant bits of value [v] and
    sets the rest of the bits to 0 *)


val used_in_another_block : llFunc -> llabelset
(** [define_in_another_block f] takes a function [f] and returns the 
    set of labels that used in other basic blocks than they are defined
    in. *)

