

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


val defining_id_in_instruction : llInst -> llLocId option
(** Returns the defining local identifier of an instruction. If the
    instruction does not define an identifier, None is returned *)


val defining_ids_in_phi_list : llPhi list -> llLocId list
(** Returns the defining local identifiers for a list of Phi commands *) 


val defining_ids_in_inst_list : llInst list -> llLocId list 
(** Returns the defining local identifiers for a list of instructions *)


val ids_of_explst : llExp list -> llId list 
(** Returns the list of identifiers available in a list of expressions. Note
    that the returned identifiers may be both local and global *)

val using_explst_in_instruction : llInst -> llExp list 
(** Returns a list of expressions used in a specific instruction *)


val using_ids_in_phi : llPhi -> llId list
(** Returns a list of identifiers used in a Phi command *)

  
val using_ids_in_phi_list : llPhi list -> llId list
(** Returns a list of identifiers used in a list of Phi commands *)


val using_ids_in_inst_list : llInst list -> llId list
(** Returns a list of identifiers used in a list of instructions *)

  
val local_ids : llId list -> llLocId list
(** Returns the list of local identifiers from a list of identifiers *)


val global_ids : llId list -> llGloId list
(** Returns the list of global identifiers from a list of identifiers *)

  
val used_in_another_block : llFunc -> sidset
(** [used_in_another_block f] takes a function [f] and returns the set
    of local identifiers that are used in other basic blocks than the
    ones they are defined in. Note that phi commands are not seen as
    defining instructions. *)


val count_ids_uses : llInst list -> (llLocId * int) list
(** [count_ids_uses l] returns an association list (key/value mapping)
    where the keys are local identifiers used in the instruction list
    [l] and the values are the number of times these identifiers occur
    in the instruction sequence. *)








