
open LlvmAst
open Ustring.Op

type tree = 
| TExp of llInst * tree list 
| TId of llId 
| TConst of llConst  

type forest = tree list
    


val make : llInst list -> sidset -> forest
(** [make il u] creates a forest (list of trees) from a instruction list
    [il]. Argument [u] is a set of string identifiers indicating if an
    identifier is used in another basic block. This set can be
    computed using function [LlvmUtils.used_in_another_block] *)

