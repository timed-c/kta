

open Ustring.Op
open MlvmAst

val llid : llId -> ustring 
(** Pretty print an llId *)

val lltype : llType -> ustring
(** Pretty print an LLVM type *)

val llconst : llConst -> ustring
(** Pretty print a constant *)

val llexp : llExp -> ustring
(** Pretty print an expression *)

val llbinop : llBinOp -> ustring
(** Pretty print a binary operator *)

val llicmp_pred_op : llIcmpPred -> ustring
(** Pretty printing icmp predicate operator *)

val llinst : llInst -> ustring
(** Pretty print an instruction *)

val llblock : llBlock -> ustring
(** Pretty print a basic block *)

val llfunc : llFunc -> ustring
(** Pretty print an anonymous function *)

val llnamed_func : ustring -> llFunc -> ustring
(** [llnamed_func n f] pretty prints a named function [f] with name [n] *)

val llmodule : llModule -> ustring
(** Pretty print a module. *)

val lltree : MlvmTree.tree -> ustring
(** Pretty print an LLVM SSA tree *)

val llforest : MlvmTree.tree list -> ustring
(** Pretty print an LLVM SSA forest (a list of trees) *)



