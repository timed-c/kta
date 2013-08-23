

open Ustring.Op
open LlvmAst

val llid : llId -> ustring 
(** Pretty print llId *)

val lltype : llType -> ustring
(** Pretty print llvm type *)

val llconst : llConst -> ustring
(** Pretty print constant *)

val llexp : llExp -> ustring
(** Pretty print expression *)

val llbinop : llBinOp -> ustring
(** Pretty print binary operator *)

val llicmp_pred_op : llIcmpPred -> ustring
(** Pretty printing icmp predicate operator *)

val llinst : llInst -> ustring
(** Pretty print an instruction *)

val llmodule : llModule -> ustring
(** Pretty print a module. *)

val lltree : LlvmTree.tree -> ustring
(** Pretty print an LLVM SSA tree *)

val llforest : LlvmTree.tree list -> ustring
(** Pretty print an LLVM SSA forest (a list of trees) *)



