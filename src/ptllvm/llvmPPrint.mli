

open Ustring.Op
open LlvmAst

val pprint_llid : llId -> ustring 
(** Pretty print llId *)

val pprint_type : llType -> ustring
(** Pretty print llvm type *)

val pprint_const : llConst -> ustring
(** Pretty print constant *)

val pprint_exp : llExp -> ustring
(** Pretty print expression *)

val pprint_binop : llBinOp -> ustring
(** Pretty print binary operator *)

val pprint_icmp_pred_op : llIcmpPred -> ustring
(** Pretty printing icmp predicate operator *)

val pp_inst : llInst -> ustring
(** Pretty print an instruction *)

val pprint_module : llModule -> ustring
(** Pretty print a module *)

val pp_tree : LlvmTree.tree -> ustring
(** Pretty print an LLVM SSA tree *)

val pp_forest : LlvmTree.tree list -> ustring
(** Pretty print an LLVM SSA forest (a list of trees) *)



