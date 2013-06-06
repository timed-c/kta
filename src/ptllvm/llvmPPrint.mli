

open Ustring.Op
open LlvmAst


val pprint_binop : llBinOp -> ustring
(** Pretty print binary operator *)

val pprint_module : llModule -> ustring
(** Pretty print a module *)
