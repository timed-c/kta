


open LlvmAst

type ssacfg = int

val construct : int -> ssacfg
(** Takes an LLVM function and creates a static single assignment
control flow graph (SSACFG). *)


