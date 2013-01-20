
(* Parameters: 
  - little/big endian
  - 32 or 64 bit
*)

(** Info type *)
type lineno = int
type info = lineno

(** Register number *)
type rd = int
type rs1 = int
type rs2 = int

(** Immediate value *)
type imm12

(** Load opcodes *)
type opload = OpLB | OpLH | OpLW | OpLD | OpLBU | OpLHU | OpLWU
type opstore = OpSB | OpSH | OpSW | OpSD

type inst = 
| ILoad of info * rd * rs1 * imm12 * opload    (* Load Memory Instructions *)
| IStore of info * rs1 * rs2 * imm12 * opstore (* Store Memory Instructions *)


let testlet = 6

