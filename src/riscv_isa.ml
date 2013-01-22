
(* Parameters: 
  - little/big endian
  - 32 or 64 bit
*)

(** Info type *)
type lineno = int
type info = 
 | NoI 
 | Info of lineno

(** Types for making the inst data type more descriptive *)
type rd  = int
type rs1 = int
type rs2 = int
type imm12 = int
type offset25 = int

(** Opcodes *)
type opAbsJmp  = OpJ  | OpJAL
type opCondJmp = OpBEQ | OpBNE | OpBLT | OpBGE | OpBLTU | OpBGEU
type opIndJmp  = OpJALR_C | OpJALR_R | OpJALR_J | OpRDNPC
type opLoad    = OpLB | OpLH | OpLW | OpLD | OpLBU | OpLHU | OpLWU
type opStore   = OpSB | OpSH | OpSW | OpSD
type opAtomic  = OpAMOADD_W  | OpAMOSWAP_W | OpAMOAND_W   | OpAMOOR_W | 
                 OpAMOMIN_W  | OpAMOMAX_W  | OpAMOMINU_W  | OpAMOMAXU_W | 
                 OpAMOADD_D  | OpAMOSWAP_D | OpAMOAND_D   | OpAMOOR_D | 
                 OpAMOMIN_D  | OpAMOMAX_D  | OpAMOMINU_D  | OpAMOMAXU_D  

(** Instruction types *)
type inst = 
| IAbsJmp  of info * offset25  * opAbsJmp          (* Absolute Jump Instructions *)
| ICondJmp of info * rs1 * rs2 * imm12 * opCondJmp (* Conditional Jump Instructions *)
| IIndJmp  of info * rd  * rs1 * imm12 * opIndJmp  (* Indirect Jump Instructions *)
| ILoad    of info * rd  * rs1 * imm12 * opLoad    (* Load Memory Instructions *)
| IStore   of info * rs1 * rs2 * imm12 * opStore   (* Store Memory Instructions *)
| IAtomic  of info * rd  * rs1 * rs2   * opAtomic  (* Atomic Memory Instructions *) 

