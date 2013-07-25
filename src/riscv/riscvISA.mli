
(* Parameters: 
  - little/big endian
  - 32 or 64 bit
*)


open Ustring.Op


(** Info type *)
type lineno = int   (* Line number if read from asm file, else 0. *)
type filename = sid (* File name if known, else empty string. *)
type instsize = int (* Number of bytes of the instruction, if known, else 9 *)
type info = Info of lineno * filename * instsize 
 

(* Pure map from addresses to string identifies. Addresses uses normal
   int, making it possible to compile 2^31 bytes of code on a 32-bits machine
   and 2^63 bytes of code on a 64 bit machine. *)
(* module type IntMap = Map.Make(struct type t = int let compare = compare end)  *)

(* type addr2sid = sid IntMap.t *)

(** Types for making the inst data type more descriptive *)
type rd  = int
type rs1 = int
type rs2 = int
type rs3 = int
type imm12 = int  (* 12 bit immediate *)
type imm25 = int  (* 25 bit immediate *)
type immv = int   (* variable numbers of bits depending on instruction 
                     SLLI, SRAI, SRLI have 5 bit, 
                     SLLIW, SRAIW, and SRLIW have 4 bit,
                     LUI has 20 bits. Note that rs1 is unused for LUI
                     other instructions have 12 bits *)


(** Opcodes *)
type opAbsJmp     = OpJ  | OpJAL
type opCondJmp    = OpBEQ | OpBNE | OpBLT | OpBGE | OpBLTU | OpBGEU
type opIndJmp     = OpJALR_C | OpJALR_R | OpJALR_J | OpRDNPC
type opLoad       = OpLB | OpLH | OpLW | OpLD | OpLBU | OpLHU | OpLWU
type opStore      = OpSB | OpSH | OpSW | OpSD
type opAtomic     = OpAMOADD_W  | OpAMOSWAP_W | OpAMOAND_W   | OpAMOOR_W | 
                    OpAMOMIN_W  | OpAMOMAX_W  | OpAMOMINU_W  | OpAMOMAXU_W | 
                    OpAMOADD_D  | OpAMOSWAP_D | OpAMOAND_D   | OpAMOOR_D | 
                    OpAMOMIN_D  | OpAMOMAX_D  | OpAMOMINU_D  | OpAMOMAXU_D 
type opCompImm    = OpADDI  | OpSLLI  | OpSLTI   | OpSLTIU | OpXORI | 
                    OpSRLI  | OpSRAI  | OpORI    | OpANDI  | OpLUI  |
                    OpADDIW | OpSLLIW | OpSRLIW  | OpSRAIW
type opCompReg    = OpADD   | OpSUB   | OpSLL    | OpSLT   | OpSLTU |
                    OpXOR   | OpSRL   | OpSRA    | OpOR    | OpAND  |
                    OpMUL   | OpMULH  | OpMULHSU | OpMULHU | OpDIV  |
                    OpDIVU  | OpREM   | OpREMU   | OpADDW  | OpSUBW |
                    OpSLLW  | OpSRLW  | OpSRAW   | OpMULW  | OpDIVW |
                    OpDIVUW | OpREMW  | OpREMUW
type opMiscMem    = OpFENCE_I   | OpFENCE
type opSys        = OpSYSCALL   | OpBREAK     | OpRDCYCLE  | OpRDTIME |
                    OpRDINSTRET

(** Instructions *)
type inst = 
| IAbsJmp     of info * opAbsJmp * imm25                 (* Absolute Jump *)
| ICondJmp    of info * opCondJmp * rs1 * rs2 * imm12    (* Conditional Jump  *)
| IIndJmp     of info * opIndJmp * rd * rs1 * imm12      (* Indirect Jump *)
| ILoad       of info * opLoad * rd  * rs1 * imm12       (* Load Memory *)
| IStore      of info * opStore * rs1 * rs2 * imm12      (* Store Memory *)
| IAtomic     of info * opAtomic * rd  * rs1 * rs2       (* Atomic Memory *) 
| ICompImm    of info * opCompImm * rd  * rs1 * immv     (* Integer Register-Immediate Computation *)
| ICompReg    of info * opCompReg * rd  * rs1 * rs2      (* Integer Register-Register Computation *)
| IMiscMem    of info * opMiscMem * rd  * rs1 * imm12    (* Misc memory instructions *)
| ISys        of info * opSys * rd                       (* System instructions *)

(*
type addr = int
type label = sid

type block = inst list
type funct = (label * (block * addr) list
type riscvModule = (label * (funct * addr)) list 
*)

(*


(** Is the integer register-immediate opcode a 32 bit instruction used only in RV64? *)
let isIntImReg32 op = match op with
   OpADDIW | OpSLLIW | OpSRLIW | OpSRAIW -> true | _ -> false

(** Is the integer register-register opcode a 32 bit instruction used only in RV64? *)
let isIntRegReg32 op = match op with
   OpADDW | OpSUBW | OpSLLW | OpSRLW | OpSRAW | OpMULW | OpDIVW | OpDIVUW |
   OpREMW | OpREMUW -> true | _ -> false


*)
