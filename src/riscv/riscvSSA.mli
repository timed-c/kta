

open Ustring.Op


type label = sid

type var = TmpVar of sid | LlvmGVar of sid | LlvmLVar of sid | SinkVar
(** Variables marked [TmpVar] are created during instruction selection. 
    Variables marked [LlvmGVar] are original global variables from the LLVM IR.
    Variables marked [LlvmLVar] are original local variables from the LLVM IR.
    Variables marked [SinkVar] are used for assigning results that should not
    be used. For RISC-V, this will translate into x0. *)
    
type svar = var
type dvar = var
type immv = int

(** Instructions *)
type inst = 
| IAbsJmp   of RiscvISA.opAbsJmp * label                        (* Absolute Jump *)
| ICondJmp  of RiscvISA.opCondJmp * svar * svar * label * label (* Conditional Jump  *)
| IIndJmp   of RiscvISA.opIndJmp * dvar * svar                  (* Indirect Jump *)
| ILoad     of RiscvISA.opLoad * dvar  * svar * immv            (* Load Memory *)
| IStore    of RiscvISA.opStore * svar * svar * immv            (* Store Memory *)
| IAtomic   of RiscvISA.opAtomic * dvar * svar * svar           (* Atomic Memory *) 
| ICompImm  of RiscvISA.opCompImm * dvar * svar * immv          (* Integer Register- 
                                                                   Immediate Computation *)
| ICompReg  of RiscvISA.opCompReg * dvar * svar * svar          (* Integer Register-
                                                                   Register Computation *)
| IMiscMem  of RiscvISA.opMiscMem * dvar * svar * immv          (* Misc memory inst *)
| ISys      of RiscvISA.opSys * dvar                            (* System instructions *)


type blocktype = 
| TempBlock 
| LlvmBlock

type phi = Phi of
  dvar * 
  ((label * svar) list)

type block = Block of
  blocktype * 
  phi list *   
  inst list  

type func = Func of       (* Function *)
  var list *                (* Parameter variable list *)
  (label * block) list      (* Labeled blocks *)

type cmodule = Module of  (* Code Module *)
  (label * func) list
    



(* Notes
   - A label must at most appear once in the false branch, so that code generators 
     can put these basic blocks directly after each other. This means that we might need
     to introduce a new basic block with an unconditional branch. 
   - Indirect jumps are right now just used for return statement.
*)
