

open Ustring.Op

val minst : int -> RiscvISA.minst -> ustring
(** Pretty print an RISC-V machine instruction. Expression 
    [RiscvPPrint.minst a i] returns a string representing
    instruction [i] if the instruction starts at address 
    [a] in memory. 
    *)


val sinst : RiscvISA.sinst -> ustring
(** Pretty print a SSA RISC-V instruction *)


val sinst_list : RiscvISA.sinst list -> ustring
(** Pretty print a list of SSA RISC-V instructions *)
