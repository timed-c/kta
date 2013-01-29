


open Ustring.Op

type big_endian = bool

(* Parse a RISC-V assembly text string *)
val parse : ustring ->  RiscvISA.inst list

(* Pretty print a RISC-V assembly text string *)
val sprint : big_endian -> RiscvISA.inst list -> ustring

