


open Ustring.Op

type big_endian = bool

(* Parse a RISC-V assembly text string *)
val parse : ustring ->  Riscv_isa.inst list

(* Pretty print a RISC-V assembly text string *)
val sprint : big_endian -> Riscv_isa.inst list -> ustring

