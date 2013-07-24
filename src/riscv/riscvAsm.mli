

open Ustring.Op

type big_endian = bool

(* Parse a RISC-V assembly text string *)
val parse : ustring -> RiscvISA.inst list

val sprint_inst : int -> RiscvISA.inst -> ustring
(** Pretty print an RISC-V instruction. Expression 
    [sprint_inst k i] creates a test string representing
    instruction [i] if the instruction starts at address 
    [a] in memory. 
    *)

(* val sprint_inst_conf : int -> RiscvISA.addr2sid -> int -> RiscvISA.inst -> ustring *)
(** Configurable version of [sprint_inst]. Expression
    [sprint_inst_conf n map k i] returns a string where space characters 
    are inserted such that [n] number of characters are between the start of 
    the instruction and its operands. The argument [map] is map between addresses 
    and symbolic names. Arguments for [k] and [i] are defined as for function
    sprint_inst *)
