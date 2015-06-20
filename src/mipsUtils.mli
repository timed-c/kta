
open Ustring.Op



val decode : ?bigendian:bool -> bytes -> MipsAst.inst list
(** [decode b] decodes a sequence of instructions, stored in the byte
    array [b]. The length of [b] must be a multiple of 4 bytes. The
    function returns a list of MIPS assembly instructions. An optional
    parameter [bigendian] can be set to true if the input data should
    be decoded as big-endian. Default is little-endian. Raises exception
    [Invalid_argument] if [b] is not a multiple of 4 bytes. *)


val pprint_reg : int -> ustring
(** [pprint_reg regno] returns the ustring representation of register number
    [regno] *)

val pprint_inst : MipsAst.inst -> ustring
(** [pprint_inst i] pretty prints one instruction *)


val pprint_inst_list : MipsAst.inst list -> ustring
(** [pprint_inst lst] pretty prints the list [lst] of instructions  *)

val pprint_asm : MipsAst.program -> int -> int -> bool -> ustring
(** [pprint_asm prog addre len prnaddr] pretty prints program [prog],
    starting at address [addr] and prints [len] length of program code
    (in bytes). If boolean parameter [prnaddr] is true, the address of
    each instruction is also printed. *)
