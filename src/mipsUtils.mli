
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

val pprint_inst : MipsAst.inst ->  ustring
(** [pprint_inst inst ] pretty prints one instruction [inst]. *)

val pprint_inst_ocaml : MipsAst.inst ->  ustring
(** [pprint_inst_ocaml inst ] pretty prints one instruction [inst]
    using the ocaml syntax used in wcet analysis code. *)
  

val pprint_inst_list :  MipsAst.inst list -> ustring
(** [pprint_inst lst] pretty prints the list [lst] of
    instructions *)

val pprint_inst_ext : MipsAst.inst -> MipsAst.program -> int -> bool -> ustring
(* [pprint_inst_ext inst prog addr print_addr] pretty prints an instruction
   together with labels etc. *)

val pprint_asm : MipsAst.program -> int -> int -> bool -> ustring
(** [pprint_asm prog addre len prnaddr] pretty prints program [prog],
    starting at address [addr] and prints [len] length of program code
    (in bytes). If boolean parameter [prnaddr] is true, the address of
    each instruction is also printed. *)


val add_branch_symbols : MipsAst.program -> MipsAst.program
(** [add_branch_symbols prog] creates branch symbols for all BEQ, BNE,
    J, and JAL instructions and adds them to the fields sym2addr and
    add2sym in the returned program. The string labels are also added to
    the instructions in fields 'code' field of the program.
*)

val get_16_bits : bool -> bytes -> int -> int32 
(** [get_16_bits bigendian b i] reads out a 16-bit value
    from byte array [b] at index [i]. *)

val set_16_bits : bool -> bytes -> int -> int32 -> unit
(** [set_16_bits bigendian b i v] writes 16-bit value [v]
    to a byte array [b] at index [i]. *)

val get_32_bits : bool -> bytes -> int -> int32 
(** [get_32_bits bigendian b i] reads out a 32-bit value
    from byte array [b] at index [i]. *)

val set_32_bits : bool -> bytes -> int -> int32 -> unit
(** [set_32_bits bigendian b i v] writes 32-bit value [v]
    to a byte array [b] at index [i]. *)


val pprint_bytes : bytes -> int -> int -> int -> bool -> ustring
(** [pprint_bytes b i len addr bigendian] pretty prints [len] number of bytes from
    byte array [b], starting at index [i], assuming that the first address is [addr] *)



  





