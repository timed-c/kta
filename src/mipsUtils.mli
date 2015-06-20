
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

val pprint_inst : bool-> MipsAst.inst ->  ustring
(** [pprint_inst readable inst ] pretty prints one instruction [inst]. If
    parameter [readable] is true, readable text elements such as labels
    will be used instead of addresses. For instance, beq will use a label.*)


val pprint_inst_list :  MipsAst.inst list -> bool -> ustring
(** [pprint_inst readable lst] pretty prints the list [lst] of
    instructions *)

val pprint_asm : MipsAst.program -> int -> int -> bool -> bool -> ustring
(** [pprint_asm prog addre len prnaddr readable] pretty prints program [prog],
    starting at address [addr] and prints [len] length of program code
    (in bytes). If boolean parameter [prnaddr] is true, the address of
    each instruction is also printed. *)

val add_branch_symbols : MipsAst.program -> MipsAst.program
(** [add_branch_symbols prog] creates branch symbols for all BEQ and BNE instructions
 and adds them to the fields sym2addr and add2sym in the returned program. The
 string labels are also added to the instructions in fields 'code' field of the program.
*)
