
open Ustring.Op

exception Decode_error of string


val decode : ?bigendian:bool -> bytes -> MipsAst.inst list
(** [decode b] decodes a sequence of instructions, stored in the byte
    array [b]. The length of [b] must be a multiple of 4 bytes. The
    function returns a list of MIPS assembly instructions. An optional
    parameter [bigendian] can be set to true if the input data should
    be decoded as big-endian. Default is little-endian. Raises exception
    [Decode_error] if there is an error when decoding the data. *)

val pprint_inst : MipsAst.inst -> ustring
(** [pprint_inst i] pretty prints one instruction *)

val pprint_inst_list : MipsAst.inst list -> ustring
(** [pprint_inst lst] pretty prints the list [lst] of instructions  *)

