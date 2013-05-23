

open Ustring.Op

exception Bitcode_error of string
(** Exception raised if any error occurs when decoding a bitcode
    stream *)

type blockid = int 
(** Each block in the bitcode structure has an identifier *)

type bcblock =
| BcBlock  of blockid * bcblock list
| BcRecord of int list 
(** Data type representing a block-structured bitcode stream. The data
    is organized in nested block.  The actual data is stored in what
    is in the LLVM bitcode documentation records, each consisting of a
    list of integers.  The meaning of the block identifiers and the
    block records are defined in the LLVM specification (and
    implicitly in the LLVM source code). *)
    
val decode : string -> bcblock list
(** Expression [decode s] decodes a bitstream represented as a string
    byte array.  The function returns a list of [bcblock] (bitcode
    blocks). If any errors occur during decoding, exception
    [Bitcode_error s] is raised, where [s] is a string describing the
    error. *)


val debug_sprint : bcblock list -> ustring
(** Expression [debug_sprint lst] creates a unicode string
    representation of a bitcode block list [lst]. Useful for giving a
    raw debug output of a bitcode file. *)
