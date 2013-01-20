


type big_endian = bool
(** If true, big endian encoding, else little endian encoding. *)

type parcel = int
(** A parcel is a 16 bits value that represents one of more
    instructions. Only the lower 16 bits of the int is used. 
    See page 4 of the RISC-V specification for more information. *)

exception Incomplete_parcel
(** Thrown when not all bytes of an instruction is available *)

val parcels_of_bytes : big_endian -> int list -> parcel list
(** Expression [parcels_of_bytes b lst] converts a list [lst] of bytes 
    into a list of parcels. Each byte is represented as an integer, i.e., 
    only the lower 8-bits of each byte is used. Raises exception 
    [Incomplete_parcel] if not a whole parcel can be decoded. *)

val bytes_of_parcels : big_endian -> parcel list -> int list
(** Expression [bytes_of_parcels b lst] creates a list [lst] of bytes from 
    a list of parcels using big endian encoding, if [b] is true. *)

val decode : parcel list -> Riscv_isa.inst list

val encode : Riscv_isa.inst list -> parcel list


