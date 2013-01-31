


type big_endian = bool
(** If true, big endian encoding, else little endian encoding. *)


exception Decode_error
(** Raised when not all bytes of an instruction is available or
    if index out-of-bound. *)

exception Unsupported_encoding
(** Raised when the encoding is using 16 bit or more than 32 bits. *)


val decode : big_endian -> string -> int -> RiscvISA.inst * int
(** Expression [decode b s i] decodes one instruction from a byte
    string [s] at index [i] (zero indexed). Decoding is using
    big-ending if [b] is true, else it uses little-endian. The
    function returns a tuple consisting of the decoded instruction and
    the new index in the string. If there is incomplete number of
    bytes to decode or if index out-of-bound, exception [Decode_error]
    is raised.  If the encoded instruction is not using 32 bit
    encoding, exception [Unsupported_encoding] is raised. *)

val decode_interval : big_endian -> string -> int -> int -> RiscvISA.inst list

val decode_all : big_endian -> string -> RiscvISA.inst list

val encode : big_endian -> RiscvISA.inst -> string

val encode_all : big_endian -> RiscvISA.inst list -> string


