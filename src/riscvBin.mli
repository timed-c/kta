

exception Decode_error
(** Raised when not all bytes of an instruction is available or
    if index out-of-bound. *)

exception Unsupported_encoding
(** Raised when the encoding is using 16 bit or more than 32 bits. *)


val decode : bool -> string -> int -> RiscvISA.inst * int
(** Expression [decode b s i] decodes one instruction from a byte
    string [s] at index [i] (zero indexed). Decoding is using
    big-ending if [b] is true, else it uses little-endian. The
    function returns a tuple consisting of the decoded instruction and
    the new index in the string. If there is incomplete number of
    bytes to decode or if index out-of-bound, exception [Decode_error]
    is raised.  If the encoded instruction is not using 32 bit
    encoding, exception [Unsupported_encoding] is raised. *)

val decode_interval : bool -> string -> int -> int -> RiscvISA.inst list
(** Expression [decode_interval b s pos len] decodes a string, starting
    at [pos] and decodes [len] number of characters. If wrong offset or
    length, exception [Invalid_argument] is raised. *)

val decode_all : bool -> string -> RiscvISA.inst list
(** Expression [decode_all b s] decodes string [s] using big-endian 
    if [b] is true. See also function [decode]. *)

val encode : bool -> RiscvISA.inst -> string
(** Expression [encode b i] encodes a 32-bit instruction [i]. The
    returned string is guaranteed to be 4 bytes long.  Encoding is
    using big-endian if [b] is true, else little-endian encoding. *)

val encode_all : bool -> RiscvISA.inst list -> string
(** Expression [encode_all b il] encodes a list of 32-bit instruction
    [il].  Encoding is using big-endian if [b] is true, else
    little-endian encoding. *)

