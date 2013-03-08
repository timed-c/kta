

open Printf
open Ustring.Op


(*************** Exported types and exceptions ********************)

exception Bitcode_error of string


(*************** Local types and exceptions ***********************)


type strpos = int       (* Position in the binary string *)
type decoded = int      (* Decoded but not used bits *)
type decoded_bits = int (* Number of decoded bits *)
type bitstream = BS of string * strpos * decoded * decoded_bits 


(*************** Local functions **********************************)

(* Functions raising various of decoding errors. *)
let error_header() = raise (Bitcode_error "Illegal LLVM bitcode header.")
let error_align() = raise (Bitcode_error "Illegal alignment in the LLVM bitcode data.")
let error_size() = raise (Bitcode_error "Illegal size of LLVM bitcode data.")
let error_version v = raise (Bitcode_error (sprintf "LLVM Bitcode version %d is not supported." v))


(* Get an int value out of a string array *)
let s2int s i = int_of_char (s.[i])

(* Returns the current bit position in the stream *)
let bit_pos (BS(_,p,_,b)) = p * 8 - b 

(* Check that [size] number of bytes are possible to decode *) 
let check_byte_size s p size =  
  if p + size <= String.length s then () else error_size()

(* Check that the stream is byte aligned *)
let check_alignment b =  
  if b != 0 then error_align() else ()

(* Decode one byte that is assumed to be byte aligned. *)
let dByte (BS(s,p,d,b)) =
  check_alignment b;
  check_byte_size s p 1;
  (s2int s p, BS(s,p+1,0,0))
  
(* Decode a 32-bits word in little-endian format*)
let dWord32 (BS(s,p,d,b)) =
  check_alignment b;
  check_byte_size s p 4;
  let v = (s2int s p) lor ((s2int s (p+1)) lsl 8) lor 
          ((s2int s (p+2)) lsl 16) lor  ((s2int s (p+3)) lsl 24) in
  (v, BS(s,p+4,0,0))
  


(** Decodes the bitcode header (if present) as well as the
    bitstream magic number. All data is checked for consistency *)
let decode_header bs =
  let (b0, bs) = dByte bs in 
  let (b1, bs) = dByte bs in
  if (b0,b1) = (0xde,0xc0) then (* Includes header *)
    let (b2, bs) = dByte bs in
    let (b3, bs) = dByte bs in
    if (b2,b3) <> (0x17,0x0b) then error_header() else
    let (version,bs) = dWord32 bs in
    if version != 0 then error_version version else
    let (offset,bs) = dWord32 bs in
    let (size,bs) = dWord32 bs in
    let (cputype,bs) = dWord32 bs in
    if bit_pos bs != offset * 8 then error_header() else
    let BS(s,_,_,_) = bs in                                      (* TEMP *)
    printf "Header: offset=%d, size_info=%d, string_size=%d\n"   (* TEMP *)
        offset size (String.length s);                           (* TEMP *)
    bs
  else if (b0,b1) = (0x42,0x43) then bs (* Only bitstream magic *)
  else error_header()
    


(**************** Exported functions *******************************)


let decode data =
  let bs = BS(data,0,0,0) in
  let BS(s,p,d,b) = decode_header bs in
  printf "String length length=%d, end_pos=%d\n" (String.length data) p








