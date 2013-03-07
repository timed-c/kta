

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
  (int_of_char (s.[p]), BS(s,p+1,0,0))
  


(** Decodes the bitcode header (if present) as well as the
    bitstream magic number. All data is checked for consitency *)
let decode_header bs =
  let (b0, bs) = dByte bs in 
  let (b1, bs) = dByte bs in
  if (b0,b1) = (0xde,0xc0) then (* includes header *)
    let (b2, bs) = dByte bs in
    let (b3, bs) = dByte bs in
    if (b2,b3) <> (0x17,0x0b) then error_header() else
    bs
  else if (b0,b1) = (0x42,0x43) then bs (* Only bitsteam magic *)
  else error_header()
    


(**************** Exported functions *******************************)



let decode data =
  let bs = BS(data,0,0,0) in
  let BS(s,p,d,b) = decode_header bs in
  printf "Length %d,%d, p=%d\n" (String.length data) Sys.word_size p
