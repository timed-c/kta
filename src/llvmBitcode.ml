

open Printf
open Ustring.Op


(*************** Exported types and exceptions ********************)

exception Bitcode_error of string


(*************** Local types and exceptions ***********************)


type strpos = int       (* Position in the binary string *)
type decoded = int      (* Decoded but not used bits *)
type decoded_bits = int (* Number of decoded bits *)
type bitstream = BS of string * strpos * decoded * decoded_bits 

(* Used in defining operands of abbreviations *)
type abbrevOp =
| AbbrevOpLiteral of int 
| AbbrevOpFixed of int 
| AbbrevOpVBR of int  
| AbbrevOpArray
| AbbrevOpChar6
| AbbrevOpBlob


(*************** Local functions **********************************)

let intsize = Sys.word_size - 1

(* Functions raising various of decoding errors. *)
let error_header() = raise (Bitcode_error "Illegal LLVM bitcode header.")
let error_align() = raise (Bitcode_error "Illegal alignment in the LLVM bitcode data.")
let error_size() = raise (Bitcode_error "Illegal size of LLVM bitcode data.")
let error_version v = raise (Bitcode_error (sprintf "LLVM Bitcode version %d is not supported." v))
let error_intsize() = raise (Bitcode_error 
         (sprintf "The machine's word size of %d bits is not large enough." Sys.word_size))
let error_unknown_abbrevcode v = raise (Bitcode_error 
         (sprintf "Unknown encoding of abbrev definition. Code = %d." v))


let print_bs txt bs = 
  let BS(s,p,d,b) = bs in
  printf "** '%s' p=%d d=%d b=%d " txt p d b;
  uprint_endline (us"[" ^. Ustring.string2hex (String.sub s p 10) ^. us"]")
                                                   
(* Return an integer with [n] bits set *)
let ones n = (1 lsl n) - 1 

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

(* Checks invariants of the bit stream. Can be removed in production code *)
let check_invariant bs =
  let BS(s,p,d,b) = bs in
  if b > 7 then failwith "Invariant violated. b is not <= 7" else ()


(* Decode [n] number of bytes that are assumed to be byte aligned. *)
let decodeBytes bs n =
  let BS(s,p,d,b) = bs in
  check_alignment b;
  check_byte_size s p n;
  (String.sub s p n, BS(s,p+n,0,0))
  
(* Decode a 32-bits word in little-endian format*)
let decodeWord32 bs =
  let BS(s,p,d,b) = bs in
  check_alignment b;
  check_byte_size s p 4;
  let v = (s2int s p) lor ((s2int s (p+1)) lsl 8) lor 
          ((s2int s (p+2)) lsl 16) lor  ((s2int s (p+3)) lsl 24) in
  (v, BS(s,p+4,0,0))

(* Decode a Fixed width Integer of bit size [n] from a bit stream [bs].
   Returns a tuple (v,bs'), where [v] is the decoded integer value
   and [bs'] the updated bit stream. *)
let decodeFixedInt bs n = 
  check_invariant bs;
  let BS(s,p,d,b) = bs in
  let rec decode p d b =
    if b > intsize then error_intsize() 
    else if b >= n then 
      let v =  d land (ones n) in
      (v, BS(s,p,d lsr n, b-n))
    else(
      check_byte_size s p 1;
      let d' = ((s2int s p) lsl b) lor d in
      decode (p + 1) d' (b+8))
  in decode p d b

(* Decode Variable width Integers (VBR), according to LLVM Bitcode spec. 
   Decode from bit stream [bs] using [n] bits chucks. The function 
   returns a tuple (v,bs'), where [v] is the decoded integer value
   and [bs'] the updated bit stream.  *)
let decodeVBR bs n =
  check_invariant bs;
  let checkbit = 1 lsl (n-1) in
  let maskbits = ones (n-1) in
  let rec decLoop bs acc b =
    if b > intsize then error_intsize() else    
    let (v,bs) = decodeFixedInt bs n in
    let acc = ((v land maskbits) lsl b) lor acc in
    if (checkbit land v) != 0 then 
      decLoop bs acc (b+n-1)
    else 
      (acc,bs)    
  in decLoop bs 0 0
  
(* Skip bits so that the bit stream points to a multiple of 32 bits. *)
let decodeAlign32 bs =
  check_invariant bs;
  let BS(s,p,d,b) = bs in
  if p mod 4 = 0 && b = 0 then bs else
  BS(s, (((p-1) / 4) + 1) * 4, 0, 0)
      
(** Decodes the bitcode header (if present) as well as the
    bitstream magic number. All data is checked for consistency *)
let decode_header bitstr =
  let decode_ir_magic bs = 
    let (str, bs) = decodeBytes bs 4 in
    if str = "\x42\x43\xc0\xde" then bs else error_header()
  in
  let (b0, bs) = decodeBytes bitstr 1 in 
  if b0 = "\xde" then (* Includes header *)
    let (b1, bs) = decodeBytes bs 3 in
    if b1 <> "\xc0\x17\x0b" then error_header() else
    let (version,bs) = decodeWord32 bs in
    if version != 0 then error_version version else
    let (offset,bs) = decodeWord32 bs in
    let (size,bs) = decodeWord32 bs in
    let (cputype,bs) = decodeWord32 bs in
    if bit_pos bs != offset * 8 then error_header() else
    let BS(s,_,_,_) = bs in                                      (* TEMP *)
    printf "Header: offset=%d, size_info=%d, string_size=%d\n"   (* TEMP *)
        offset size (String.length s);                           (* TEMP *)
    decode_ir_magic bs
  else 
    decode_ir_magic bitstr (* No header included *)

(* Decodes definition of abbreviations. Returns a tuple [(ops,bs)] where
   [ops] a list of abbreviation operands and [bs] is the bitstream. *)
let decode_define_abbrev bs = 
  let (numabbrevops,bs) = decodeVBR bs 5 in
  let rec decodeOps bs n = 
    if n = 0 then ([],bs) else
    let (abbrev_type,bs) = decodeFixedInt bs 1 in
    if abbrev_type = 1 then 
      let (litvalue, bs) = decodeVBR bs 8 in
      let (ops,bs) = decodeOps bs (n-1) in
      ((AbbrevOpLiteral(litvalue))::ops,bs)
    else
      let (encoding,bs) = decodeFixedInt bs 3 in
      (match encoding with
      | 1 (* Fixed *) -> 
        let (value,bs) = decodeVBR bs 5 in
        let (ops,bs) = decodeOps bs (n-1) in
        ((AbbrevOpFixed(value))::ops,bs)
      | 2 (* VBR *) -> 
        let (value,bs) = decodeVBR bs 5 in
        let (ops,bs) = decodeOps bs (n-1) in
        ((AbbrevOpVBR(value))::ops, bs)
      | 3 (* Array *) -> 
        let (ops,bs) = decodeOps bs (n-1) in
        ((AbbrevOpArray::ops), bs)
      | 4 (* Char6 *) ->
        let (ops,bs) = decodeOps bs (n-1) in
        ((AbbrevOpChar6::ops), bs)
      | 5 (* Blob *) -> 
        let (ops,bs) = decodeOps bs (n-1) in
        ((AbbrevOpBlob::ops), bs)
      | v -> error_unknown_abbrevcode v
      )
  in decodeOps bs numabbrevops 
        


let rec decode_stream bs scopes =
  match scopes with
  | [] -> failwith "The scope list cannot be empty." 
  | (abbrev_len)::top_scopes -> (
    print_bs "Before decode" bs;
    let (v,bs) = decodeFixedInt bs abbrev_len in
    match v with 
    | 0 (* END_BLOCK *) ->
      let bs = decodeAlign32 bs in
      printf "---- END BLOCK abbrevlen=%d-----\n" abbrev_len;                     (* TEMP *)
      decode_stream bs top_scopes
    | 1 (* ENTER_SUB_BLOCK *) ->
      let (blockid,bs) = decodeVBR bs 8 in
      let (newabbrev_len, bs) = decodeVBR bs 4 in
      let bs = decodeAlign32 bs in
      let (blocklength, bs) = decodeWord32 bs in
      printf "---- BEGIN BLOCK blockid=%d length=%d abbrev_len=%d new_ablen=%d -----\n"  (* TEMP *)
            blockid blocklength abbrev_len newabbrev_len ;                            (* TEMP *)
      decode_stream bs (newabbrev_len::scopes)      
    | 2 (* DEFINE_ABBREV *) ->
      let (ops,bs) = decode_define_abbrev bs in
      printf "---- DEFINE_ABBREV no_of_ops=%d -----\n" (List.length ops);       (* TEMP *)
      decode_stream bs scopes
    | 3 (* UNABBREV_RECORD *) ->
      let (code,bs) = decodeVBR bs 6 in
      let (numops,bs) = decodeVBR bs 6 in
      printf "---- UNABBREV_RECORD abbrev_len=%d code=%d numops=%d -----\n  ["   (* TEMP *)
           abbrev_len code numops;                                               (* TEMP *)
      let rec decodeOps bs n =
        if n = 0 then ([],bs) else
        let (op,bs) = decodeVBR bs 6 in
        let (ops,bs) = decodeOps bs (n-1) in
        (op::ops,bs) 
      in
      let (ops,bs) = decodeOps bs numops in
      List.iter (printf "%d,") ops; printf "]\n";                                (* TEMP *)                                                                   
      decode_stream bs scopes
    | _ -> failwith "Unknown abbreviation!!!"
  )



(**************** Exported functions *******************************)


let decode data =
  (* Create bitstream from byte string *)
  let bs = BS(data,0,0,0) in
  (* Decode the header *)
  let bs = decode_header bs in
  print_bs "" bs;
  (* Decode stream content *)
  let bs = decode_stream bs [2]  in
  print_bs "" bs

  


  (*
  let BS(s,p,d,b) = bs in
  printf "String length length=%d, end_pos=%d (%x)\n" (String.length data) p p;
  let bs = decodeAlign32 bs in
  let (v1,bs) = decodeVBR bs 2 in
  let (v2,bs) = decodeVBR bs 6 in
  let (v3,bs) = decodeFI bs 4 in
  let (v4,bs) = decodeFI bs 18 in
  let (v5,bs) = decodeFI bs 8 in
  printf "Decoded %d,%d,%d,%d,%d\n" v1 v2 v3 v4 v5
  *)







