
open Ustring.Op
open Printf
open MipsAst

exception Decode_error of string



let decode ?(bigendian=false) data = 
  (* Check that the size of the input data is correct (multiple of 4 bytes) *)
  if (Bytes.length data) mod 4 != 0 then 
    raise (Invalid_argument "MIPS decode error. The input data must be a multiple of 4 bytes.")
  else

  (* Function for returning endianness *)
  let get_32bits i = 
    if bigendian then
      (int_of_char (Bytes.get data (i)) lsl 24) lor
      (int_of_char (Bytes.get data (i+1)) lsl 16) lor
      (int_of_char (Bytes.get data (i+2)) lsl 8) lor
      (int_of_char (Bytes.get data (i+3)))
    else
      (int_of_char (Bytes.get data (i+3)) lsl 24) lor
      (int_of_char (Bytes.get data (i+2)) lsl 16) lor
      (int_of_char (Bytes.get data (i+1)) lsl 8) lor
      (int_of_char (Bytes.get data (i))) in

  let bininst = get_32bits 4 in
  let op = bininst lsr 26 in
  let rs() = (bininst lsr 21) land 0b11111 in
  let rt() = (bininst lsr 16) land 0b11111 in
  let rd() = (bininst lsr 11) land 0b11111 in
  let shamt() = (bininst lsr 6) land 0b11111 in
  let funct() = bininst land 0b111111 in
  let imm() = Utils.sign_extension (bininst land 0xffff) 16 in
  let address() = bininst land 0x3ffffff in
  let inst = 
    match op with
    | 0 -> (match funct() with
            | 32 -> MipsADD(rd(),rs(),rt())
            | 33 -> MipsADDU(rd(),rs(),rt()))
    | 9 -> MipsADDIU(rt(),rs(),imm())
    | _ -> MipsUnknown(bininst)      
  in 
    [inst]
    
let reg x = us(
  match x with
  | 0  -> "$0"
  | 1  -> "$at"
  | 2  -> "$v0"
  | 3  -> "$v1"
  | 4  -> "$a0"
  | 5  -> "$a1"
  | 6  -> "$a2"
  | 7  -> "$a3"
  | 8  -> "$t0"
  | 9  -> "$t1"
  | 10 -> "$t2"
  | 11 -> "$t3"
  | 12 -> "$t4"
  | 13 -> "$t5"
  | 14 -> "$t6"
  | 15 -> "$t7"
  | 16 -> "$s0"
  | 17 -> "$s1"
  | 18 -> "$s2"
  | 19 -> "$s3"
  | 20 -> "$s4"
  | 21 -> "$s5"
  | 22 -> "$s6"
  | 23 -> "$s7"
  | 24 -> "$t8"
  | 25 -> "$t9"
  | 26 -> "$k0"
  | 27 -> "$k1"
  | 28 -> "$gp"
  | 29 -> "$sp"
  | 30 -> "$fp"
  | 31 -> "$ra")

let com = us","


let pprint_inst inst = 
  let rdst rd rs rt = (reg rd) ^. com ^. (reg rs) ^. com ^. (reg rt) in
  let rtsimm rt rs imm = (reg rt) ^. com ^. (reg rs) ^. com ^. ustring_of_int imm in
  let istr is = Ustring.spaces_after (us is) 8 in
  match inst with
  | MipsADD(rd,rs,rt) -> (istr "add") ^. (rdst rd rs rt)
  | MipsADDI(rt,rs,imm) -> (istr "addi") ^. (rtsimm rt rs imm)
  | MipsADDIU(rt,rs,imm) -> (istr "addiu") ^. (rtsimm rt rs imm)
  | MipsADDU(rd,rs,rt) -> (istr "addu") ^. (rdst rd rs rt)
  | MipsUnknown(inst) -> us(sprintf "0x%x" inst)


let pprint_inst_list instlst = 
  (Ustring.concat (us"\n") (List.map pprint_inst instlst)) ^. us"\n"




