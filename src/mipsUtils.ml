
open Ustring.Op
open Printf
open MipsAst
 
exception Decode_error of string


let decode_inst bininst =   
  let op = bininst lsr 26 in
  let rs() = (bininst lsr 21) land 0b11111 in
  let rt() = (bininst lsr 16) land 0b11111 in
  let rd() = (bininst lsr 11) land 0b11111 in
  let shamt() = (bininst lsr 6) land 0b11111 in
  let funct() = bininst land 0b111111 in
  let imm() = Utils.sign_extension (bininst land 0xffff) 16 in
  let address() = bininst land 0x3ffffff in
  match op with
  | 0 -> (match funct() with
           | 0  -> MipsSLL(rd(),rt(),shamt()) 
           | 2  -> MipsSRL(rd(),rt(),shamt()) 
           | 3  -> MipsSRA(rd(),rt(),shamt()) 
           | 4  -> MipsSLLV(rd(),rt(),rs())
           | 6  -> MipsSRLV(rd(),rt(),rs())
           | 8  -> MipsJR(rs())
           | 9  -> MipsJALR(rs())
           | 32 -> MipsADD(rd(),rs(),rt())
           | 33 -> MipsADDU(rd(),rs(),rt())           
           | 36 -> MipsAND(rd(),rs(),rt())
           | 42 -> MipsSLT(rd(),rs(),rt())
           | 43 -> MipsSLTU(rd(),rs(),rt())
           | _ -> MipsUnknown(bininst))
  | 2  -> MipsJ(address())
  | 3  -> MipsJAL(address())
  | 4  -> MipsBEQ(rs(),rt(),imm())
  | 5  -> MipsBNE(rs(),rt(),imm())
  | 8  -> MipsADDI(rt(),rs(),imm())
  | 9  -> MipsADDIU(rt(),rs(),imm())
  | 10 -> MipsSLTI(rt(),rs(),imm())
  | 11 -> MipsSLTIU(rt(),rs(),imm())
  | 12 -> MipsANDI(rt(),rs(),imm())
  | 32 -> MipsLB(rt(),imm(),rs())
  | 35 -> MipsLW(rt(),imm(),rs())
  | 36 -> MipsLBU(rt(),imm(),rs())
  | 40 -> MipsSB(rt(),imm(),rs())
  | 43 -> MipsSW(rt(),imm(),rs())
  | _ -> MipsUnknown(bininst)      
    

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
        (int_of_char (Bytes.get data (i)))
  in

  (* Decode all instructions and return a list *)
  let rec decode_list k acc = 
    if k < Bytes.length data then
      decode_list (k+4) ((decode_inst (get_32bits k))::acc) 
    else
      acc 
  in List.rev (decode_list 0 [])
      

  
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
let lparan = us"("
let rparan = us")"

let pprint_inst inst = 
  let rdst rd rs rt = (reg rd) ^. com ^. (reg rs) ^. com ^. (reg rt) in
  let rdts rd rt rs = rdst rd rt rs in
  let rdst rd rs rt = (reg rd) ^. com ^. (reg rs) ^. com ^. (reg rt) in
  let rtsi rt rs imm = (reg rt) ^. com ^. (reg rs) ^. com ^. ustring_of_int imm in
  let rtis rt imm rs = (reg rt) ^. com ^. ustring_of_int imm ^. 
                       lparan ^. (reg rs) ^. rparan in  
  let dta  rd rt shamt = rtsi rd rt shamt in
  let istr is = Ustring.spaces_after (us is) 8 in
  let address a = us(sprintf "0x%x" a) in
  match inst with
  | MipsADD(rd,rs,rt)    -> (istr "add") ^. (rdst rd rs rt)
  | MipsADDI(rt,rs,imm)  -> (istr "addi") ^. (rtsi rt rs imm)
  | MipsADDIU(rt,rs,imm) -> (istr "addiu") ^. (rtsi rt rs imm)
  | MipsADDU(rd,rs,rt)   -> (istr "addu") ^. (rdst rd rs rt)
  | MipsAND(rd,rs,rt)    -> (istr "and") ^. (rdst rd rs rt)
  | MipsANDI(rt,rs,imm)  -> (istr "andi") ^. (rtsi rt rs imm)
  | MipsBEQ(rs,rt,imm)   -> (istr "beq") ^. (rtsi rs rt imm)
  | MipsBNE(rs,rt,imm)   -> (istr "bne") ^. (rtsi rs rt imm)    
  | MipsJALR(rs)         -> (istr "jalr") ^. (reg rs)
  | MipsJR(rs)           -> (istr "jr") ^. (reg rs)
  | MipsJ(addr)          -> (istr "j") ^. (address addr)
  | MipsJAL(addr)        -> (istr "jal") ^. (address addr)
  | MipsSLT(rd,rs,rt)    -> (istr "slt") ^. (rdst rd rs rt)
  | MipsSLTU(rd,rs,rt)   -> (istr "sltu") ^. (rdst rd rs rt)  
  | MipsSLTI(rt,rs,imm)  -> (istr "slti") ^. (rtsi rt rs imm)
  | MipsSLTIU(rt,rs,imm) -> (istr "sltiu") ^. (rtsi rt rs imm)
  | MipsSLL(rd,rt,shamt) -> (istr "sll") ^. (dta rd rt shamt)
  | MipsSLLV(rd,rt,rs)   -> (istr "sllv") ^. (rdts rd rt rs)
  | MipsSRA(rd,rt,shamt) -> (istr "sra") ^. (dta rd rt shamt)
  | MipsSRL(rd,rt,shamt) -> (istr "srl") ^. (dta rd rt shamt)
  | MipsSRLV(rd,rt,rs)   -> (istr "srlv") ^. (rdts rd rt rs)
  | MipsLB(rt,imm,rs)    -> (istr "lb") ^. (rtis rt imm rs)
  | MipsLBU(rt,imm,rs)   -> (istr "lbu") ^. (rtis rt imm rs)
  | MipsLW(rt,imm,rs)    -> (istr "lw") ^. (rtis rt imm rs)
  | MipsSB(rt,imm,rs)    -> (istr "sb") ^. (rtis rt imm rs)
  | MipsSW(rt,imm,rs)    -> (istr "sw") ^. (rtis rt imm rs)
  | MipsNOP              -> (istr "nop") 
  | MipsUnknown(inst)    -> us(sprintf "[0x%x]" inst)


let pprint_inst_list instlst = 
  (Ustring.concat (us"\n") (List.map pprint_inst instlst)) ^. us"\n"




