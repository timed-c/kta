
open Ustring.Op
open Printf
open MipsAst
 
exception Decode_error of string


(* ---------------------------------------------------------------------*)
let decode_inst bininst =   
  let op = bininst lsr 26 in
  let rs() = (bininst lsr 21) land 0b11111 in
  let rt() = (bininst lsr 16) land 0b11111 in
  let rd() = (bininst lsr 11) land 0b11111 in
  let code() = (bininst lsr 6) land 0b1111111111 in
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
           | 16 -> MipsMFHI(rd())
           | 17 -> MipsMTHI(rs())
           | 18 -> MipsMFLO(rd())
           | 19 -> MipsMTLO(rs())
           | 24 -> MipsMULT(rs(),rt())
           | 25 -> MipsMULTU(rs(),rt())
           | 26 -> MipsDIV(rs(),rt())
           | 27 -> MipsDIVU(rs(),rt())
           | 32 -> MipsADD(rd(),rs(),rt())
           | 33 -> MipsADDU(rd(),rs(),rt())           
           | 34 -> MipsSUB(rd(),rs(),rt())           
           | 35 -> MipsSUBU(rd(),rs(),rt())           
           | 36 -> MipsAND(rd(),rs(),rt())
           | 37 -> MipsOR(rd(),rs(),rt())
           | 38 -> MipsXOR(rd(),rs(),rt())           
           | 39 -> MipsNOR(rd(),rs(),rt())
           | 42 -> MipsSLT(rd(),rs(),rt())
           | 43 -> MipsSLTU(rd(),rs(),rt())
           | 52 -> MipsTEQ(rs(),rt(),code())
           | _  -> MipsUnknown(bininst))
  | 2  -> MipsJ(address())
  | 3  -> MipsJAL(address())
  | 4  -> MipsBEQ(rs(),rt(),imm(),"")
  | 5  -> MipsBNE(rs(),rt(),imm(),"")
  | 6  -> MipsBLEZ(rs(),imm(),"")
  | 8  -> MipsADDI(rt(),rs(),imm())
  | 9  -> MipsADDIU(rt(),rs(),imm())
  | 10 -> MipsSLTI(rt(),rs(),imm())
  | 11 -> MipsSLTIU(rt(),rs(),imm())
  | 12 -> MipsANDI(rt(),rs(),imm())
  | 13 -> MipsORI(rt(),rs(),imm())
  | 14 -> MipsXORI(rt(),rs(),imm())
  | 15 -> MipsLUI(rt(),imm())
  | 28 -> (match funct() with
           | 2  -> MipsMUL(rd(),rs(),rt())
           | _  -> MipsUnknown(bininst))
  | 32 -> MipsLB(rt(),imm(),rs())
  | 35 -> MipsLW(rt(),imm(),rs())
  | 36 -> MipsLBU(rt(),imm(),rs())
  | 40 -> MipsSB(rt(),imm(),rs())
  | 43 -> MipsSW(rt(),imm(),rs())
  | _ -> MipsUnknown(bininst)      
    

(* ---------------------------------------------------------------------*)
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
      

  
(* ---------------------------------------------------------------------*)
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
  | 31 -> "$ra"
  | _  -> failwith "Not a register.")



(* ---------------------------------------------------------------------*)
let pprint_reg x = reg x

let com = us","
let lparan = us"("
let rparan = us")"



(* ---------------------------------------------------------------------*)
let pprint_inst inst = 
  let rdst rd rs rt = (reg rd) ^. com ^. (reg rs) ^. com ^. (reg rt) in
  let rdts rd rt rs = rdst rd rt rs in
  let rst rs rt = (reg rs) ^. com ^. (reg rt) in
  let rtsi rt rs imm = (reg rt) ^. com ^. (reg rs) ^. com ^. ustring_of_int imm in
  let rtsis rt rs imm s = (reg rt) ^. com ^. (reg rs) ^. com ^. 
                  if String.length s <> 0 then us s else ustring_of_int imm in
  let rsis rt imm s = (reg rt) ^. com ^. 
                  if String.length s <> 0 then us s else ustring_of_int imm in
  let rti  rt imm = (reg rt) ^. com ^. ustring_of_int imm in
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
  | MipsBEQ(rs,rt,imm,s) -> (istr "beq") ^. (rtsis rs rt imm s)
  | MipsBLEZ(rs,imm,s)   -> (istr "blez") ^. (rsis rs imm s)    
  | MipsBNE(rs,rt,imm,s) -> (istr "bne") ^. (rtsis rs rt imm s)    
  | MipsJALR(rs)         -> (istr "jalr") ^. (reg rs)
  | MipsJR(rs)           -> (istr "jr") ^. (reg rs)
  | MipsJ(addr)          -> (istr "j") ^. (address addr)
  | MipsJAL(addr)        -> (istr "jal") ^. (address addr)
  | MipsLB(rt,imm,rs)    -> (istr "lb") ^. (rtis rt imm rs)
  | MipsLBU(rt,imm,rs)   -> (istr "lbu") ^. (rtis rt imm rs)
  | MipsLUI(rt,imm)      -> (istr "lui") ^. (rti rt imm)
  | MipsLW(rt,imm,rs)    -> (istr "lw") ^. (rtis rt imm rs)
  | MipsMFHI(rd)         -> (istr "mfhi") ^. (reg rd)
  | MipsMFLO(rd)         -> (istr "mflo") ^. (reg rd)
  | MipsMTHI(rs)         -> (istr "mthi") ^. (reg rs)
  | MipsMTLO(rs)         -> (istr "mtlo") ^. (reg rs)
  | MipsMUL(rd,rs,rt)    -> (istr "mul") ^. (rdst rd rs rt)
  | MipsMULT(rs,rt)      -> (istr "mult") ^. (rst rs rt)
  | MipsMULTU(rs,rt)     -> (istr "multu") ^. (rst rs rt)
  | MipsDIV(rs,rt)       -> (istr "div") ^. (rst rs rt)
  | MipsDIVU(rs,rt)      -> (istr "divu") ^. (rst rs rt)
  | MipsNOR(rd,rs,rt)    -> (istr "nor") ^. (rdst rd rs rt)
  | MipsOR(rd,rs,rt)     -> (istr "or") ^. (rdst rd rs rt)
  | MipsORI(rt,rs,imm)   -> (istr "ori") ^. (rtsi rt rs imm)
  | MipsSLT(rd,rs,rt)    -> (istr "slt") ^. (rdst rd rs rt)
  | MipsSLTU(rd,rs,rt)   -> (istr "sltu") ^. (rdst rd rs rt)  
  | MipsSLTI(rt,rs,imm)  -> (istr "slti") ^. (rtsi rt rs imm)
  | MipsSLTIU(rt,rs,imm) -> (istr "sltiu") ^. (rtsi rt rs imm)
  | MipsSLL(rd,rt,shamt) -> (istr "sll") ^. (dta rd rt shamt)
  | MipsSLLV(rd,rt,rs)   -> (istr "sllv") ^. (rdts rd rt rs)
  | MipsSRA(rd,rt,shamt) -> (istr "sra") ^. (dta rd rt shamt)
  | MipsSRL(rd,rt,shamt) -> (istr "srl") ^. (dta rd rt shamt)
  | MipsSRLV(rd,rt,rs)   -> (istr "srlv") ^. (rdts rd rt rs)
  | MipsSB(rt,imm,rs)    -> (istr "sb") ^. (rtis rt imm rs)
  | MipsSW(rt,imm,rs)    -> (istr "sw") ^. (rtis rt imm rs)
  | MipsSUB(rd,rs,rt)    -> (istr "sub") ^. (rdst rd rs rt)
  | MipsSUBU(rd,rs,rt)   -> (istr "subu") ^. (rdst rd rs rt)
  | MipsTEQ(rs,rt,code)  -> (istr "teq") ^. (rtsi rs rt code)
  | MipsXOR(rd,rs,rt)    -> (istr "xor") ^. (rdst rd rs rt)
  | MipsXORI(rt,rs,imm)  -> (istr "xori") ^. (rtsi rt rs imm)
  | MipsUnknown(inst)    -> us(sprintf "[0x%x]" inst)


(* ---------------------------------------------------------------------*)
let pprint_inst_list instlst  = 
  (Ustring.concat (us"\n") (List.map pprint_inst instlst)) ^. us"\n"


(* ---------------------------------------------------------------------*)
let pprint_inst_ext inst prog addr print_addr =
  let pprint_label caddr =
    try 
      let sym = Addr2Sym.find caddr prog.addr2sym in
      us sym ^. us":\n" ^. 
      (if print_addr then us"           " else us"")          
    with Not_found -> us""     
  in
    (if print_addr then us(sprintf "0x%08x " addr) else us"") ^.
        pprint_label addr ^. (us"  ") ^.
        (pprint_inst (inst)) 



(* ---------------------------------------------------------------------*)
let pprint_asm prog addr len print_addr =
  let pos = (addr - prog.text_sec.addr) / 4 in  
  let rec loop cpos acc = 
    if cpos < len/4 then
      let caddr = cpos*4 + prog.text_sec.addr in
      loop (cpos + 1) (
          acc ^. pprint_inst_ext (prog.code.(cpos)) prog caddr print_addr ^. us"\n")
    else
      acc 
  in
    loop pos (us"")



(* ---------------------------------------------------------------------*)
let add_branch_symbols prog = 
  let codearray = Array.copy prog.code in
  let x = ref 1 in
  let (_,s2a,a2s) = Array.fold_left (fun (i,s2a,a2s) inst -> 
    let makenew imm =
      let addr = i*4 + 4 + imm*4 + prog.text_sec.addr in
      if Addr2Sym.mem addr a2s then 
        (Addr2Sym.find addr a2s,s2a,a2s)
      else 
        let rec find_unique() =
          let l = "l" ^ string_of_int !x in
          if not (Sym2Addr.mem l s2a) then 
            (x := !x + 1;
            (l,Sym2Addr.add l addr s2a,Addr2Sym.add addr l a2s))
          else
            find_unique()
        in find_unique()
    in

    match inst with
    | MipsBEQ(_,_,imm,_) | MipsBLEZ(_,imm,_) | MipsBNE(_,_,imm,_)  ->
        let (newlabel,s2a',a2s') = makenew imm in
        let i2 = (match inst with
          | MipsBEQ(rs,rt,_,_)  -> MipsBEQ(rs,rt,imm,newlabel)
          | MipsBLEZ(rs,_,_) -> MipsBLEZ(rs,imm,newlabel)
          | MipsBNE(rs,rt,_,_)  -> MipsBNE(rs,rt,imm,newlabel)
          | _ -> failwith "Should not happen"
        )in
        codearray.(i) <- i2;
        (i+1,s2a',a2s')
    | _ -> (i+1,s2a,a2s)
  ) (0,prog.sym2addr,prog.addr2sym) codearray in
  { prog with code = codearray ; sym2addr = s2a ; addr2sym = a2s}


(* ---------------------------------------------------------------------*)
let get_shift array index shift =
  Int32.shift_left (Int32.of_int (int_of_char (Bytes.get array index))) shift


(* ---------------------------------------------------------------------*)
let get_32_bits_from_bytes bigendian array i =
    if bigendian then
      Int32.logor (get_shift array (i) 24)
       (Int32.logor (get_shift array (i+1) 16)
        (Int32.logor (get_shift array (i+2) 8)
                       (get_shift array (i+3) 0)))
    else
      Int32.logor (get_shift array (i+3) 24)
       (Int32.logor (get_shift array (i+2) 16)
        (Int32.logor (get_shift array (i+1) 8)
                       (get_shift array (i) 0)))


(* ---------------------------------------------------------------------*)
let pprint_bytes b index len addr bigendian =
  let pc k = us(if k < len then  
                let v = Bytes.get b (k+index) in 
                sprintf "%c" (if v < ' ' || v > '~' then '.' else v)
                else " ") in
  let pb k = us(if k < len then sprintf "%4d" 
                (int_of_char (Bytes.get b (k+index))) else " ") in
  let get32 k = Int32.to_int 
    (get_32_bits_from_bytes bigendian b (k+index)) land 0xffffffff in
  let rec work k acc =
    if k < len then
      let str = acc ^.
                us(sprintf "0x%08x |" (addr+k)) ^. 
                pc k ^. pc (k+1) ^. pc (k+2) ^. pc (k+3) ^. us"|" ^.
                pb k ^. pb (k+1) ^. pb (k+2) ^. pb (k+3) ^.
                us(if k+4 <= len then 
                    let v = get32 k in
                    sprintf " | 0x%08x | %d" v v else "") ^.
                us"\n"
      in                
      work (k+4) str
    else acc
  in
    work 0 (us"")


