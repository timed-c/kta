

open RiscvISA
open Printf
open Ustring.Op

(*************** Exported types and exceptions ********************)

exception Decode_error
exception Unsupported_encoding

type big_endian = bool



(*************** Local types and exceptions ***********************)


(*************** Local functions **********************************)

(* Decoding of opcode for R, R4, I, B, L, and J Types *)
let d_op low = low land 0b1111111

(* Decoding of rd for R, R4, I, and L Types *)
let d_rd high = high lsr 11 

(* Decoding of rs1 for R, R4, I, and B Types *)
let d_rs1 high = (high lsr 6) land 0b11111 

(* Decoding of rs2 for R, R4, and B Types  *)
let d_rs2 high = (high lsr 1) land 0b11111 

(* Decoding of rs3 for the R4 type *)
let d_rs3 high low = ((high land 1) lsl 4) lor (low lsr 12) 

(* Decoding of 12 bit immediate for I-Type *)
let d_imI high low = ((high land 0b111111) lsl 6) lor (low lsr 10) 

(* Decoding of 12 bit immediate for B-Type *)
let d_imB high low = ((high lsr 11) lsl 7) lor ((high land 1) lsl 6) 
                     lor (low lsr 10)

(* Decoding of 20 bit immediate for L-Type *)
let d_imL high low = ((high land 0b11111111111) lsl 9) lor (low lsr 7)

(* Decoding of 25 bit jump offset for J-Type *)
let d_offJ high low = (high lsl 9) lor (low lsr 7)

(* Decoding of 10 bit funct opcode field for R-Type *)
let d_funR high low = ((high land 1) lsl 9) lor (low lsr 7)

(* Decoding of 5 bit funct opcode field for R4-Type *)
let d_funR4 low = (low lsr 7) land 0b11111

(* Decoding of 3 bit funct opcode field for I and B Types *)
let d_funIB low = (low lsr 7) land 0b111

(* Error reporting if we have an unknown instruction *)
let failinst h l = 
  failwith (sprintf "ERROR: Unknown instruction %x,%x,%x,%x\n" 
            (h lsr 8) (h land 0xff) (l lsr 8) (l land 0xff))

(* Decodes the top 7 bits funct opcode of the R-Type, excluding rm field *)
let d_fp_funR h l = ((h land 1) lsl 6) lor ((l lsr 12) lsl 2) lor
                    ((l lsr 7) land 0b11)


             
(* Decodes one 32 bit instruction *)
let decode_32inst h l =
  let fi = Info(0, usid"", 4) in
  match d_op l with
    (* Absolute Jump Instructions *)
  | 0b1100111 -> IAbsJmp(fi, d_offJ h l, OpJ)
  | 0b1101111 -> IAbsJmp(fi, d_offJ h l, OpJAL)
    (* Conditional Jump Instructions *)
  | 0b1100011 -> 
      let op = match d_funIB l with 0b000 -> OpBEQ | 0b001 -> OpBNE  | 0b100 -> OpBLT |
                                    0b101 -> OpBGE | 0b110 -> OpBLTU | 0b111 -> OpBGEU | 
                                    _ -> failinst h l in
    ICondJmp(fi, d_rs1 h, d_rs2 h, d_imB h l, op) 
    (* Indirect Jump Instructions *)
  | 0b1101011 -> 
      let op = match d_funIB l with 0b000 -> OpJALR_C | 0b001 -> OpJALR_R | 
                                    0b010 -> OpJALR_J | 0b100 -> OpRDNPC | 
                                    _ -> failinst h l in
      IIndJmp(fi, d_rd h, d_rs1 h, d_imI h l, op)
    (* Load Memory Instructions *)
  | 0b0000011 -> 
      let op = match d_funIB l with 0b000 -> OpLB  | 0b001 -> OpLH  | 0b010 -> OpLW |
                                    0b011 -> OpLD  | 0b100 -> OpLBU | 0b101 -> OpLHU |
                                    0b110 -> OpLWU | _ -> failinst h l in
      ILoad(fi, d_rd h, d_rs1 h, d_imI h l, op)
    (* Store Memory Instructions *)
  | 0b0100011 ->
      let op = match d_funIB l with 0b000 -> OpSB | 0b001 -> OpSH | 
                                    0b010 -> OpSW | 0b011 -> OpSD | _ -> failinst h l in
      IStore(fi, d_rs1 h, d_rs2 h, d_imB h l, op)
    (* Atomic Memory Instructions *)
  | 0b0101011 ->
      let op = match d_funR h l with 0b000010 -> OpAMOADD_W  | 0b001010 -> OpAMOSWAP_W |
                                     0b010010 -> OpAMOAND_W  | 0b011010 -> OpAMOOR_W |
                                     0b100010 -> OpAMOMIN_W  | 0b101010 -> OpAMOMAX_W |
                                     0b110010 -> OpAMOMINU_W | 0b111010 -> OpAMOMAXU_W |
                                     0b000011 -> OpAMOADD_D  | 0b001011 -> OpAMOSWAP_D |
                                     0b010011 -> OpAMOAND_D  | 0b011011 -> OpAMOOR_D |
                                     0b100011 -> OpAMOMIN_D  | 0b101011 -> OpAMOMAX_D |
                                     0b110011 -> OpAMOMINU_D | 0b111011 -> OpAMOMAXU_D |
                                     _ -> failinst h l in
      IAtomic(fi, d_rd h, d_rs1 h, d_rs2 h, op)
     (* Integer Register-Immediate Compute Instructions *)
  | 0b0010011 ->
      let im12 = d_imI h l in
      let imop = im12 lsr 6 in
      let op = match d_funIB l with 0b000 -> OpADDI  | 0b001 -> OpSLLI | 0b010 -> OpSLTI |
                                    0b011 -> OpSLTIU | 0b100 -> OpXORI | 
                                    0b101 when imop = 0 -> OpSRLI |
                                    0b101 when imop = 1 -> OpSRAI |
                                    0b110 -> OpORI   | 0b111 -> OpANDI |
                                    _ -> failinst h l in
      let im = match op with OpSLLI | OpSRLI | OpSRAI -> im12 land 0b111111 | _ -> im12 in
      ICompImm(fi, d_rd h, d_rs1 h, im, op)
  | 0b0110111 -> ICompImm(fi, d_rd h, 0, d_imL h l, OpLUI)
  | 0b0011011 ->
      let im12 = d_imI h l in
      let imop = im12 lsr 5 in
      let op = match d_funIB l with 0b000 -> OpADDIW | 0b001 -> OpSLLIW | 
                                    0b101 when imop = 0b00 -> OpSRLIW |
                                    0b101 when imop = 0b10 -> OpSRAIW |
                                    _ -> failinst h l in
      let im = match op with OpSRLIW | OpSRAIW -> im12 land 0b11111 | _ -> im12 in
      ICompImm(fi, d_rd h, d_rs1 h, im, op)
     (* Integer Register-Register Compute Instructions *)
  | 0b0110011 ->
      let op = match d_funR h l with 0b0000000000 -> OpADD    | 0b1000000000 -> OpSUB   |
                                     0b0000000001 -> OpSLL    | 0b0000000010 -> OpSLT   |
                                     0b0000000011 -> OpSLTU   | 0b0000000100 -> OpXOR   | 
                                     0b0000000101 -> OpSRL    | 0b1000000101 -> OpSRA   | 
                                     0b0000000110 -> OpOR     | 0b0000000111 -> OpAND   |
                                     0b0000001000 -> OpMUL    | 0b0000001001 -> OpMULH  |
                                     0b0000001010 -> OpMULHSU | 0b0000001011 -> OpMULHU |
                                     0b0000001100 -> OpDIV    | 0b0000001101 -> OpDIVU  |
                                     0b0000001110 -> OpREM    | 0b0000001111 -> OpREMU  |
                                     _ -> failinst h l in
      ICompReg(fi, d_rd h, d_rs1 h, d_rs2 h, op)
  | 0b0111011 ->
      let op = match d_funR h l with 0b0000000000 -> OpADDW   | 0b1000000000 -> OpSUBW  |
                                     0b0000000001 -> OpSLLW   | 0b0000000101 -> OpSRLW  |
                                     0b1000000101 -> OpSRAW   | 0b0000001000 -> OpMULW  |
                                     0b0000001100 -> OpDIVW   | 0b0000001101 -> OpDIVUW |
                                     0b0000001110 -> OpREMW   | 0b0000001111 -> OpREMUW |
                                     _ -> failinst h l in
      ICompReg(fi, d_rd h, d_rs1 h, d_rs2 h, op)
  (* Miscellaneous Memory Instructions *)
  | 0b0101111 ->
       let op = match d_funIB l with 0b001 -> OpFENCE_I | 0b010 -> OpFENCE |
                                           _ -> failinst h l in
       IMiscMem(fi, d_rd h, d_rs1 h, d_imI h l, op)
  (* System Instructions *)
  | 0b1110111 ->
       let op = match d_funR h l with 0b0000000000 -> OpSYSCALL  | 0b0000000001 -> OpBREAK |
                                      0b0000000100 -> OpRDCYCLE  | 0b0000001100 -> OpRDTIME |
                                      0b0000010100 -> OpRDINSTRET | _ -> failinst h l in
       ISys(fi, d_rd h, op)
  | _ -> failinst h l




(**************** Exported functions *******************************)


let decode bige data pos = 
  let parcel high low =
    let h = int_of_char high in
    let l = int_of_char low in
     if bige then (h lsl 8) lor l else  (l lsl 8) lor h in
  let size = String.length data in
  if pos+2 > size then raise Decode_error else
  let p1 = parcel data.[pos] data.[pos+1] in
  if (p1 land 0b11) <> 0b11 then raise Unsupported_encoding else
  if (p1 land 0b11111) = 0b11111 then raise Unsupported_encoding else
  if pos+4 > size then raise Decode_error else
  let p2 = parcel data.[pos+2] data.[pos+3] in
  (decode_32inst p2 p1, pos+4) 


let decode_interval bige data pos len = []
  

let decode_all bige data = 
  decode_interval bige data 0 (String.length data) 


let encode bige inst = ""

let encode_all bige instlst = ""
  

