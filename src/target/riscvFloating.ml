


(* This file contains floating-point implementations that are not used for the 
   moment 
   A challenge of encoding the FP instructions is to make it in a compact way
   and at the same time guarantee static properties. For example, it is safer
   to distinguees between the number of rs fields in an instruction directly in
   the ast. 
*)



(*** From riscvISA.ml ***)

type opFPLoad     = OpFLW   | OpFLD
type opFPStore    = OpFSW   | OpFSD 
type opFPComp0    = OpMFFSR  
type opFPComp1    = OpFSQRT_S   | OpFSQRT_D   | OpFCVT_S_D | OpFCVT_D_S
                    OpFCVT_S_L  | OpFCVT_S_LU | OpFCVT_S_W | OpFCVT_S_WU |
                    OpFCVT_D_L  | OpFCVT_D_LU | OpFCVT_D_W | OpFCVT_D_WU |
                    OpFCVT_L_S  | OpFCVT_LU_S | OpFCVT_W_S | OpFCVT_WU_S |
                    OpFCVT_L_D  | OpFCVT_LU_D | OpFCVT_W_D | OpFCVT_WU_D |
                    OpMXTF_S    | OpMXTF_D    | OpMTFSR    |
                    OpMFTX_S    | OpMFTX_D      
type opFPComp2    = OpFADD_S    | OpFSUB_S    | OpFMUL_S   | OpFDIV_S |
                    OpFMIN_S    | OpFMAX_S    | 
                    OpFADD_D    | OpFSUB_D    | OpFMUL_D   | OpFDIV_D |
                    OpFMIN_D    | OpFMAX_D    | 
                    OpFSGNJ_S   | OpFSGNJN_S  | OpFSGNJX_S | OpFSGNJ_D | 
                    OpFSGNJN_D  | OpFSGNJX_D  |
                    OpFEQ_S     | OpFLT_S     | OpFLE_S    | OpFEQ_D |
                    OpFLT_D     | OpFLE_D                   
type opFPComp3    = OpFMADD_S   | OpFMSUB_S   | OpFNMSUB_S | OpFNMADD_S |
                    OpFMADD_D   | OpFMSUB_D   | OpFNMSUB_D | OpFNMADD_D


| IFPLoad     of info * rd  * rs1 * imm12 * opFPLoad             (* Floating-Point Load Memory *) 
| IFPStore    of info * rs1 * rs2 * imm12 * opFPStore            (* Floating-Point Store Memory *)
| IFPComp0    of info * rd  * rs1 * rm    * opFPComp             (* FP comp, mov, compare (0 reg) *)
| IFPComp1    of info * rd  * rs1 * rm    * opFPComp             (* FP comp, mov, compare (1 reg) *)
| IFPComp2    of info * rd  * rs1 * rs2   * rm * opFPComp        (* FP comp, mov, compare (2 reg) *)
| IFPComp3    of info * rd  * rs1 * rs2   * rs3 * rm * opFPComp3 (* FP comp, mov, compare (3 reg) *)


(* Floating-Point Rounding Mode. See table 3 in RISC-V manual *)
type rm = RmRNE | RmRTZ | RmRDN | RmRUP | RmRMM 



(*** riscvBIN.ml ***)

(* Decodes the rounding mode from R-type (used in FP instructions) *)
let d_rmR l = match (l lsr 9) land 0b111 with 
                0b000 -> RmRNE | 0b001 -> RmRTZ | 0b010 -> RmRDN | 
                0b011 -> RmRUP | 0b100 -> RmRMM | 
                _ -> failwith "ERROR: Unknown rounding mode."


    (* Floating-Point Load Memory Instructions *)
  | 0b0000111 -> 
      let op = match d_funIB l with 0b010 -> OpFLW | 0b011 -> OpFLD | _ -> failinst h l in
      IFPLoad(fi, d_rd h, d_rs1 h, d_imI h l, op)
    (* Floating-Point Store Memory Instructions *)
  | 0b0100111 ->
      let op = match d_funIB l with 0b010 -> OpFSW | 0b011 -> OpFSD | _ -> failinst h l in
      IFPStore(fi, d_rs1 h, d_rs2 h, d_imB h l, op)
    (* Floating-Point Register-Register Compute Instructions *)
  | 0b1010011 ->
      let op = match d_fp_funR h l with 
               0b0000000 -> OpFADD_S    | 0b0000100 -> OpFSUB_S  | 0b0001000 -> OpFMUL_S | (* Comp S *)
               0b0001100 -> OpFDIV_S    | 0b0010000 -> OpFSQRT_S | 0b1100000 -> OpFMIN_S |
               0b1100100 -> OpFMAX_S    |
               0b0000001 -> OpFADD_D    | 0b0000101 -> OpFSUB_D  | 0b0001001 -> OpFMUL_D | (* Comp D *)
               0b0001101 -> OpFDIV_D    | 0b0010001 -> OpFSQRT_D | 0b1100001 -> OpFMIN_D |
               0b1100101 -> OpFMAX_D    | 
               0b0010100 -> OpFSGNJ_S   | 0b0011000 -> OpFSGNJN_S  | 0b0011100 -> OpFSGNJX_S  | (* FP Mov/Conv *)
               0b0010101 -> OpFSGNJ_D   | 0b0011001 -> OpFSGNJN_D  | 0b0011101 -> OpFSGNJX_D  | 
               0b1000100 -> OpFCVT_S_D  | 0b1000001 -> OpFCVT_D_S  |
               0b0110000 -> OpFCVT_S_L  | 0b0110100 -> OpFCVT_S_LU | 0b0111000 -> OpFCVT_S_W  | (* Int-FP Mov/Conv *)
               0b0111100 -> OpFCVT_S_WU | 0b0110001 -> OpFCVT_D_L  | 0b0110101 -> OpFCVT_D_LU |
               0b0111001 -> OpFCVT_D_W  | 0b0111101 -> OpFCVT_D_WU | 0b1111000 -> OpMXTF_S    |
               0b1111001 -> OpMXTF_D    | 0b1111100 -> OpMTFSR |
               0b0100000 -> OpFCVT_L_S  | 0b0100100 -> OpFCVT_LU_S | 0b0101000 -> OpFCVT_W_S  | (* FP-Int Mov/Conv *)
               0b0101100 -> OpFCVT_WU_S | 0b0100001 -> OpFCVT_L_D  | 0b0100101 -> OpFCVT_LU_D |
               0b0101001 -> OpFCVT_W_D  | 0b0101101 -> OpFCVT_WU_D | 0b1110000 -> OpMFTX_S    |
               0b1110001 -> OpMFTX_D    | 0b1110100 -> OpMFFSR     |
               0b1010100 -> OpFEQ_S     | 0b1011000 -> OpFLT_S     | 0b1011100 -> OpFLE_S     | (* FP Compare *)
               0b1010101 -> OpFEQ_D     | 0b1011001 -> OpFLT_D     | 0b1011101 -> OpFLE_D     |
               _ -> failinst h l in
      IFPComp(fi, d_rd h, d_rs1 h, d_rs2 h, d_rmR l, op)
  | 0b1000011 ->
       let op = if l land 0b110000000 = 0 then OpFMADD_S else OpFMADD_D in
       IFPComp3(fi, d_rd h, d_rs1 h, d_rs2 h, d_rs3 h l, d_rmR l, op)
  | 0b1000111 -> 
       let op = if l land 0b110000000 = 0 then OpFMSUB_S else OpFMSUB_D in
       IFPComp3(fi, d_rd h, d_rs1 h, d_rs2 h, d_rs3 h l, d_rmR l, op)
  | 0b1001011 ->  
       let op = if l land 0b110000000 = 0 then OpFNMSUB_S else OpFNMSUB_D in
       IFPComp3(fi, d_rd h, d_rs1 h, d_rs2 h, d_rs3 h l, d_rmR l, op)
  | 0b1001111 ->
       let op = if l land 0b110000000 = 0 then OpFNMADD_S else OpFNMADD_D in
       IFPComp3(fi, d_rd h, d_rs1 h, d_rs2 h, d_rs3 h l, d_rmR l, op)





(*** riscvAsm.ml ***)

(* Pretty print floating-point loads *)
let ppFPLoad op = us (match op with 
  | OpFLW -> "flw"  | OpFLD -> "fld")

(* Pretty print floating-point stores *)
let ppFPStore op = us (match op with 
  | OpFSW -> "fsw" | OpFSD -> "fsd")

(* Pretty printing of floating point computations *)
let ppFPComp op = us (match op with 
  | OpFADD_S    -> "fadd.s"    | OpFSUB_S    -> "fsub.s"    | OpFMUL_S    -> "fmul.s"   
  | OpFDIV_S    -> "fdiv.s"    | OpFSQRT_S   -> "fsqrt.s"   | OpFMIN_S    -> "fmin.s"   
  | OpFMAX_S    -> "fmax.s"    | OpFADD_D    -> "fadd.d"    | OpFSUB_D    -> "fsub.d"   
  | OpFMUL_D    -> "fmul.d"    | OpFDIV_D    -> "fdiv.d"    | OpFSQRT_D   -> "fsqrt.d"  
  | OpFMIN_D    -> "fmin.d"    | OpFMAX_D    -> "fmax.d"    | OpFSGNJ_S   -> "fsgnj.s"  
  | OpFSGNJN_S  -> "fsgnjn.s"  | OpFSGNJX_S  -> "fsgnjx.s"  | OpFSGNJ_D   -> "fsgnj.d" 
  | OpFSGNJN_D  -> "fsgnjn.d"  | OpFSGNJX_D  -> "fsgnjx.d"  | OpFCVT_S_D  -> "fcvt.s.d"
  | OpFCVT_D_S  -> "fcvt.d.s"  | OpFCVT_S_L  -> "fcvt.s.l"  | OpFCVT_S_LU -> "fcvt.s.lu"
  | OpFCVT_S_W  -> "fcvt.s.w"  | OpFCVT_S_WU -> "fcvt.s.wu" | OpFCVT_D_L  -> "fcvt.d.l"
  | OpFCVT_D_LU -> "fcvt.d.lu" | OpFCVT_D_W  -> "fcvt.d.w"  | OpFCVT_D_WU -> "fcvt.d.wu" 
  | OpMXTF_S    -> "mxtf.s"    | OpMXTF_D    -> "mxtf.d"    | OpMTFSR     -> "mtfsr"  
  | OpFCVT_L_S  -> "fcvt.l.s"  | OpFCVT_LU_S -> "fcvt.lu.s" | OpFCVT_W_S  -> "fcvt.w.s"
  | OpFCVT_WU_S -> "fcvt.wu.s" | OpFCVT_L_D  -> "fcvt.l.d"  | OpFCVT_LU_D -> "fcvt.lu.d" 
  | OpFCVT_W_D  -> "fcvt.w.d"  | OpFCVT_WU_D -> "fcvt.wu.d" | OpMFTX_S    -> "mftx.s"
  | OpMFTX_D    -> "mftx.d"    | OpMFFSR     -> "mffsr"     | OpFEQ_S     -> "feq.s"
  | OpFLT_S     -> "flt.s"     | OpFLE_S     -> "fle.s"     | OpFEQ_D     -> "feq.d"
  | OpFLT_D     -> "flt.d"     | OpFLE_D     -> "fle.d")
                  
(* Pretty printing of floating point computations (with 3 args) *)
let ppFPComp3 op = us (match op with 
  | OpFMADD_S  -> "fmadd.s"  | OpFMSUB_S  -> "fmsub.s"  | OpFNMSUB_S -> "fnmsub.s"
  | OpFNMADD_S -> "fnmadd.s" | OpFMADD_D  -> "fmadd.d"  | OpFMSUB_D  -> "fmsub.d" 
  | OpFNMSUB_D -> "fnmsub.d" | OpFNMADD_D -> "fnmadd.d")


(* Pretty print floating point register *)
let ppFPreg r =
  if r < 0 || r > 31 then raise (Failure (sprintf "Unknown register value %d." r))
    else us"f" ^. ustring_of_int r

(* Returns true for floating-point instructions that are using rounding mode *)
let useRM rm = match rm with 
  | OpFMIN_S  | OpFMAX_S   | OpFMIN_D   | OpFMAX_D | OpFSGNJ_S | OpFSGNJN_S | OpFSGNJX_S 
  | OpFSGNJ_D | OpFSGNJN_D | OpFSGNJX_D | OpMXTF_S | OpMXTF_D  | OpMTFSR    | OpMFTX_S   
  | OpMFTX_D  | OpMFFSR _ -> false | _ -> true

(* Pretty print the rounding mode *)
let ppRM rm = us (match rm with 
  | RmRNE -> "rne" | RmRTZ -> "rtz" | RmRDN -> "rdn" | RmRUP -> "rup" | RmRMM -> "rmm")


(* Returns [str] if [opt] is true, else returns an empty string *)
let stropt opt str = if opt then us"," ^. str else us""

let pp4arg n map op a1 a2 a3 a4 = op ^. space op n  ^. a1 ^. us"," ^. a2 ^. us"," ^. a3 ^. us"," ^. a4 
let pp5arg n map op a1 a2 a3 a4 a5 = op ^. space op n  ^. a1 ^. us"," ^. a2 ^. us"," ^. a3 ^. us"," ^. a4
                                  ^. us"," ^. a5 



  | IFPLoad(fi,rd,rs1,imm12,op) ->
      pp3arg_addr n map (ppFPLoad op) (ppXreg rd) (ppXreg rs1) (ustring_of_int (sign_ext imm12 12))
  | IFPStore(fi,rs1,rs2,imm12,op) ->
      pp3arg_addr n map (ppFPStore op) (ppXreg rs2) (ppXreg rs1) (ustring_of_int (sign_ext imm12 12))  
  | IFPComp(fi,rd,rs1,rs2,rm,op) ->
      pp4arg n map (ppFPComp op) (ppXreg rd) (ppXreg rs1) (ppXreg rs2) (stropt (useRM op) (ppRM rm))
  | IFPComp3(fi,rd,rs1,rs2,rs3,rm,op) ->
      pp5arg n map (ppFPComp3 op) (ppXreg rd) (ppXreg rs1) (ppXreg rs2) (ppXreg rs3) (ppRM rm)



(*** testBinDecodePrint.ml ***)


  (*** Floating-point Loads and Stores ***)

  test_ustr "Instruction flw"  
             (inst true 0x1e48  "\xd1\x07\x18\x90")  (us"flw     x3,1076(x2)");

  test_ustr "Instruction fsw"  
             (inst true 0x18d8  "\x11\x27\x07\xba")  (us"fsw     x29,4(x30)");

