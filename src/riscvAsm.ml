

open Ustring.Op
open RiscvISA
open Printf

(*************** Exported types and exceptions ********************)

type big_endian = bool



(*************** Local types and exceptions ***********************)


(* Needs to consider instrutions "move" (actually addi) beqz etc. *)

(*************** Local functions **********************************)

(* Sign extend value [v] that has [n] significant bits *)
let sign_ext v n = if ((v lsr (n-1)) land 1) = 0 
                   then v else (-1 lsl n) lor v

(* Pretty print absolute jumps *)
let ppAbsJmp op = us (match op with  
   | OpJ   -> "j" | OpJAL -> "jal")
  
(* Pretty print conditional jumps *)
let ppCondJmp op = us (match op with
  | OpBEQ  -> "beq"  | OpBNE  -> "bne" | OpBLT  -> "blt" | OpBGE  -> "bge"
  | OpBLTU -> "bltu" | OpBGEU -> "bgeu")

(* Pretty print indirect jumps *)
let ppIndJmp op = us (match op with
  | OpJALR_C -> "jalr.c" | OpJALR_R -> "jalr.r" | OpJALR_J -> "jalr.j"
  | OpRDNPC  -> "rdnpc")

(* Pretty print loads *)
let ppLoad op = us (match op with 
  | OpLB  -> "lb"  | OpLH  -> "lh"  | OpLW  -> "lw"  | OpLD -> "ld"
  | OpLBU -> "lbu" | OpLHU -> "lhu" | OpLWU -> "lwu")

(* Pretty print stores *)
let ppStore op = us (match op with 
  | OpSB -> "sb" | OpSH -> "sh" | OpSW -> "sw" | OpSD -> "sd")

(* Pretty printing atomic memory instructions *)
let ppAtomic op = us (match op with 
  | OpAMOADD_W  -> "amoadd.w"  | OpAMOSWAP_W -> "amoswap.w"| OpAMOAND_W  -> "amoand.w" 
  | OpAMOOR_W   -> "amoor.w"   | OpAMOMIN_W  -> "amomin.w" | OpAMOMAX_W  -> "amomax.w" 
  | OpAMOMINU_W -> "amominu.w" | OpAMOMAXU_W -> "amomaxu.w"| OpAMOADD_D  -> "amoadd.d" 
  | OpAMOSWAP_D -> "amoswap.d" | OpAMOAND_D  -> "amoand.d" | OpAMOOR_D   -> "amoor.d" 
  | OpAMOMIN_D  -> "amomin.d"  | OpAMOMAX_D  -> "amomax.d" | OpAMOMINU_D -> "amominu.d" 
  | OpAMOMAXU_D -> "amomaxu.d")

(* Pretty printing integer register-register instructions *)
let ppCompReg op = us (match op with
  | OpADD   -> "add"   | OpSUB  -> "sub"  | OpSLL    -> "sll"    | OpSLT   -> "slt"   | OpSLTU -> "sltu" 
  | OpXOR   -> "xor"   | OpSRL  -> "srl"  | OpSRA    -> "sra"    | OpOR    -> "or"    | OpAND  -> "and" 
  | OpMUL   -> "mul"   | OpMULH -> "mulh" | OpMULHSU -> "mulhsu" | OpMULHU -> "mulhu" | OpDIV  -> "div"
  | OpDIVU  -> "divu"  | OpREM  -> "rem"  | OpREMU   -> "remu"   | OpADDW  -> "addw"  | OpSUBW -> "subw" 
  | OpSLLW  -> "sllw"  | OpSRLW -> "srlw" | OpSRAW   -> "sraw"   | OpMULW  -> "mulw"  | OpDIVW -> "divw"
  | OpDIVUW -> "divuw" | OpREMW -> "remw" | OpREMUW  -> "remuw")

(* Pretty print integer intermediate-register instructions *)
let ppCompImm op = us (match op with
  | OpADDI  -> "addi"  | OpSLLI  -> "slli"  | OpSLTI  -> "slti" | OpSLTIU -> "sltiu"
  | OpXORI  -> "xori"  | OpSRLI  -> "srli"  | OpSRAI  -> "srai" | OpORI   -> "ori"   
  | OpANDI  -> "andi"  | OpLUI   -> "lui"   | OpADDIW -> "addiw"| OpSLLIW -> "slliw"
  | OpSRLIW -> "srliw" | OpSRAIW -> "sraiw")

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

(* Pretty printing misc memory instructions  *)
let ppMiscMem op = us (match op with 
  | OpFENCE_I -> "fence.i"  | OpFENCE -> "fence")

(* Pretty printing sys instructions *)
let ppSys op = us (match op with 
  | OpSYSCALL -> "syscall"  | OpBREAK     -> "break"      | OpRDCYCLE -> "rdcycle"   
  | OpRDTIME  -> "rdtime"   | OpRDINSTRET -> "rdinstret")

(* Pretty print the rounding mode *)
let ppRM rm = us (match rm with 
  | RmRNE -> "rne" | RmRTZ -> "rtz" | RmRDN -> "rdn" | RmRUP -> "rup" | RmRMM -> "rmm")

(* Returns true for floating-point instructions that are using rounding mode *)
let useRM rm = match rm with 
  | OpFMIN_S  | OpFMAX_S   | OpFMIN_D   | OpFMAX_D | OpFSGNJ_S | OpFSGNJN_S | OpFSGNJX_S 
  | OpFSGNJ_D | OpFSGNJN_D | OpFSGNJX_D | OpMXTF_S | OpMXTF_D  | OpMTFSR    | OpMFTX_S   
  | OpMFTX_D  | OpMFFSR _ -> false | _ -> true

(* Pretty print general purpose register *)
let ppXreg r =
  if r < 0 || r > 31 then raise (Failure (sprintf "Unknown register value %d." r))
    else us"x" ^. ustring_of_int r

(* Pretty print floating point register *)
let ppFPreg r =
  if r < 0 || r > 31 then raise (Failure (sprintf "Unknown register value %d." r))
    else us"f" ^. ustring_of_int r

(* Make space *)
let space op n =  
  let oplen = Ustring.length op in
  Ustring.make (if n > oplen then n - oplen else 1) (uc ' ') 

(* Pretty print an instruction with different number of arguments *)
let pp1arg n map op a1  = op ^. space op n ^. a1 
let pp2arg n map op a1 a2 = op ^. space op n  ^. a1 ^. us"," ^. a2
let pp3arg n map op a1 a2 a3 = op ^. space op n  ^. a1 ^. us"," ^. a2 ^. us"," ^. a3
let pp3arg_addr n map op a1 a2 a3  = op ^. space op n  ^. a1 ^. us"," ^. a3 ^. us"(" ^. a2 ^. us")"
let pp4arg n map op a1 a2 a3 a4 = op ^. space op n  ^. a1 ^. us"," ^. a2 ^. us"," ^. a3 ^. us"," ^. a4 
let pp5arg n map op a1 a2 a3 a4 a5 = op ^. space op n  ^. a1 ^. us"," ^. a2 ^. us"," ^. a3 ^. us"," ^. a4
                                  ^. us"," ^. a5 


(* Returns [str] if [opt] is true, else returns an empty string *)
let stropt opt str = if opt then us"," ^. str else us""

(* Pretty print an address with symbol (if exists) *)
let ppAddr addr map = 
  us(sprintf "%x" addr) ^. 
    try us" <" ^. ustring_of_sid (IntMap.find addr map) ^. us">" with Not_found -> us""


(**************** Exported functions *******************************)

let parse str = []

let sprint_inst_conf n map pc inst = 
    match inst with
  | IAbsJmp(fi,imm25,op) ->
      let addr = ((sign_ext imm25 25) lsl 1) + pc in      
      pp1arg n map (ppAbsJmp op) (ppAddr addr map)
  | ICondJmp(fi,rs1,rs2,imm12,op) ->            
      let addr = ((sign_ext imm12 12) lsl 1) + pc in
      pp3arg n map (ppCondJmp op) (ppXreg rs1) (ppXreg rs2) (ppAddr addr map)
  | IIndJmp(fi,rd,rs1,imm12,op) -> (
      if op = OpRDNPC then pp1arg n map (ppIndJmp op) (ppXreg rd)
      else if imm12 = 0 then pp2arg n map (ppIndJmp op) (ppXreg rd) (ppXreg rs1) 
      else pp3arg n map (ppIndJmp op) (ppXreg rd) (ppXreg rs1) (ustring_of_int (sign_ext imm12 12))    )
  | ILoad(fi,rd,rs1,imm12,op) ->
      pp3arg_addr n map (ppLoad op) (ppXreg rd) (ppXreg rs1) (ustring_of_int (sign_ext imm12 12))
  | IStore(fi,rs1,rs2,imm12,op) ->
      pp3arg_addr n map (ppStore op) (ppXreg rs2) (ppXreg rs1) (ustring_of_int (sign_ext imm12 12))
      (* Note the reversed order for rs1 and rs2 *)
  | IAtomic(fi,rd,rs1,rs2,op) ->
      pp3arg n map (ppAtomic op) (ppXreg rd) (ppXreg rs1) (ppXreg rs2)
  | ICompImm(fi,rd,rs1,immv,op) -> 
      pp3arg n map (ppCompImm op) (ppXreg rd) (ppXreg rs1) (ustring_of_int (sign_ext immv 12))
  | ICompReg(fi,rd,rs1,rs2,op) ->
      pp3arg n map (ppCompReg op) (ppXreg rd) (ppXreg rs1) (ppXreg rs2)
  | IFPLoad(fi,rd,rs1,imm12,op) ->
      pp3arg_addr n map (ppFPLoad op) (ppXreg rd) (ppXreg rs1) (ustring_of_int (sign_ext imm12 12))
  | IFPStore(fi,rs1,rs2,imm12,op) ->
      pp3arg_addr n map (ppFPStore op) (ppXreg rs2) (ppXreg rs1) (ustring_of_int (sign_ext imm12 12))  
  | IFPComp(fi,rd,rs1,rs2,rm,op) ->
      pp4arg n map (ppFPComp op) (ppXreg rd) (ppXreg rs1) (ppXreg rs2) (stropt (useRM op) (ppRM rm))
  | IFPComp3(fi,rd,rs1,rs2,rs3,rm,op) ->
      pp5arg n map (ppFPComp3 op) (ppXreg rd) (ppXreg rs1) (ppXreg rs2) (ppXreg rs3) (ppRM rm)
  | IMiscMem(fi,rd,rs1,imm12,op) ->
      
  | _ -> us""
 
(*| IMiscMem    of info * rd  * rs1 * imm12 * opMiscMem            (* Misc memory instructions *)
| ISys        of info * rd  * opSys                              (* System instructions *)
*)

  

let sprint_inst = sprint_inst_conf 8 (IntMap.empty)






