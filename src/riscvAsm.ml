

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
  | _ -> us""
 

  

let sprint_inst = sprint_inst_conf 8 (IntMap.empty)






