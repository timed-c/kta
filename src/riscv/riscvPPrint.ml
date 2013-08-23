
open Ustring.Op
open RiscvISA
open Printf

module IntMap = Map.Make(struct type t = int let compare = compare end)  
type addr2sid = sid IntMap.t 

(* Needs to consider instrutions "move" (actually addi) beqz etc. *)

(* Space separation between instruction and operands *)
let sep = 8

(* Sign extend value [v] that has [n] significant bits *)
let sign_ext v n = if ((v lsr (n-1)) land 1) = 0 
                   then v else (-1 lsl n) lor v

(* Pretty print absolute jumps *)
let pp_abs_jmp op = us (match op with  
   | OpJ   -> "j" | OpJAL -> "jal")
  
(* Pretty print conditional jumps *)
let pp_cond_jmp op = us (match op with
  | OpBEQ  -> "beq"  | OpBNE  -> "bne" | OpBLT  -> "blt" | OpBGE  -> "bge"
  | OpBLTU -> "bltu" | OpBGEU -> "bgeu")

(* Pretty print indirect jumps *)
let pp_ind_jmp op = us (match op with
  | OpJALR_C -> "jalr.c" | OpJALR_R -> "jalr.r" | OpJALR_J -> "jalr.j"
  | OpRDNPC  -> "rdnpc")

(* Pretty print loads *)
let pp_load op = us (match op with 
  | OpLB  -> "lb"  | OpLH  -> "lh"  | OpLW  -> "lw"  | OpLD -> "ld"
  | OpLBU -> "lbu" | OpLHU -> "lhu" | OpLWU -> "lwu")

(* Pretty print stores *)
let pp_store op = us (match op with 
  | OpSB -> "sb" | OpSH -> "sh" | OpSW -> "sw" | OpSD -> "sd")

(* Pretty printing atomic memory instructions *)
let pp_atomic op = us (match op with 
  | OpAMOADD_W  -> "amoadd.w"  | OpAMOSWAP_W -> "amoswap.w"| OpAMOAND_W  -> "amoand.w" 
  | OpAMOOR_W   -> "amoor.w"   | OpAMOMIN_W  -> "amomin.w" | OpAMOMAX_W  -> "amomax.w" 
  | OpAMOMINU_W -> "amominu.w" | OpAMOMAXU_W -> "amomaxu.w"| OpAMOADD_D  -> "amoadd.d" 
  | OpAMOSWAP_D -> "amoswap.d" | OpAMOAND_D  -> "amoand.d" | OpAMOOR_D   -> "amoor.d" 
  | OpAMOMIN_D  -> "amomin.d"  | OpAMOMAX_D  -> "amomax.d" | OpAMOMINU_D -> "amominu.d" 
  | OpAMOMAXU_D -> "amomaxu.d")

(* Pretty printing integer register-register instructions *)
let pp_comp_reg op = us (match op with
  | OpADD   -> "add"   | OpSUB  -> "sub"  | OpSLL    -> "sll"    | OpSLT   -> "slt"   | OpSLTU -> "sltu" 
  | OpXOR   -> "xor"   | OpSRL  -> "srl"  | OpSRA    -> "sra"    | OpOR    -> "or"    | OpAND  -> "and" 
  | OpMUL   -> "mul"   | OpMULH -> "mulh" | OpMULHSU -> "mulhsu" | OpMULHU -> "mulhu" | OpDIV  -> "div"
  | OpDIVU  -> "divu"  | OpREM  -> "rem"  | OpREMU   -> "remu"   | OpADDW  -> "addw"  | OpSUBW -> "subw" 
  | OpSLLW  -> "sllw"  | OpSRLW -> "srlw" | OpSRAW   -> "sraw"   | OpMULW  -> "mulw"  | OpDIVW -> "divw"
  | OpDIVUW -> "divuw" | OpREMW -> "remw" | OpREMUW  -> "remuw")

(* Pretty print integer intermediate-register instructions *)
let pp_comp_imm op = us (match op with
  | OpADDI  -> "addi"  | OpSLLI  -> "slli"  | OpSLTI  -> "slti" | OpSLTIU -> "sltiu"
  | OpXORI  -> "xori"  | OpSRLI  -> "srli"  | OpSRAI  -> "srai" | OpORI   -> "ori"   
  | OpANDI  -> "andi"  | OpLUI   -> "lui"   | OpADDIW -> "addiw"| OpSLLIW -> "slliw"
  | OpSRLIW -> "srliw" | OpSRAIW -> "sraiw")


(* Pretty printing misc memory instructions  *)
let pp_misc_mem op = us (match op with 
  | OpFENCE_I -> "fence.i"  | OpFENCE -> "fence")

(* Pretty printing sys instructions *)
let pp_sys op = us (match op with 
  | OpSYSCALL -> "syscall"  | OpBREAK     -> "break"      | OpRDCYCLE -> "rdcycle"   
  | OpRDTIME  -> "rdtime"   | OpRDINSTRET -> "rdinstret")


(* Pretty print general purpose register *)
let pp_x_reg r =
  if r < 0 || r > 31 then raise (Failure (sprintf "Unknown register value %d." r))
    else us"x" ^. ustring_of_int r


(* Make space *)
let space op n =  
  let oplen = Ustring.length op in
  Ustring.make (if n > oplen then n - oplen else 1) (uc ' ') 

(* Pretty print an instruction with different number of arguments *)
let pp1arg n op a1  = op ^. space op n ^. a1 
let pp2arg n op a1 a2 = op ^. space op n  ^. a1 ^. us"," ^. a2
let pp3arg n op a1 a2 a3 = op ^. space op n  ^. a1 ^. us"," ^. a2 ^. us"," ^. a3
let pp3arg_addr n op a1 a2 a3  = op ^. space op n  ^. a1 ^. us"," ^. a3 ^. us"(" ^. a2 ^. us")"



(* Pretty print an address with symbol (if exists) *)
let pp_addr addr map = 
  us(sprintf "%x" addr) ^. 
    try us" <" ^. ustring_of_sid (IntMap.find addr map) ^. us">" with Not_found -> us""




let pprint_minst_conf n map pc inst = 
    match inst with
  | MIUncondJmp(fi,op,imm25) ->
      let addr = ((sign_ext imm25 25) lsl 1) + pc in      
      pp1arg n (pp_abs_jmp op) (pp_addr addr map)
  | MICondJmp(fi,op,rs1,rs2,imm12) ->            
      let addr = ((sign_ext imm12 12) lsl 1) + pc in
      pp3arg n (pp_cond_jmp op) (pp_x_reg rs1) (pp_x_reg rs2) (pp_addr addr map)
  | MIIndJmp(fi,op,rd,rs1,imm12) -> (
      if op = OpRDNPC then pp1arg n (pp_ind_jmp op) (pp_x_reg rd)
      else if imm12 = 0 then pp2arg n (pp_ind_jmp op) (pp_x_reg rd) (pp_x_reg rs1) 
      else pp3arg n (pp_ind_jmp op) (pp_x_reg rd) (pp_x_reg rs1) (ustring_of_int (sign_ext imm12 12))    )
  | MILoad(fi,op,rd,rs1,imm12) ->
      pp3arg_addr n (pp_load op) (pp_x_reg rd) (pp_x_reg rs1) (ustring_of_int (sign_ext imm12 12))
  | MIStore(fi,op,rs1,rs2,imm12) ->
      pp3arg_addr n (pp_store op) (pp_x_reg rs2) (pp_x_reg rs1) (ustring_of_int (sign_ext imm12 12))
      (* Note the reversed order for rs1 and rs2 *)
  | MIAtomic(fi,op,rd,rs1,rs2) ->
      pp3arg n (pp_atomic op) (pp_x_reg rd) (pp_x_reg rs1) (pp_x_reg rs2)
  | MICompImm(fi,op,rd,rs1,immv) -> 
      pp3arg n (pp_comp_imm op) (pp_x_reg rd) (pp_x_reg rs1) (ustring_of_int (sign_ext immv 12))
  | MICompReg(fi,op,rd,rs1,rs2) ->
      pp3arg n (pp_comp_reg op) (pp_x_reg rd) (pp_x_reg rs1) (pp_x_reg rs2)
  | MIMiscMem(fi,op,rd,rs1,imm12) -> 
      pp3arg n (pp_misc_mem op) (pp_x_reg rd) (pp_x_reg rs1) (ustring_of_int imm12)
  | MISys(fi,op,rd) ->
      pp1arg n (pp_sys op) (pp_x_reg rd)

(* Pretty print label *) 
let pp_label l = ustring_of_sid l

(* Pretty print SSA register and machine register (if assigned) *)
let pp_sreg (s,i) =
  us"%" ^. ustring_of_sid s ^. 
  if i = -1 then us"" else us"->r" ^. ustring_of_int i

(**************** Exported functions *******************************)

let minst = pprint_minst_conf sep (IntMap.empty)

let sinst inst =
  match inst with
  | SIUncondJmp(op,l) -> 
      pp1arg sep (pp_abs_jmp op) (pp_label l)
  | SICondJmp(op,rs1,rs2,l) ->    
      pp3arg sep (pp_cond_jmp op) (pp_sreg rs1) (pp_sreg rs2) (pp_label l)
  | SIIndJmp(_,_,_,_) -> failwith "Not implemented"  
  | SILoad(_,_,_,_) -> failwith "Not implemented"
  | SIStore(_,_,_,_) -> failwith "Not implemented"
  | SIAtomic(_,_,_,_) -> failwith "Not implemented"
  | SICompImm(op,rd,rs1,immv) ->
      pp3arg sep (pp_comp_imm op) (pp_sreg rd) (pp_sreg rs1) (ustring_of_int (sign_ext immv 12))
  | SICompReg(op,rd,rs1,rs2) ->
      pp3arg sep (pp_comp_reg op) (pp_sreg rd) (pp_sreg rs1) (pp_sreg rs2)
  | SIMiscMem(_,_,_,_) -> failwith "Not implemented"
  | SISys(_,_) -> failwith "Not implemented"
  

let sinst_list insts =
  List.fold_left (fun a i -> a ^. sinst i ^. us"\n") (us"") insts





