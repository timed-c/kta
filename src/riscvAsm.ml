

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
let ppAbsJmp op = 
  match op with 
  | OpJ   -> us"j"
  | OpJAL -> us"jal"
  
(* Pretty print conditional jumps *)
let ppCondJmp op =
  match op with
  | OpBEQ  -> us"beq"
  | OpBNE  -> us"bne"
  | OpBLT  -> us"blt"
  | OpBGE  -> us"bge"
  | OpBLTU -> us"bltu" 
  | OpBGEU -> us"bgeu"

(* Pretty print indirect jumps *)
let ppIndJmp op =
  match op with
  | OpJALR_C -> us"jalr.c"
  | OpJALR_R -> us"jalr.r"
  | OpJALR_J -> us"jalr.j"
  | OpRDNPC  -> us"rdnpc"

(* Pretty print loads *)
let ppLoad op = 
  match op with 
  | OpLB -> us"lb"
  | OpLH -> us"lh"
  | OpLW -> us"lw"
  | OpLD -> us"ld"
  | OpLBU -> us"lbu"
  | OpLHU -> us"lhu"
  | OpLWU-> us"lwu"

(* Pretty print stores *)
let ppStore op =
  match op with 
  | OpSB -> us"sb"
  | OpSH -> us"sh"
  | OpSW -> us"sw"
  | OpSD -> us"sd"


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
  | _ -> us""

  

let sprint_inst = sprint_inst_conf 8 (IntMap.empty)






