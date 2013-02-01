

open Ustring.Op
open RiscvISA
open Printf

(*************** Exported types and exceptions ********************)

type big_endian = bool



(*************** Local types and exceptions ***********************)


(*************** Local functions **********************************)

(* Sign extend value [v] that has [n] significant bits *)
let sign_ext v n = if ((v lsr (n-1)) land 1) = 0 
                   then v else (-1 lsl n) lor v
  
(* Pretty print conditional jumps *)
let ppCondJmp op =
  match op with
  | OpBEQ  -> us"beq"
  | OpBNE  -> us"bne"
  | OpBLT  -> us"blt"
  | OpBGE  -> us"bge"
  | OpBLTU -> us"bltu" 
  | OpBGEU -> us"bgeu"

let ppAbsJmp op = 
  match op with 
  | OpJ   -> us"j"
  | OpJAL -> us"jal"

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
  | _ -> us""
  

let sprint_inst = sprint_inst_conf 8 (IntMap.empty)






