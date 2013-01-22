

open Riscv_isa
open Printf

(*************** Exported types and exceptions ********************)

exception Incomplete_parcel
exception Not_enough_parcels

type big_endian = bool
type parcel = int

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
let d_rs3 high low = ((high land 1) lsl 4) land (low lsr 12) 

(* Decoding of 12 bit immediate for I-Type *)
let d_imI high low = ((high land 0b111111) lsl 6) land (low lsr 10) 

(* Decoding of 12 bit immediate for B-Type *)
let d_imB high low = ((high lsr 11) lsl 7) land ((high land 1) lsl 6) 
                     land (low lsr 10)

(* Decoding of 20 bit immediate for L-Type *)
let d_imL high low = ((high land 0b11111111111) lsl 9) land (low lsr 7)

(* Decoding of 25 bit jump offset for J-Type *)
let d_offJ high low = (high lsl 9) land (low lsr 7)

(* Decoding of 10 bit funct opcode field for R-Type *)
let d_funR high low = ((high land 1) lsl 9) land (low lsr 7)

(* Decoding of 5 bit funct opcode field for R4-Type *)
let d_funR4 low = (low lsr 7) land 0b11111

(* Decoding of 3 bit funct opcode field for I and B Types *)
let d_funIB low = (low lsr 7) land 0b111

(* Error reporting if we have an unknown instruction *)
let failinst h l = 
  failwith (sprintf "ERROR: Unknown instruction %x,%x,%x,%x\n" 
            (h lsr 8) (h land 0xff) (l lsr 8) (l land 0xff))

(* Decodes one 32 bit instruction *)
let decode_32inst h l =
  match d_op l with
    (* Absolute Jump Instructions *)
  | 0b1100111 -> IAbsJmp(NoI, d_offJ h l, OpJ)
  | 0b1101111 -> IAbsJmp(NoI, d_offJ h l, OpJAL)
    (* Conditional Jump Instructions *)
  | 0b1100011 -> 
      let op = match d_funIB l with 0b000 -> OpBEQ | 0b001 -> OpBNE  | 0b100 -> OpBLT |
                                    0b101 -> OpBGE | 0b110 -> OpBLTU | 0b111 -> OpBGEU | 
                                    _ -> failinst h l in
    ICondJmp(NoI, d_rs1 h, d_rs2 h, d_imB h l, op) 
    (* Indirect Jump Instructions *)
  | 0b1101011 -> 
      let op = match d_funIB l with 0b000 -> OpJALRC | 0b001 -> OpJALRR | 
                                    0b010 -> OpJALRJ | 0b100 -> OpRDNPC | 
                                    _ -> failinst h l in
      IIndJmp(NoI, d_rd h, d_rs1 h, d_imI h l, op)
    (* Load Memory Instructions *)
  | 0b0000011 -> 
      let op = match d_funIB l with 0b000 -> OpLB  | 0b001 -> OpLH  | 0b010 -> OpLW |
                                    0b011 -> OpLD  | 0b100 -> OpLBU | 0b101 -> OpLHU |
                                    0b110 -> OpLWU | _ -> failinst h l in
      ILoad(NoI, d_rd h, d_rs1 h, d_imI h l, op)
    (* Store Memory Instructions *)
  | 0b0100011 ->
      let op = match d_funIB l with 0b000 -> OpSB | 0b001 -> OpSH | 
                                    0b010 -> OpSW | 0b011 -> OpSD | _ -> failinst h l in
      IStore(NoI, d_rs1 h, d_rs2 h, d_imB h l, op)
  | _ -> failinst h l




(* Local function for decoding one instruction. Returns a triple 
   consisting of the new parcel list, number of parcels consumed, and
   the format type value *)
let decode_inst parcels =
  match parcels with
  | p1::ps when (p1 land 0b11111) = 0b11111 -> 
       failwith "ERROR: Instructions larger than 32-bit are not yet supported."
  | p1::ps when (p1 land 0b11) <> 0b11 ->
       failwith "ERROR: 16-bit instructions are not yet supported."
  | low::high::ps -> "TODO"
  | _ -> raise Not_enough_parcels


(**************** Exported functions *******************************)

let parcels_of_bytes bigendian lst =
  let rec loop lst acc =  
    match bigendian,lst with
    | true, (high::low::rest) -> loop rest (((high lsl 8) lor low)::acc)
    | false, (low::high::rest) -> loop rest (((high lsl 8) lor low)::acc)
    | _,[] -> List.rev acc
    | _,_ -> raise Incomplete_parcel 
  in loop lst []    


let bytes_of_parcels bige lst = 
  List.rev (List.fold_left 
               (fun acc p -> if bige then (p land 0xff)::(p lsr 8)::acc
                             else (p lsr 8)::(p land 0xff)::acc) [] lst)


  
  


let decode bin = []



let encode inst = []
  

