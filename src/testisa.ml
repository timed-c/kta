

open RiscvISA
open Printf
open Ustring.Op

(*
let prog1 = [
(* rd=1, rs1=3, imm=352, op=LW *)
0b00001000;0b11000011;0b10101001;0b00000011;
(*   5|      5|            12|    3|     7| *)
]*)
let prog1 = "\x00\x23\x82\x12"


let print_hex str = 
  String.iter (fun c -> printf "%x," (int_of_char c)) str;
  print_endline ""

let main = 
  print_hex prog1;
  print_endline "-------";
  let code = RiscvBin.decode true prog1 in 
  print_hex (RiscvBin.encode true code);
  uprint_endline (RiscvAsm.sprint true code);
  ()  




