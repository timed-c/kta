

open Riscv_isa
open Printf
open Ustring.Op

let prog1 = [
(* rd=1, rs1=3, imm=352, op=LW *)
0b00001000;0b11000011;0b10101001;0b00000011;
(*   5|      5|            12|    3|     7| *)
]


let print_hex chars = 
  List.iter (fun c -> printf "%x," c) chars;
  print_endline ""

let main = 
  let _ = print_hex (Riscv_bin.bytes_of_parcels false 
                     (Riscv_bin.parcels_of_bytes false prog1)) in
  let _ = print_endline "-------" in
  let code = Riscv_bin.decode (Riscv_bin.parcels_of_bytes true prog1) in
  let _ = print_hex prog1 in
  let _ = print_hex (Riscv_bin.bytes_of_parcels true (Riscv_bin.encode code)) in
  let _ = uprint_endline (Riscv_asm.sprint true code) in
  ()  




