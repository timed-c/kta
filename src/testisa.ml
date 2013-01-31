

open RiscvISA
open Printf
open Ustring.Op



(*  4: 0040 b063   |  beqz    ra,5c            what does z in beqz mean?
   00000000 01000000 10110000 01100011          
   Encoded using big-endian
*)   
let beqz = "\xb0\x63\x00\x40" 


(* 14: 0044 6063   |  beq     ra,v0,44 
   00000000 01000100 01100000 01100011
*)

let print_hex str = 
  String.iter (fun c -> printf "%x," (int_of_char c)) str;
  print_endline ""

let main = 
  let prog = beqz in
  print_hex prog;
  print_endline "-------";
  let (code,i) = RiscvBin.decode true prog 0 in 
  Std.print code;
  print_hex (RiscvBin.encode true code);
  ()  




