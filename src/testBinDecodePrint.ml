

open Ustring.Op
open Utest
open RiscvISA

(* Special instructions that I need to parse:
   beqz, bnez *)


let main = 
  init "Binary decoding and pretty printing";
  let m = IntMap.add 0x66 (usid"foo") (IntMap.add 0x7c (usid"jumpid") (IntMap.empty)) in
  let inst bige pos code = RiscvAsm.sprint_inst pos (fst (RiscvBin.decode bige code 0)) in
  let inst_conf1 pos code = RiscvAsm.sprint_inst_conf 6 m pos (fst (RiscvBin.decode true code 0)) in


  (* Unconditional Jumps *)

  (* test_ustr "Instruction jal"  
             (inst true 0x1cfc  "\xc7\x6f\xff\xf9")  (us"jal     418"); *)
  

  (* Conditional branches *)

  test_ustr "Instruction beq with little-endian"  
             (inst false 0x3c  "\x63\x38\x44\x00")  (us"beq     x1,x2,58"); 

  test_ustr "Instruction beq with big-endian"  
             (inst true 0x3c  "\x38\x63\x00\x44")  (us"beq     x1,x2,58"); 

  test_ustr "Instruction beq"  
             (inst true 0x4  "\xb0\x63\x00\x40")  (us"beq     x1,x0,5c"); 

  test_ustr "Instruction bne"   
             (inst true 0x74 "\x10\xe3\x06\x80")  (us"bne     x26,x0,7c");

  test_ustr "Instruction bne with symbol" 
             (inst_conf1 0x74 "\x10\xe3\x06\x80")  (us"bne   x26,x0,7c <jumpid>");

  test_ustr "Instruction bne with negative address"   
             (inst true 0x2b0 "\xc0\xe3\xf9\x0b")  (us"bne     x4,x5,290"); 

  test_ustr "Instruction blt"   
             (inst true 0xe44 "\xc2\x63\xf8\x81")  (us"blt     x2,x0,e24");

  test_ustr "Instruction bltu"   
             (inst true 0x154 "\x13\x63\x00\x44")  (us"bltu    x1,x2,15c");

  test_ustr "Instruction bge"   
             (inst true 0x3a4 "\x2a\xe3\x00\x40")  (us"bge     x1,x0,3b8");

  test_ustr "Instruction bgeu"   
             (inst true 0x1654 "\xeb\xe3\x00\xc4")  (us"bgeu    x3,x2,16c8");

  
  result()


  

