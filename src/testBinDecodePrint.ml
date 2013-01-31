

open Ustring.Op
open Utest
open RiscvISA

(* Special instructions that I need to parse:
   beqz, bnez *)


let main = 
  init "Binary decoding and pretty printing";
  let m = IntMap.add 0x66 (usid"boy") (IntMap.add 0x7c (usid"jumpid") (IntMap.empty)) in
  let inst pos code = RiscvAsm.sprint_inst pos (fst (RiscvBin.decode true code 0)) in
  let inst_conf1 pos code = RiscvAsm.sprint_inst_conf 6 m pos (fst (RiscvBin.decode true code 0)) in

  test_ustr "Instruction beq"  
             (inst 0x4  "\xb0\x63\x00\x40")  (us"beq     x1,x0,5c");

  test_ustr "Instruction bne"   
             (inst 0x74 "\x10\xe3\x06\x80")  (us"bne     x26,x0,7c");

  test_ustr "Instruction bne with symbol" 
             (inst_conf1 0x74 "\x10\xe3\x06\x80")  (us"bne   x26,x0,7c <jumpid>");

  test_ustr "Instruction bne with negative address"   
             (inst 0x2b0 "\xc0\xe3\xf9\x0b")  (us"bne     x4,x5,290"); 

  
  result()


  

