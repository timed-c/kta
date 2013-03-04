

open Ustring.Op
open Utest
open RiscvISA


let main = 
  init "Binary decoding and pretty printing";
  let m = IntMap.add 0x66 (usid"foo") (IntMap.add 0x7c (usid"jumpid") (IntMap.empty)) in
  let inst bige pos code = RiscvAsm.sprint_inst pos (fst (RiscvBin.decode bige code 0)) in
  let inst_conf1 pos code = RiscvAsm.sprint_inst_conf 6 m pos (fst (RiscvBin.decode true code 0)) in


  (*** Absolute Jumps ***)

  test_ustr "Instruction j"  
             (inst true 0x1cfc  "\xc7\x67\xff\xf9")  (us"j       418");
  test_ustr "Instruction jal"  
             (inst true 0x1cfc  "\xc7\x6f\xff\xf9")  (us"jal     418");
  

  (*** Conditional branches ***)

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


  (*** Indirect jumps ***)

  test_ustr "Instruction jalr.c"  
             (inst true 0xc0  "\x00\x6b\x98\x80")  (us"jalr.c  x19,x2");

  test_ustr "Instruction jalr.j"  
             (inst true 0x138  "\x01\x6b\x99\x80")  (us"jalr.j  x19,x6");

  test_ustr "Instruction jalr.r"  
             (inst true 0x138  "\x00\xeb\x99\x80")  (us"jalr.r  x19,x6");

  test_ustr "Instruction jalr.r negative offset"  
             (inst true 0x138  "\xfc\xeb\x99\xbf")  (us"jalr.r  x19,x6,-1");

  test_ustr "Instruction rdnpc"  
             (inst true 0x138  "\x02\x6b\xf8\x00")  (us"rdnpc   x31");


  (*** Loads and Stores ***)

  test_ustr "Instruction lw"  
             (inst true 0x1e48  "\xd1\x03\x18\x90")  (us"lw      x3,1076(x2)");

  test_ustr "Instruction sw"  
             (inst true 0x18d8  "\x11\x23\x07\xba")  (us"sw      x29,4(x30)");
  
  (*** Atomic memory instruction ***)

  test_ustr "Instruction amoadd.w"  
             (inst true 0x0  "\x01\x2b\x00\x44")  (us"amoadd.w x0,x1,x2");


  (*** Integer computation instructions ***)

  test_ustr "Instruction xori"  
             (inst true 0x128  "\x3e\x13\x18\x5c")  (us"xori    x3,x1,1807");

  test_ustr "Instruction srai"  
             (inst true 0x190  "\x7e\x93\x18\x41")  (us"srai    x3,x1,31");

  test_ustr "Instruction sll"  
             (inst true 0x80  "\x00\xb3\x08\x74")  (us"sll     x1,x1,x26");

  test_ustr "Instruction xor"  
             (inst true 0x174  "\x02\x33\x10\x44")  (us"xor     x2,x1,x2");

  test_ustr "Instruction mulhu"  
             (inst true 0x208  "\x05\xb3\x18\x44")  (us"mulhu   x3,x1,x2");
 

  result()


  

