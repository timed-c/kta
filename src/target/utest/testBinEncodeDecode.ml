




open Ustring.Op
open Printf
open Utest
open RiscvISA


let main = 
  init "Binary decoding and encoding of RISC-V instructions.";
  let encdec bige s = RiscvBin.encode bige (fst (RiscvBin.decode bige s 0)) in
  let encdeclist bige s = RiscvBin.encode_all bige (RiscvBin.decode_all bige s) in
  

  (*** Absolute Jumps ***)
  
  let s = "\xc7\x67\xff\xf9" in
  test_str "Instruction j" (encdec true s) s;

  let s = "\xc7\x6f\xff\xf9" in
  test_str "Instruction jal" (encdec true s) s;


  (*** Conditional branches ***)

  let s = "\x63\x38\x44\x00" in 
  test_str "Instruction beq with little-endian" (encdec false s) s;

  let s = "\x38\x63\x00\x44" in 
  test_str "Instruction beq with big-endian" (encdec true s) s; 

  let s = "\xb0\x63\x00\x40" in 
  test_str "Instruction beq" (encdec true s) s; 

  let s = "\x10\xe3\x06\x80" in 
  test_str "Instruction bne" (encdec true s) s;  

  let s = "\xc0\xe3\xf9\x0b" in 
  test_str "Instruction bne with negative address" (encdec true s) s;  

  let s = "\xc2\x63\xf8\x81" in 
  test_str "Instruction blt" (encdec true s) s;

  let s = "\x13\x63\x00\x44" in 
  test_str "Instruction bltu" (encdec true s) s;  

  let s = "\x2a\xe3\x00\x40" in 
  test_str "Instruction bge" (encdec true s) s;  

  let s = "\xeb\xe3\x00\xc4" in 
  test_str "Instruction bgeu" (encdec true s) s;  


  (*** Indirect jumps ***)

  let s = "\x00\x6b\x98\x80" in
  test_str "Instruction jalr.c" (encdec true s) s;

  let s = "\x01\x6b\x99\x80" in
  test_str "Instruction jalr.j" (encdec true s) s; 

  let s = "\x00\xeb\x99\x80" in
  test_str "Instruction jalr.r" (encdec true s) s; 

  let s = "\xfc\xeb\x99\xbf" in
  test_str "Instruction jalr.r negative offset" (encdec true s) s; 

  let s = "\x02\x6b\xf8\x00" in
  test_str "Instruction rdnpc" (encdec true s) s; 


  (*** Loads and Stores ***)

  let s = "\xd1\x03\x18\x90" in
  test_str "Instruction lw" (encdec true s) s; 

  let s = "\x11\x23\x07\xba" in
  test_str "Instruction sw" (encdec true s) s;  


  (*** Atomic memory instruction ***)

  let s = "\x01\x2b\x00\x44" in
  test_str "Instruction amoadd.w" (encdec true s) s; 


  (*** Integer computation instructions ***)

  let s = "\x3e\x13\x18\x5c" in
  test_str "Instruction xori" (encdec true s) s; 

  let s = "\x7e\x93\x18\x41" in
  test_str "Instruction srai" (encdec true s) s; 

  let s = "\x00\xb3\x08\x74" in
  test_str "Instruction sll" (encdec true s) s; 

  let s = "\x02\x33\x10\x44" in
  test_str "Instruction xor" (encdec true s) s; 

  let s = "\x05\xb3\x18\x44" in
  test_str "Instruction mulhu" (encdec true s) s; 
 

  (** Encode and decode a list of instructions **)

  
  let s = "\x02\x33\x10\x44\x11\x23\x07\xba\x00\x6b\x98\x80\xc2\x63\xf8\x81" in 
  test "Encode and decode a list of instructions" ((encdeclist true s) = s); 

  result()








