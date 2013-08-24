

open Ustring.Op
open Utest
open LlvmAst


let main = 

  init "Test RISC-V instruction selection.";
  
  (* Extracted test functions *)
  let m_integerloops = LlvmDecode.bcfile2ast "ccode/integerloops.bc" in
  let m_arithemtic = LlvmDecode.bcfile2ast "ccode/arithmetic.bc" in
  let f_looptest2 = LlvmUtils.get_func "looptest2" m_integerloops in
  let f_arith1 = LlvmUtils.get_func "arith1" m_arithemtic in
  
  (* Test maximal munch on one block *)
  let LLBlock(_,insts) = LlvmUtils.get_block "for.body" f_looptest2  in
  let forest = LlvmTree.make insts (LlvmUtils.used_in_another_block f_looptest2) in 
  let insts = RiscvInstSelect.maximal_munch forest 1 in
  let res = RiscvPPrint.sinst_list insts in
  let exp = us"mul     %mul,%i.06,%j.05\n" ^.
            us"add     %add,%mul,%j.05\n" ^.
            us"addi    %inc,%i.06,1\n" ^.
            us"beq     %inc,%k,for.end\n" ^.
            us"j       for.body\n" in
  test_ustr "Selecting mul,add,addi,beq,j" res exp;

  (* Test maximal munch on one block *)
  let LLBlock(_,insts) = LlvmUtils.get_block "entry" f_looptest2 in
  let forest = LlvmTree.make insts (LlvmUtils.used_in_another_block f_looptest2) in 
  let insts = RiscvInstSelect.maximal_munch forest 1 in
  let res = RiscvPPrint.sinst_list insts in 
  let exp = us"addi    %tmp#1,%->r0,1\n" ^.
            us"blt     %tmp#1,%k,for.body\n" ^.
            us"j       for.end\n" in
  test_ustr "Selecting addi,blt,j" res exp;


  (* Test maximal munch on an artihmetic block *)
  let LLBlock(_,insts) = LlvmUtils.get_block "entry" f_arith1 in
  let forest = LlvmTree.make insts (LlvmUtils.used_in_another_block f_arith1) in 
  let insts = RiscvInstSelect.maximal_munch forest 1 in
  let res = RiscvPPrint.sinst_list insts in 
  let exp = us"addi    %sub4,%z,-1\n" ^.
            us"mul     %mul3,%y,%y\n" ^.
            us"addi    %tmp#1,%->r0,23\n" ^.
            us"mul     %mul,%x,%tmp#1\n" ^.
            us"mul     %mul1,%mul,%x\n" ^.
            us"mul     %mul2,%mul1,%x\n" ^.
            us"div     %div,%mul2,%y\n" ^.
            us"sub     %sub,%div,%mul3\n" ^.
            us"sll     %shl,%sub,%z\n" ^.
            us"add     %add,%shl,%y\n" ^.
            us"sra     %shr,%add,%sub4\n" ^.
            us"rem     %rem,%shr,%y\n" ^.
            us"jalr.r  %->r0,%,%rem\n" in
  test_ustr "Selecting addi,mul,div,sub,sll,sra,rem,jalr.r" res exp;

(*  uprint_endline (LlvmPPrint.llfunc f_arith1);
  print_endline "--------------";
  uprint_endline (LlvmPPrint.llforest forest); 
  print_endline "--------------";
  uprint_endline res;  *)
 

  result()







