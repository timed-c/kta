

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
  let f_arith2 = LlvmUtils.get_func "arith2" m_arithemtic in
  let f_logic1 = LlvmUtils.get_func "logic1" m_arithemtic in
  let f_logic2 = LlvmUtils.get_func "logic2" m_arithemtic in
  let f_comp1 = LlvmUtils.get_func "comp1" m_arithemtic in
  
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


  (* Test maximal munch on an arithmetic block *)
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


  (* Test maximal munch on an arithmetic block (unsigned integer operations) *)
  let LLBlock(_,insts) = LlvmUtils.get_block "entry" f_arith2 in
  let forest = LlvmTree.make insts (LlvmUtils.used_in_another_block f_arith2) in 
  let insts = RiscvInstSelect.maximal_munch forest 1 in
  let res = RiscvPPrint.sinst_list insts in 
  let exp = us"mul     %mul,%x,%x\n" ^.
            us"add     %add,%mul,%y\n" ^.
            us"divu    %div,%add,%z\n" ^.
            us"sub     %sub,%mul,%y\n" ^.
            us"remu    %rem,%sub,%z\n" ^.
            us"add     %add2,%rem,%div\n" ^.
            us"jalr.r  %->r0,%,%add2\n" in
  test_ustr "Selecting mul,add,divu,sub,remu,jalr.r" res exp;


  (* Test maximal munch on logic block *)
  let LLBlock(_,insts) = LlvmUtils.get_block "entry" f_logic1 in
  let forest = LlvmTree.make insts (LlvmUtils.used_in_another_block f_logic1) in 
  let insts = RiscvInstSelect.maximal_munch forest 1 in
  let res = RiscvPPrint.sinst_list insts in 
  let exp = us"and     %and,%y,%x\n" ^.
            us"sltiu   %tmp#1,%x,1\n" ^.
            us"xori    %notlhs,%tmp#1,1\n" ^.
            us"sltiu   %tmp#2,%y,1\n" ^.
            us"xori    %notrhs,%tmp#2,1\n" ^.
            us"and     %not.or.cond,%notrhs,%notlhs\n" ^.
            us"sltiu   %tmp#3,%z,1\n" ^.
            us"xori    %tobool2,%tmp#3,1\n" ^.
            us"and     %.tobool2,%tobool2,%not.or.cond\n" ^.
            us"addi    %land.ext,%.tobool2,0\n" ^.
            us"add     %add,%land.ext,%and\n" ^.
            us"jalr.r  %->r0,%,%add\n" in
  test_ustr "Selecting sltiu,xori,and" res exp;


  (* Test maximal munch on logic block *)
  let LLBlock(_,insts) = LlvmUtils.get_block "entry" f_logic2 in
  let forest = LlvmTree.make insts (LlvmUtils.used_in_another_block f_logic2) in 
  let insts = RiscvInstSelect.maximal_munch forest 1 in
  let res = RiscvPPrint.sinst_list insts in 
  let exp = us"srl     %shr,%x,%y\n" ^.
            us"or      %or,%z,%y\n" ^.
            us"and     %and,%or,%x\n" ^.
            us"xor     %xor,%and,%shr\n" ^.
            us"add     %add,%xor,%shr\n" ^.
            us"jalr.r  %->r0,%,%add\n" in
  test_ustr "Selecting srl,and,or,xor" res exp;

  (* Test maximal munch on large immediate values*)
  let LLBlock(_,insts) = LlvmUtils.get_block "entry" f_comp1 in
  let forest = LlvmTree.make insts (LlvmUtils.used_in_another_block f_comp1) in 
  let insts = RiscvInstSelect.maximal_munch forest 1 in
  let res = RiscvPPrint.sinst_list insts in 
  let exp = us"addi    %tmp#1,%->r0,2047\n" ^.
            us"mul     %mul2,%z,%tmp#1\n" ^.
            us"addi    %tmp#2,%->r0,-120\n" ^.
            us"mul     %mul1,%y,%tmp#2\n" ^.
            us"lui     %tmp#3,%->r0,30133\n" ^.
            us"addi    %tmp#4,%tmp#3,-1533\n" ^.
            us"lui     %tmp#5,%->r0,530\n" ^.
            us"addi    %tmp#6,%tmp#5,251\n" ^.
            us"mul     %mul,%x,%tmp#6\n" ^.
            us"add     %add,%mul,%tmp#4\n" ^.
            us"add     %add3,%add,%mul1\n" ^.
            us"add     %add4,%add3,%mul2\n" ^.
            us"jalr.r  %->r0,%,%add4\n" in
  test_ustr "Selecting large intermediate constants" res exp;

(*
  uprint_endline (LlvmPPrint.llfunc f_comp1);
  print_endline "--------------";
  uprint_endline (LlvmPPrint.llforest forest); 
  print_endline "--------------";
  uprint_endline res;  
*)

  result()


