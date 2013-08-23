

open Ustring.Op
open Utest
open LlvmAst


let main = 

  init "Test RISC-V instruction selection.";
  
  (* Extracted test functions *)
  let integerloops_ast = LlvmDecode.bcfile2ast "ccode/integerloops.bc" in
  let f_looptest2 = LlvmUtils.get_fun "looptest2" integerloops_ast in
  
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


(*
  uprint_endline (LlvmPPrint.llmodule integerloops_ast);
  print_endline "--------------";
  uprint_endline (LlvmPPrint.llforest forest); 
  print_endline "--------------";
  uprint_endline res;  
*) 

  result()


