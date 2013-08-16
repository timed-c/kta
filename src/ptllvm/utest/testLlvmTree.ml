


open Ustring.Op
open Utest
open LlvmAst


let main = 
  init "Test llvm tree module.";
  
  (* Extracted test functions *)
  let integerloops_ast = LlvmDecode.bcfile2ast "ccode/integerloops.bc" in
  let f_looptest2 = LlvmUtils.get_fun integerloops_ast (usid "looptest2") in
  
  (* Test tree creation *)
  let LLFunc(_,_,blocks) = f_looptest2 in
  let LLBlock(_,insts) = List.assoc (usid "for.body") blocks in
  
  let forest = LlvmTree.make insts (LlvmUtils.used_in_another_block f_looptest2) in 
  let res = LlvmPPrint.pp_forest forest in
  let exp = us"TExp(%add = add i32 %mul, %j.05)\n" ^.
            us"    TExp(%mul = mul i32 %i.06, %j.05)\n" ^.
            us"        TId(%i.06)\n" ^.
            us"        TId(%j.05)\n" ^.
            us"    TId(%j.05)\n" ^. 
            us"TExp(%inc = add i32 %i.06, 1)\n" ^.
            us"    TId(%i.06)\n" ^.
            us"    TConst(1)\n" ^.
            us"TExp(br i1 %exitcond, label %for.end, label %for.body)\n" ^.
            us"    TExp(%exitcond = icmp eq i32 %inc, %k)\n" ^.
            us"        TId(%inc)\n" ^.
            us"        TId(%k)\n" in
  test_ustr "Test SSA tree construction." res exp;
   

  result()

