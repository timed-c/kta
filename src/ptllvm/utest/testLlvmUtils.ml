

open Ustring.Op
open Utest
open LlvmAst


let main = 
  init "Test llvm utils.";

  (* Test id used in another block *)
  let ast = LlvmDecode.bcfile2ast "ccode/integerloops.bc" in
  let f = LlvmUtils.get_fun ast (usid "looptest2") in
  let lset = LlvmUtils.used_in_another_block f in
  let expected = List.map usid ["k";"i.06";"inc";"j.05";"add";"j.0.lcssa"] in
  test_list "Function used_in_another_block()" 
    (SidSet.elements lset) expected ustring_of_sid;

  result()
