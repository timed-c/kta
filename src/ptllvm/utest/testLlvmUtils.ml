

open Ustring.Op
open Utest
open LlvmAst


let main = 
  init "Test llvm utils.";

  (* Extracted test functions *)
  let integerloops_ast = LlvmDecode.bcfile2ast "ccode/integerloops.bc" in
  let f_looptest2 = LlvmUtils.get_fun "looptest2" integerloops_ast  in

  (* Test id used in another block *)
  let lset = LlvmUtils.used_in_another_block f_looptest2 in
  let expected = List.map usid ["k";"i.06";"inc";"j.05";"add";"j.0.lcssa"] in
  test_list "Function used_in_another_block()" 
    (SidSet.elements lset) expected ustring_of_sid;

  (* Test count identifier uses *)
  let LLBlock(_,insts) = LlvmUtils.get_block "for.body" f_looptest2 in
  let count = LlvmUtils.count_ids_uses insts in 
  let pprint_pair (id,c) = (ustring_of_sid id) ^. us":" ^. ustring_of_int c in
  let expected = [(usid"j.05",2); (usid"i.06",2); (usid"mul",1); (usid"k",1);
                  (usid"inc",1); (usid"exitcond",1)] in
  test_list "Function count_ids_uses()" count expected pprint_pair;
  
  result()
