

open Ustring.Op
open Utest
open LlvmDecode
open LlvmUtils
open LlvmPPrint
open Printf

let main = 
  init "Test llvm decode and pretty print";

  let ast = LlvmDecode.bcfile2ast "unittest/testcode/smallloop.bc" in
  uprint_endline (LlvmPPrint.pprint_module ast);    

  let btime b1 b2 = 1 in 
  let fname = usid "looptest2" in
  let args = [const_int_val 32 10] in
  let (t,v) = LlvmEval.eval_fun ast btime fname args (-1) in
  uprint_endline (us"*********");
  printf "time: %d\n" t;
  uprint_endline (us"val: " ^.  (pprint_val v));

  test_str "Instruction j" "foo" "foo";
  result()

