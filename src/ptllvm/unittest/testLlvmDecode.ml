

open Ustring.Op
open Utest
open LlvmDecode

let main = 
  init "Test llvm decode and pretty print";
  let ast = LlvmDecode.bcfile2ast "unittest/testcode/smallloop.bc" in
  uprint_endline (LlvmPPrint.pprint_module ast);  

  test_str "Instruction j" "foo" "foo";
  result()

