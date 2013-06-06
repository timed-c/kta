

open Ustring.Op
open Utest
open LlvmDecode

let main = 
  init "Test llvm decode and pretty print";
  bcfile2ast "unittest/testcode/smallloop.bc";
  test_str "Instruction j" "foo" "foo";
  result()

