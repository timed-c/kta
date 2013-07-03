
open Ustring.Op
open Utest
open LlvmAst
open LlvmDecode
open LlvmUtils
open LlvmPPrint
open Printf

(* Help function for printing function results *)
let pprint_res r = 
  match r with 
  | None -> us"None"
  | Some(v) -> pprint_const v 

(* Ignore timing in these tests *)
let btime b1 b2 = 1 

(* Function for testing integer functions *)
let test_llvm_int_res name res expint =
  let r = match res with 
    | Some(CInt(w,v)) -> Int64.compare v (Int64.of_int expint) = 0
    | _ -> false
  in
    test name r 

let test_llvm_void_res name res =
  let r = match res with 
    | Some(_) -> false
    | None -> true
  in
    test name r 



let main = 
  init "Test llvm decode and evaluation.";

  (* ------------------------------------------------------------------- *)

  let ast = LlvmDecode.bcfile2ast "unittest/testcode/integerloops.bc" in
  (*uprint_endline (LlvmPPrint.pprint_module ast);   *)

  (* Test looptest1 *)
  let fname = "looptest1" in
  let args = [const32 10] in
  let (t,res) = LlvmEval.eval_fun ast btime (usid fname) args (-1) in
  test_llvm_int_res "Function looptest1()" res 47;


  (* Test looptest2 *)
  let fname = "looptest2" in
  let args = [const32 10] in
  let (t,res) = LlvmEval.eval_fun ast btime (usid fname) args (-1) in
  test_llvm_int_res "Function looptest2()" res 7257600;

  (* ------------------------------------------------------------------- *)

  let ast = LlvmDecode.bcfile2ast "unittest/testcode/functioncalls.bc" in
  (* uprint_endline (LlvmPPrint.pprint_module ast);  *)

  (* Test functest1. Tests function call with several arguments. *)
  let fname = "functest1" in
  let args = [const32 77] in
  let (t,res) = LlvmEval.eval_fun ast btime (usid fname) args (-1) in
  test_llvm_int_res "Function functest1()" res 6006;

  (* Test functest2. Test a void function with no parameters *)
  let fname = "functest2" in
  let args = [] in
  let (t,res) = LlvmEval.eval_fun ast btime (usid fname) args (-1) in
  test_llvm_void_res "Function functest2()" res;


  (* ------------------------------------------------------------------- *)

  result()











