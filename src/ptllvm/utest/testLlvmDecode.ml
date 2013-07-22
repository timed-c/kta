
open Ustring.Op
open Utest
open LlvmAst
open LlvmDecode
open LlvmUtils
open LlvmPPrint
open LlvmEval
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
    | Some(VConst(CInt(w,v))) -> 
      let r = Int64.compare v (Int64.of_int expint) = 0 in
      if r then true else (printf "Wrong result. Returned %s\n" (Int64.to_string v); r)
    | _ -> 
      printf "Expected value %d, but received a void result.\n" expint;
      false
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

  (* -------------------- INTEGER LOOPS ------------------------------- *)

  let ast = LlvmDecode.bcfile2ast "utest/testcode/integerloops.bc" in
  (*uprint_endline (LlvmPPrint.pprint_module ast);   *)

  (* Test looptest1 *)
  let fname = "looptest1" in
  let args = [v32 10] in
  let (t,res) = LlvmEval.eval_fun ast btime (usid fname) args (-1) in
  test_llvm_int_res "Function looptest1()" res 47;

  (* Test looptest2 *)
  let fname = "looptest2" in
  let args = [v32 10] in
  let (t,res) = LlvmEval.eval_fun ast btime (usid fname) args (-1) in
  test_llvm_int_res "Function looptest2()" res 7257600;

  (* ------------------ FUNCTION CALLS -------------------------------- *)

  let ast = LlvmDecode.bcfile2ast "utest/testcode/functioncalls.bc" in
  (* uprint_endline (LlvmPPrint.pprint_module ast);  *)

  (* Test functest1. Tests function call with several arguments. *)
  let fname = "functest1" in
  let args = [v32 77] in
  let (t,res) = LlvmEval.eval_fun ast btime (usid fname) args (-1) in
  test_llvm_int_res "Function functest1()" res 6006;

  (* Test functest2. Test a void function with no parameters *)
  let fname = "functest2" in
  let args = [] in
  let (t,res) = LlvmEval.eval_fun ast btime (usid fname) args (-1) in
  test_llvm_void_res "Function functest2()" res;

  (* ---------------------- ARRAYS ---------------------------------- *)

  let ast = LlvmDecode.bcfile2ast "utest/testcode/arrays.bc" in
  (* uprint_endline (LlvmPPrint.pprint_module ast); *)

  (* Test addnums)  *)
  let fname = "addnums" in
  let args = [v32 7] in
  let (t,res) = LlvmEval.eval_fun ast btime (usid fname) args (-1) in
  test_llvm_int_res "Function addnums()" res 215;

  (* Test simple_array_access.  *)
  let fname = "simple_array_access" in
  let args = [v32 3; v32 3] in
  let (t,res) = LlvmEval.eval_fun ast btime (usid fname) args (-1) in
  test_llvm_int_res "Function simple_array_access()" res 3;

  (* Test simple_matrix_access.  *)
  let fname = "simple_matrix_access" in
  let args = [v32 4; v32 5; v32 4; v32 5; v32 10] in
  let (t,res) = LlvmEval.eval_fun ast btime (usid fname) args (-1) in
  test_llvm_int_res "Function simple_matrix_access()" res 10; 

  (* Test less_simple_matrix_access.  *)
  let fname = "less_simple_matrix_access" in
  let args = [v32 4; v32 5; v32 4; v32 5; v32 10] in
  let (t,res) = LlvmEval.eval_fun ast btime (usid fname) args (-1) in
  test_llvm_int_res "Function less_simple_matrix_access()" res 25;
  
  (* --------------------- STRUCTS ------------------------------------ *)

  let ast = LlvmDecode.bcfile2ast "utest/testcode/structs.bc" in
  (* uprint_endline (LlvmPPrint.pprint_module ast); *)

  (* Test simple_struct_access)  *)
  let fname = "simple_struct_access" in
  let args = [v32 7] in
  let (t,res) = LlvmEval.eval_fun ast btime (usid fname) args (-1) in
  test_llvm_int_res "Function simple_struct_access()" res 77; 


  (* --------------------- SWITCHES ------------------------------------ *)

(*
  let ast = LlvmDecode.bcfile2ast "utest/testcode/switches.bc" in
  uprint_endline (LlvmPPrint.pprint_module ast); 

  (* Test simple_switch(222))  *)
  let fname = "simple_switch" in
  let args = [v32 222] in
  let (t,res) = LlvmEval.eval_fun ast btime (usid fname) args (-1) in
  test_llvm_int_res "Function simple_switch(222)" res 98568; 

  (* Test simple_switch(93))  *)
  let fname = "simple_switch" in
  let args = [v32 93] in
  let (t,res) = LlvmEval.eval_fun ast btime (usid fname) args (-1) in
  test_llvm_int_res "Function simple_switch(93)" res 16; 
*)

  (* ------------- MDH WCET BENCHMARK : adpcm -------------------------- *)

(*  let ast = LlvmDecode.bcfile2ast "utest/mdhwcet/adpcm.bc" in
  uprint_endline (LlvmPPrint.pprint_module ast); 
*)

  (* ------------------------------------------------------------------- *)


  result()











