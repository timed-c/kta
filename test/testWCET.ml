
open MipsCfg
open MipsSys
open MipsUtils
open Ustring.Op
open Printf
open Str
open Utest
       
let tmpfile = "temp-file-090916-070704.tmp"

let compile_file filename fname args optimize debug =
  (try
     MipsSys.pic32_compile [filename] false optimize tmpfile
   with Sys_error e ->
     e |> eprintf "Error %s");
  let prog = MipsSys.get_program tmpfile |>  MipsUtils.add_branch_symbols in
  let (prog,cfgmap) = MipsCfg.make_cfgmap fname prog in
  let program_code = MipsCfg.pprint_ocaml_cps_from_cfgmap true fname cfgmap prog in
  let stdout = MipsSys.wcet_compile "array_mul" false program_code args in
  try
    let regex = Str.regexp "BCET:[^0-9]*\\([0-9]+\\)\\(.\\|\n\\)*WCET:[^0-9]*\\([0-9]+\\)" in
    let _ = Str.search_forward regex stdout 0 in
    let bcet, wcet = int_of_string (Str.matched_group 1 stdout), int_of_string (Str.matched_group 3 stdout) in
    (bcet,wcet)                                                 
  with Not_found ->
    (eprintf "Error: BCET/WCET Not_Found\n";
     raise Not_found)
       
let main =
  Utest.init "WCET";
  
  let bcet,wcet = compile_file "test/demo/array_mul.c" "array_mul" ["a0=[1,2]";"a1=[1,4]";"a2=[1,3]"] false false in
  Utest.test_int "array_mul.c: Non optimized." bcet 118;
  Utest.test_int "array_mul.c: Non optimized." wcet 1687;
  let bcet,wcet = compile_file "test/demo/array_mul.c" "array_mul" ["a0=[1,2]";"a1=[1,4]";"a2=[1,3]"] true false in
  Utest.test_int "array_mul.c: Optimized." bcet 36;
  Utest.test_int "array_mul.c: Optimized." wcet 308;
  let bcet,wcet = compile_file "test/demo/fact.c" "fact" ["a0=[1,6]"] false false in
  Utest.test_int "fact.c: Not optimized." bcet 15;
  Utest.test_int "fact.c: Not optimized." wcet 70;
  let bcet,wcet = compile_file "test/demo/fact.c" "fact" ["a0=[1,6]"] true false in
  Utest.test_int "fact.c: Optimized." bcet 3;
  Utest.test_int "fact.c: Optimized." wcet 30;



