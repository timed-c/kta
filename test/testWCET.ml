
open MipsCfg
open MipsSys
open MipsUtils
open Ustring.Op
open Printf
open Str
open Utest
       
let tmpfile = "temp-file-090916-070704.tmp"

let compile_file filename fname args optimize debug bsconfig =
  (try
     MipsSys.pic32_compile [filename] false optimize tmpfile
   with Sys_error e ->
     e |> eprintf "Error %s");
  let prog = MipsSys.get_program tmpfile |>  MipsUtils.add_branch_symbols in
  let (prog,cfgmap) = MipsCfg.make_cfgmap fname prog in
  let program_code = MipsCfg.pprint_ocaml_cps_from_cfgmap true fname cfgmap prog in
  let stdout = MipsSys.wcet_compile "array_mul" false bsconfig program_code args in
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
  
  printf "\ntesting array_mul.c\n%!";  
  let bcet,wcet = compile_file "test/demo/array_mul.c" "array_mul" ["a0=[1,2]";"a1=[1,4]";"a2=[1,3]"] 0 false None in
  Utest.test_int "array_mul.c: Non optimized." bcet 98;
  Utest.test_int "array_mul.c: Non optimized." wcet 1279;
  let bcet,wcet = compile_file "test/demo/array_mul.c" "array_mul" ["a0=[1,2]";"a1=[1,4]";"a2=[1,3]"] 3 false None in
  Utest.test_int "array_mul.c: Optimized." bcet 41;
  Utest.test_int "array_mul.c: Optimized." wcet 282;
  let bcet,wcet = compile_file "test/demo/fact.c" "fact" ["a0=[1,6]"] 0 false None in
  printf "\ntesting fact.c\n%!";  
  Utest.test_int "fact.c: Not optimized." bcet 15;
  Utest.test_int "fact.c: Not optimized." wcet 70;
  let bcet,wcet = compile_file "test/demo/fact.c" "fact" ["a0=[1,6]"] 3 false None in
  Utest.test_int "fact.c: Optimized." bcet 3;
  Utest.test_int "fact.c: Optimized." wcet 30;
  (* NS.C *)
  printf "\ntesting ns.c\n%!";
  let bcet,wcet = compile_file "test/wcet_tests/mdh/ns.c" "foo" ["a0=[1,10]"] 0 false None in
  Utest.test_int "ns.c: BCET (foo) Not Optimized." bcet 114;
  Utest.test_int "ns.c: WCET (foo) Not Optimized." wcet 24250;
  let bcet,wcet = compile_file "test/wcet_tests/mdh/ns.c" "foo" ["a0=[1,10]"] 1 false None in
  Utest.test_int "ns.c: BCET (foo) Optimized: -O1." bcet 31;
  Utest.test_int "ns.c: WCET (foo) Optimized: -O1." wcet 3536;
  let bcet,wcet = compile_file "test/wcet_tests/mdh/ns.c" "foo" ["a0=[1,10]"] 2 false None in
  Utest.test_int "ns.c: BCET (foo) Optimized: -O2." bcet 32;
  Utest.test_int "ns.c: WCET (foo) Optimized: -O2." wcet 3912;
  let bcet,wcet = compile_file "test/wcet_tests/mdh/ns.c" "foo" ["a0=[1,10]"] 3 false None in
  Utest.test_int "ns.c: BCET (foo) Optimized: -O3." bcet 29;
  Utest.test_int "ns.c: WCET (foo) Optimized: -O3." wcet 1394;
  (* FAC.C - -O0 infinite loop, ld -> branch -> ld *)
  printf "\ntesting fac.c\n%!";
  (*
  let bcet,wcet = compile_file "test/wcet_tests/mdh/fac.c" "fac" ["a0=[1,10]"] 0 false None in
  Utest.test_int "fac.c: BCET Not Optimized." bcet ?;
  Utest.test_int "fac.c: WCET Not Optimized." wcet ?;
 *)
  let bcet,wcet = compile_file "test/wcet_tests/mdh/fac.c" "fac" ["a0=[1,10]"] 1 false None in
  Utest.test_int "fac.c: BCET (fac) Optimized: -O1." bcet 16;
  Utest.test_int "fac.c: WCET (fac) Optimized: -O1." wcet 133;
  let bcet,wcet = compile_file "test/wcet_tests/mdh/fac.c" "fac" ["a0=[1,10]"] 2 false None in
  Utest.test_int "fac.c: BCET (fac) Optimized: -O2." bcet 9;
  Utest.test_int "fac.c: WCET (fac) Optimized: -O2." wcet 54;
  let bcet,wcet = compile_file "test/wcet_tests/mdh/fac.c" "fac" ["a0=[1,10]"] 3 false None in
  Utest.test_int "fac.c: BCET (fac) Optimized: -O3." bcet 9;
  Utest.test_int "fac.c: WCET (fac) Optimized: -O3." wcet 54;
  (* RECURSION.C *)
  printf "\ntesting recursion.c\n%!";
  let bcet,wcet = compile_file "test/wcet_tests/mdh/recursion.c" "kalle" ["a0=[0,10]"] 0 false None in
  Utest.test_int "recursion.c (kalle): BCET Not Optimized." bcet 15;
  Utest.test_int "recursion.c (kalle): WCET Not Optimized." wcet 185;
  let bcet,wcet = compile_file "test/wcet_tests/mdh/recursion.c" "kalle" ["a0=[0,10]"] 1 false None in
  Utest.test_int "recursion.c (kalle): BCET Optimized: -O1." bcet 3;
  Utest.test_int "recursion.c (kalle): WCET Optimized: -O1." wcet 73;
  let bcet,wcet = compile_file "test/wcet_tests/mdh/recursion.c" "kalle" ["a0=[0,10]"] 2 false None in
  Utest.test_int "recursion.c (kalle): BCET Optimized: -O2." bcet 3;
  Utest.test_int "recursion.c (kalle): WCET Optimized: -O2." wcet 25;
  let bcet,wcet = compile_file "test/wcet_tests/mdh/recursion.c" "kalle" ["a0=[0,10]"] 3 false None in
  Utest.test_int "recursion.c (kalle): BCET Optimized: -O3." bcet 3;
  Utest.test_int "recursion.c (kalle): WCET Optimized: -O3." wcet 34;
  let bcet,wcet = compile_file "test/wcet_tests/mdh/recursion.c" "anka" ["a0=[0,10]"] 0 false None in
  Utest.test_int "recursion.c (anka): BCET Not Optimized." bcet 15;
  Utest.test_int "recursion.c (anka): WCET Not Optimized." wcet 185;
  let bcet,wcet = compile_file "test/wcet_tests/mdh/recursion.c" "anka" ["a0=[0,10]"] 1 false None in
  Utest.test_int "recursion.c (anka): BCET Optimized: -O1." bcet 3;
  Utest.test_int "recursion.c (anka): WCET Optimized: -O1." wcet 73;
  let bcet,wcet = compile_file "test/wcet_tests/mdh/recursion.c" "anka" ["a0=[0,10]"] 2 false None in
  Utest.test_int "recursion.c (anka): BCET Optimized: -O2." bcet 3;
  Utest.test_int "recursion.c (anka): WCET Optimized: -O2." wcet 26;
  let bcet,wcet = compile_file "test/wcet_tests/mdh/recursion.c" "anka" ["a0=[0,10]"] 3 false None in
  Utest.test_int "recursion.c (anka): BCET Optimized: -O3." bcet 3;
  Utest.test_int "recursion.c (anka): WCET Optimized: -O3." wcet 32;
(*
  let bcet,wcet = compile_file "test/wcet_tests/mdh/recursion.c" "fib" ["a0=[0,10]"] 0 false None in
  Utest.test_int "recursion.c (fib): BCET Not Optimized." bcet 15;
  Utest.test_int "recursion.c (fib): WCET Not Optimized." wcet 185;
 *)
  let bcet,wcet = compile_file "test/wcet_tests/mdh/recursion.c" "fib" ["a0=[0,10]"] 1 false None in
  Utest.test_int "recursion.c (fib): BCET Optimized: -O1." bcet 3;
  Utest.test_int "recursion.c (fib): WCET Optimized: -O1." wcet 1675;
  let bcet,wcet = compile_file "test/wcet_tests/mdh/recursion.c" "fib" ["a0=[0,10]"] 2 false None in
  Utest.test_int "recursion.c (fib): BCET Optimized: -O2." bcet 11;
  Utest.test_int "recursion.c (fib): WCET Optimized: -O2." wcet 1386;
  let bcet,wcet = compile_file "test/wcet_tests/mdh/recursion.c" "fib" ["a0=[0,10]"] 3 false None in
  Utest.test_int "recursion.c (fib): BCET Optimized: -O3." bcet 3;
  Utest.test_int "recursion.c (fib): WCET Optimized: -O3." wcet 993;
 
  (* BSORT100.C - Different Values due to initialization - -O1 looks weird *)
  printf "\ntesting bsort100.c\n%!";

  let bcet,wcet = compile_file "test/wcet_tests/mdh/bsort100.c" "main" [] 0 false None in
  Utest.test_int "bsort100.c (main): BCET Not Optimized." bcet 4535;
  Utest.test_int "bsort100.c (main): WCET Not Optimized." wcet 251917;
  let bcet,wcet = compile_file "test/wcet_tests/mdh/bsort100.c" "main" [] 1 false None in
  Utest.test_int "bsort100.c (main): BCET Optimized: -O1." bcet 1417;
  Utest.test_int "bsort100.c (main): WCET Optimized: -O1." wcet 51010;
  let bcet,wcet = compile_file "test/wcet_tests/mdh/bsort100.c" "main" [] 2 false None in
  Utest.test_int "bsort100.c (main): BCET Optimized: -O2." bcet 1511;
  Utest.test_int "bsort100.c (main): WCET Optimized: -O2." wcet 55954;
  let bcet,wcet = compile_file "test/wcet_tests/mdh/bsort100.c" "main" [] 3 false None in
  Utest.test_int "bsort100.c (main): BCET Optimized: -O3." bcet 1511;
  Utest.test_int "bsort100.c (main): WCET Optimized: -O3." wcet 55954;

  (* INSERTSORT.C - same results WCET = BCET *)
  printf "\ntesting insertsort.c\n%!";

  let bcet,wcet = compile_file "test/wcet_tests/mdh/insertsort.c" "main" [] 0 false None in
  Utest.test_int "insertsort.c (main): BCET=WCET Not Optimized." bcet wcet;
  Utest.test_int "insertsort.c (main): WCET Not Optimized." wcet 2342;
  let bcet,wcet = compile_file "test/wcet_tests/mdh/insertsort.c" "main" [] 1 false None in
  Utest.test_int "insertsort.c (main): BCET=WCET Optimized: -O1." bcet wcet;
  Utest.test_int "insertsort.c (main): WCET Optimized: -O1." wcet 316;
  let bcet,wcet = compile_file "test/wcet_tests/mdh/insertsort.c" "main" [] 2 false None in
  Utest.test_int "insertsort.c (main): BCET=WCET Optimized: -O2." bcet wcet;
  Utest.test_int "insertsort.c (main): WCET Optimized: -O2." wcet 316;
  let bcet,wcet = compile_file "test/wcet_tests/mdh/insertsort.c" "main" [] 3 false None in
  Utest.test_int "insertsort.c (main): BCET=WCET Optimized: -O3." bcet wcet;
  Utest.test_int "insertsort.c (main): WCET Optimized: -O3." wcet 251;

  (* MATMULT.C - same results WCET = BCET *)
  printf "\ntesting matmult.c\n%!";

  let bcet,wcet = compile_file "test/wcet_tests/mdh/matmult.c" "main" [] 0 false None in
  Utest.test_int "matmult.c (main): BCET=WCET Not Optimized." bcet wcet;
  Utest.test_int "matmult.c (main): WCET Not Optimized." wcet 373479;
  let bcet,wcet = compile_file "test/wcet_tests/mdh/matmult.c" "main" [] 1 false None in
  Utest.test_int "matmult.c (main): BCET=WCET Optimized: -O1." bcet wcet;
  Utest.test_int "matmult.c (main): WCET Optimized: -O1." wcet 95935;
  let bcet,wcet = compile_file "test/wcet_tests/mdh/matmult.c" "main" [] 2 false None in
  Utest.test_int "matmult.c (main): BCET=WCET Optimized: -O2." bcet wcet;
  Utest.test_int "matmult.c (main): WCET Optimized: -O2." wcet 83874;
  let bcet,wcet = compile_file "test/wcet_tests/mdh/matmult.c" "main" [] 3 false None in
  Utest.test_int "matmult.c (main): BCET=WCET Optimized: -O3." bcet wcet;
  Utest.test_int "matmult.c (main): WCET Optimized: -O3." wcet 83898;

  (* FIBCALL.C *)
  printf "\ntesting fibcall.c\n%!";
  let bcet,wcet = compile_file "test/wcet_tests/mdh/fibcall.c" "fib" ["a0=[0,100]"] 0 false None in
  Utest.test_int "fibcall.c (fib): BCET Not Optimized." bcet 24;
  Utest.test_int "fibcall.c (fib): WCET Not Optimized." wcet 542;
  let bcet,wcet = compile_file "test/wcet_tests/mdh/fibcall.c" "fib" ["a0=[0,100]"] 1 false None in
  Utest.test_int "fibcall.c (fib): BCET Optimized: -O1." bcet 3;
  Utest.test_int "fibcall.c (fib): WCET Optimized: -O1." wcet 173;
  let bcet,wcet = compile_file "test/wcet_tests/mdh/fibcall.c" "fib" ["a0=[0,100]"] 2 false None in
  Utest.test_int "fibcall.c (fib): BCET Optimized: -O2." bcet 3;
  Utest.test_int "fibcall.c (fib): WCET Optimized: -O2." wcet 174;
  let bcet,wcet = compile_file "test/wcet_tests/mdh/fibcall.c" "fib" ["a0=[0,100]"] 3 false None in
  Utest.test_int "fibcall.c (fib): BCET Optimized: -O3." bcet 3;
  Utest.test_int "fibcall.c (fib): WCET Optimized: -O3." wcet 174;

  (* NSICHNEU.C - main: not same - compares array elements that are 
     not defined *)
  printf "\ntesting nsichneu.c\n%!";
  (* -O0 inf loop or too slow *)
  (*
  let bcet,wcet = compile_file "test/wcet_tests/mdh/nsichneu.c" "main" [] 0 false None in
  Utest.test_int "nsichneu.c (main): BCET Optimized: -O1." bcet ?;
  Utest.test_int "nsichneu.c (main): WCET Optimized: -O1." wcet ?;
   *)
  let bcet,wcet = compile_file "test/wcet_tests/mdh/nsichneu.c" "main" [] 1 false None in
  Utest.test_int "nsichneu.c (main): BCET Optimized: -O1." bcet 766;
  Utest.test_int "nsichneu.c (main): WCET Optimized: -O1." wcet 10552;

  let bcet,wcet = compile_file "test/wcet_tests/mdh/nsichneu.c" "main" [] 2 false None in
  Utest.test_int "nsichneu.c (main): BCET Optimized: -O2." bcet 767;
  Utest.test_int "nsichneu.c (main): WCET Optimized: -O2." wcet 10333;

  let bcet,wcet = compile_file "test/wcet_tests/mdh/nsichneu.c" "main" [] 3 false None in
  Utest.test_int "nsichneu.c (main): BCET Optimized: -O3." bcet 767;
  Utest.test_int "nsichneu.c (main): WCET Optimized: -O3." wcet 10333;
  
  (* FDCT.C - Forward Discrete Cosine Transform - WCET = BCET *)
  printf "\ntesting fdct.c\n%!";

  let bcet,wcet = compile_file "test/wcet_tests/mdh/fdct.c" "main" [] 0 false None in
  Utest.test_int "fdct.c (main): BCET=WCET Not Optimized." bcet wcet;
  Utest.test_int "fdct.c (main): WCET Not Optimized." wcet 5238;
  let bcet,wcet = compile_file "test/wcet_tests/mdh/fdct.c" "main" [] 1 false None in
  Utest.test_int "fdct.c (main): BCET=WCET Optimized: -O1." bcet wcet;
  Utest.test_int "fdct.c (main): WCET Optimized: -O1." wcet 2599;
  let bcet,wcet = compile_file "test/wcet_tests/mdh/fdct.c" "main" [] 2 false None in
  Utest.test_int "fdct.c (main): BCET=WCET Optimized: -O2." bcet wcet;
  Utest.test_int "fdct.c (main): WCET Optimized: -O2." wcet 3230;
  let bcet,wcet = compile_file "test/wcet_tests/mdh/fdct.c" "main" [] 3 false None in
  Utest.test_int "fdct.c (main): BCET=WCET Optimized: -O3." bcet wcet;
  Utest.test_int "fdct.c (main): WCET Optimized: -O3." wcet 3223;

  (* CNT.C - Count Non-negative Numbers - WCET = BCET *)
  printf "\ntesting cnt.c\n%!";

  let bcet,wcet = compile_file "test/wcet_tests/mdh/cnt.c" "main" [] 0 false None in
  Utest.test_int "cnt.c (main): BCET=WCET Not Optimized." bcet wcet;
  Utest.test_int "cnt.c (main): WCET Not Optimized." wcet 6527;

  let bcet,wcet = compile_file "test/wcet_tests/mdh/cnt.c" "main" [] 1 false None in
  Utest.test_int "cnt.c (main): BCET=WCET Optimized: -O1." bcet wcet;
  Utest.test_int "cnt.c (main): WCET Optimized: -O1." wcet 2300;
  
  let bcet,wcet = compile_file "test/wcet_tests/mdh/cnt.c" "main" [] 2 false None in
  Utest.test_int "cnt.c (main): BCET=WCET Optimized: -O2." bcet wcet;
  Utest.test_int "cnt.c (main): WCET Optimized: -O2." wcet 1776;
  let bcet,wcet = compile_file "test/wcet_tests/mdh/cnt.c" "main" [] 3 false None in
  Utest.test_int "cnt.c (main): BCET=WCET Optimized: -O3." bcet wcet;
  Utest.test_int "cnt.c (main): WCET Optimized: -O3." wcet 2125;

  (* FIR.C - Coefficients not considered (Any) - WCET = BCET *)
  printf "\ntesting fir.c\n%!";

  (* TODO(Romy): very slow *)
  (*
  let bcet,wcet = compile_file "test/wcet_tests/mdh/fir.c" "main" [] 0 false None in
  Utest.test_int "fir.c (main): BCET=WCET Not Optimized." bcet 38123;
  Utest.test_int "fir.c (main): WCET Not Optimized." wcet 5140880;
  
  let bcet,wcet = compile_file "test/wcet_tests/mdh/fir.c" "main" [] 1 false None in
  Utest.test_int "fir.c (main): BCET=WCET Optimized: -O1." bcet wcet;
  Utest.test_int "fir.c (main): WCET Optimized: -O1." wcet 225477; 
  let bcet,wcet = compile_file "test/wcet_tests/mdh/fir.c" "main" [] 2 false None in
  Utest.test_int "fir.c (main): BCET=WCET Optimized: -O2." bcet wcet;
  Utest.test_int "fir.c (main): WCET Optimized: -O2." wcet 225458;
  
  let bcet,wcet = compile_file "test/wcet_tests/mdh/fir.c" "main" [] 3 false None in
  Utest.test_int "fir.c (main): BCET=WCET Optimized: -O3." bcet wcet;
  Utest.test_int "fir.c (main): WCET Optimized: -O3." wcet 225458;
  *)
  (* PRIME.C - prime: returns bool, main: returns bool *)
  printf "\ntesting prime.c\n%!";
  let bcet,wcet = compile_file "test/wcet_tests/mdh/prime.c" "prime" ["a0=[0,100]"] 0 false None in
  Utest.test_int "prime.c (prime): BCET Not Optimized." bcet 47;
  Utest.test_int "prime.c (prime): WCET Not Optimized." wcet 176;
  let bcet,wcet = compile_file "test/wcet_tests/mdh/prime.c" "prime" ["a0=[0,100]"] 1 false None in
  Utest.test_int "prime.c (prime): BCET Optimized: -O1." bcet 4;
  Utest.test_int "prime.c (prime): WCET Optimized: -O1." wcet 21;
  (* Syntax error in OCaml, prime.part.0:
    let prime.part.0 ms = ms                |>
      sltiu   v0 a0 9                       |>
      bnelds  v0 zero ex6_                  |>
      next
      ...
    Works if manualy renamed
  *)
  (*
  let bcet,wcet = compile_file "test/wcet_tests/mdh/prime.c" "prime" ["a0=[0,100]"] 2 false None in
  Utest.test_int "prime.c (prime): BCET Optimized: -O2." bcet ?;
  Utest.test_int "prime.c (prime): WCET Optimized: -O2." wcet ?;
  *)
  let bcet,wcet = compile_file "test/wcet_tests/mdh/prime.c" "prime" ["a0=[0,100]"] 3 false None in
  Utest.test_int "prime.c (prime): BCET Optimized: -O3." bcet 4;
  Utest.test_int "prime.c (prime): WCET Optimized: -O3." wcet 20;

  (* JFDCTINT.C - JPEG Integer Forward Discrete Cosine Transform - WCET = BCET *)
  printf "\ntesting jfdctint.c\n%!";

  let bcet,wcet = compile_file "test/wcet_tests/mdh/jfdctint.c" "main" [] 0 false None in
  Utest.test_int "jfdctint.c (main): BCET=WCET Not Optimized." bcet wcet;
  Utest.test_int "jfdctint.c (main): WCET Not Optimized." wcet 6858;
  let bcet,wcet = compile_file "test/wcet_tests/mdh/jfdctint.c" "main" [] 1 false None in
  Utest.test_int "jfdctint.c (main): BCET=WCET Optimized: -O1." bcet wcet;
  Utest.test_int "jfdctint.c (main): WCET Optimized: -O1." wcet 3396; 
  let bcet,wcet = compile_file "test/wcet_tests/mdh/jfdctint.c" "main" [] 2 false None in
  Utest.test_int "jfdctint.c (main): BCET=WCET Optimized: -O2." bcet wcet;
  Utest.test_int "jfdctint.c (main): WCET Optimized: -O2." wcet 3807;
  
  let bcet,wcet = compile_file "test/wcet_tests/mdh/jfdctint.c" "main" [] 3 false None in
  Utest.test_int "jfdctint.c (main): BCET=WCET Optimized: -O3." bcet wcet;
  Utest.test_int "jfdctint.c (main): WCET Optimized: -O3." wcet 3807;

  (* ADPCM.C - Adaptive Differential Pulse Code Modulation 
     Comment: BCET!=WCET due to ANY values (coefficients) split with div/mod 
  *)
  printf "\ntesting adpcm.c\n%!";

  let bcet,wcet = compile_file "test/wcet_tests/mdh/adpcm.c" "main" [] 0 false None in
  Utest.test_int "adpcm.c (main): BCET Not Optimized." bcet 26832;
  Utest.test_int "adpcm.c (main): WCET Not Optimized." wcet 302369;
  let bcet,wcet = compile_file "test/wcet_tests/mdh/adpcm.c" "main" [] 1 false None in
  Utest.test_int "adpcm.c (main): BCET Optimized: -O1." bcet 9527;
  Utest.test_int "adpcm.c (main): WCET Optimized: -O1." wcet 112957;
  let bcet,wcet = compile_file "test/wcet_tests/mdh/adpcm.c" "main" [] 2 false None in
  Utest.test_int "adpcm.c (main): BCET Optimized: -O2." bcet 9286;
  Utest.test_int "adpcm.c (main): WCET Optimized: -O2." wcet 95639;
  let bcet,wcet = compile_file "test/wcet_tests/mdh/adpcm.c" "main" [] 3 false None in
  Utest.test_int "adpcm.c (main): BCET Optimized: -O3." bcet 8790;
  Utest.test_int "adpcm.c (main): WCET Optimized: -O3." wcet 95127;

  let bcet,wcet = compile_file "test/wcet_tests/mdh/adpcm.c" "my_cos" ["a0=[0,6282]"] 0 false None in
  Utest.test_int "adpcm.c (my_cos): BCET Not Optimized." bcet 90;
  Utest.test_int "adpcm.c (my_cos): WCET Not Optimized." wcet 116442;
  let bcet,wcet = compile_file "test/wcet_tests/mdh/adpcm.c" "my_cos" ["a0=[0,6282]"] 1 false None in
  Utest.test_int "adpcm.c (my_cos): BCET Optimized: -O1." bcet 27;
  Utest.test_int "adpcm.c (my_cos): WCET Optimized: -O1." wcet 43659;
  let bcet,wcet = compile_file "test/wcet_tests/mdh/adpcm.c" "my_cos" ["a0=[0,6282]"] 2 false None in
  Utest.test_int "adpcm.c (my_cos): BCET Optimized: -O2." bcet 21;
  Utest.test_int "adpcm.c (my_cos): WCET Optimized: -O2." wcet 36381;
  let bcet,wcet = compile_file "test/wcet_tests/mdh/adpcm.c" "my_cos" ["a0=[0,6282]"] 3 false None in
  Utest.test_int "adpcm.c (my_cos): BCET Optimized: -O3." bcet 21;
  Utest.test_int "adpcm.c (my_cos): WCET Optimized: -O3." wcet 36381;
  let bcet,wcet = compile_file "test/wcet_tests/mdh/adpcm.c" "my_sin" ["a0=[0,6282]"] 0 false None in
  Utest.test_int "adpcm.c (my_sin): BCET Not Optimized." bcet 75;
  Utest.test_int "adpcm.c (my_sin): WCET Not Optimized." wcet 153339;
  let bcet,wcet = compile_file "test/wcet_tests/mdh/adpcm.c" "my_sin" ["a0=[0,6282]"] 1 false None in
  Utest.test_int "adpcm.c (my_sin): BCET Optimized: -O1." bcet 21;
  Utest.test_int "adpcm.c (my_sin): WCET Optimized: -O1." wcet 57495;
  let bcet,wcet = compile_file "test/wcet_tests/mdh/adpcm.c" "my_sin" ["a0=[0,6282]"] 2 false None in
  Utest.test_int "adpcm.c (my_sin): BCET Optimized: -O2." bcet 19;
  Utest.test_int "adpcm.c (my_sin): WCET Optimized: -O2." wcet 47914;
  let bcet,wcet = compile_file "test/wcet_tests/mdh/adpcm.c" "my_sin" ["a0=[0,6282]"] 3 false None in
  Utest.test_int "adpcm.c (my_sin): BCET Optimized: -O3." bcet 19;
  Utest.test_int "adpcm.c (my_sin): WCET Optimized: -O3." wcet 47914;

  (* BS.C - Binary Search 

     Comment: Doesn't depend on input.
  *)
  printf "\ntesting bs.c\n%!";

  let bcet,wcet = compile_file "test/wcet_tests/mdh/bs.c" "binary_search" [] 0 false (Some 23) in
  Utest.test_int "bs.c (binary_search): BCET Not Optimized." bcet 48;
  Utest.test_int "bs.c (binary_search): WCET Not Optimized." wcet 143;
  let bcet,wcet = compile_file "test/wcet_tests/mdh/bs.c" "binary_search" [] 1 false (Some 23) in
  Utest.test_int "bs.c (binary_search): BCET Optimized: -O1." bcet 18;
  Utest.test_int "bs.c (binary_search): WCET Optimized: -O1." wcet 45;
  let bcet,wcet = compile_file "test/wcet_tests/mdh/bs.c" "binary_search" [] 2 false (Some 23) in
  Utest.test_int "bs.c (binary_search): BCET Optimized: -O2." bcet 17;
  Utest.test_int "bs.c (binary_search): WCET Optimized: -O2." wcet 47;
  let bcet,wcet = compile_file "test/wcet_tests/mdh/bs.c" "binary_search" [] 3 false (Some 23) in
  Utest.test_int "bs.c (binary_search): BCET Optimized: -O3." bcet 17;
  Utest.test_int "bs.c (binary_search): WCET Optimized: -O3." wcet 47;

  (* EDN.C - Simple Vector Algorithms (fir,jpeg discrete cosine transform)

     Comment: main (BSET=WCET)
  *)
  printf "\ntesting edn.c\n%!";
  (* -O0: Error: Undefined reference to `memcpy *)
  (*
  let bcet,wcet = compile_file "test/wcet_tests/mdh/edn.c" "main" [] 0 false None in
  Utest.test_int "edn.c (main): BCET=WCET Not Optimized." bcet wcet;
  Utest.test_int "edn.c (main): WCET Not Optimized." wcet 143;
  
  let bcet,wcet = compile_file "test/wcet_tests/mdh/edn.c" "main" [] 1 false None in
  Utest.test_int "edn.c (main): BCET=WCET Optimized: -O1." bcet wcet;
  Utest.test_int "edn.c (main): WCET Optimized: -O1." wcet 49274;
   *)
  let bcet,wcet = compile_file "test/wcet_tests/mdh/edn.c" "main" [] 2 false None in
  Utest.test_int "edn.c (main): BCET=WCET Optimized: -O2." bcet wcet;
  Utest.test_int "edn.c (main): WCET Optimized: -O2." wcet 50884;
  let bcet,wcet = compile_file "test/wcet_tests/mdh/edn.c" "main" [] 3 false None in
  Utest.test_int "edn.c (main): BCET=WCET Optimized: -O3." bcet wcet;
  Utest.test_int "edn.c (main): WCET Optimized: -O3." wcet 50213;

  (* NDES.C

     Comment: BCET!=WCET (SLT)
  *)
  printf "\ntesting ndes.c\n%!";

  let bcet,wcet = compile_file "test/wcet_tests/mdh/ndes.c" "main" [] 0 false None in
  Utest.test_int "ndes.c (main): BCET Not Optimized." bcet 53388;
  Utest.test_int "ndes.c (main): WCET Not Optimized." wcet 101756;
  let bcet,wcet = compile_file "test/wcet_tests/mdh/ndes.c" "main" [] 1 false None in
  Utest.test_int "ndes.c (main): BCET Optimized: -O1." bcet 21826;
  Utest.test_int "ndes.c (main): WCET Optimized: -O1." wcet 41840;
  let bcet,wcet = compile_file "test/wcet_tests/mdh/ndes.c" "main" [] 2 false None in
  Utest.test_int "ndes.c (main): BCET Optimized: -O2." bcet 19302;
  Utest.test_int "ndes.c (main): WCET Optimized: -O2." wcet 46214;
  let bcet,wcet = compile_file "test/wcet_tests/mdh/ndes.c" "main" [] 3 false None in
  Utest.test_int "ndes.c (main): BCET Optimized: -O3." bcet 19061;
  Utest.test_int "ndes.c (main): WCET Optimized: -O3." wcet 45958;

  (* LCDNUM.C

     Comment: Trivial loop, BCET!=WCET (SLT)
  *)
  printf "\ntesting lcdnum.c\n%!";
  (* -O{0,1} Indirect Jumps Not Supported *)
  let bcet,wcet = compile_file "test/wcet_tests/mdh/lcdnum.c" "main" [] 2 false None in
  Utest.test_int "lcdnum.c (main): BCET Optimized: -O2." bcet 120;
  Utest.test_int "lcdnum.c (main): WCET Optimized: -O2." wcet 125;
  let bcet,wcet = compile_file "test/wcet_tests/mdh/lcdnum.c" "main" [] 3 false None in
  Utest.test_int "lcdnum.c (main): BCET Optimized: -O3." bcet 45;
  Utest.test_int "lcdnum.c (main): WCET Optimized: -O3." wcet 57;


  Utest.result()
