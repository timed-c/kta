


open Ustring.Op
open Utest
open Printf


let main = 

  init "Test SAT solver";

  let cnf = Usat.read_cnf "unittest/cnf_tests/small1.cnf" in
  (* let (_,na,op,on,pn) = Usat.var_statistics cnf in
  printf "vars = %d, not available = %d, pos = %d, neg = %d, pos and neg = %d\n"
         (Usat.variables cnf) na op on pn; *)
  (*uprint_endline (Usat.pprint_cnf cnf);*)
  ();
    
  result()
