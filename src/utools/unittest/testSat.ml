


open Ustring.Op
open Utest
open Printf


let main = 

  init "Test SAT solver";

  let cnf = Usat.read_cnf "unittest/cnf_tests/rand1.cnf" in
  let (na,op,on,pn) = Usat.var_kind_stat (Usat.var_kind cnf) in
  printf "vars = %d, not available = %d, pos = %d, neg = %d, pos and neg = %d\n"
         (Usat.variables cnf) na op on pn; 
  (*uprint_endline (Usat.pprint_cnf cnf);*)
  ();
    
  result()
