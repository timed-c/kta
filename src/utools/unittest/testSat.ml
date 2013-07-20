


open Ustring.Op
open Utest
open Printf


let main = 

  init "Test SAT solver";

  let cnf = Usat.read_cnf "unittest/cnf_tests/rand1.cnf" in
  (); (*uprint_endline (Usat.pprint_cnf cnf);*)

    
  result()
