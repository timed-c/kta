


open Printf
open AbstractMIPS 
open Ustring.Op

(* -- Basic Block Identifiers -- *)

let exp1_   = 0
let loop_   = 1
let return_ = 2




(* -- Program code -- *)
let rec start() = ()
    
and exp1 gs ps = ps      |>   
    addi  v0 zero 88     |>
    next  gs

and loop gs ps = ps      |>
    add   v0 v0 a0       |>
    addi  a0 a0 (-1)     |>	
    bne	  a0 a1 loop_ gs  

and return gs ps = ps    |>
    jr	  ra gs          

    

(* -- Basic Block Info -- *)


let blocks =
[|
  (* {func=exp1;   nextid=loop_;   addr=0x00400000}; *)
  {func=exp1;   nextid=return_; dist=3; addr=0x00400000};
  {func=loop;   nextid=return_; dist=2; addr=0x00400200};
  {func=return; nextid=exit_;   dist=1; addr=0x00400400};
|]

(* -- Start of analysis -- *)

let main =
    analyze exp1_ blocks
    
  
let testmain =
  let s =
    init 0x100        |>
    lii  t1 (-7) 100  |>
    addi v0 zero 77   |>
    add  t0 v0 v0     |>
    add  t2 t1 v0     |>
    add  t3 t1 t2            
        
  in
  uprint_endline (pprint_pstate s 32)
  




