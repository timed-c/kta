


open Printf
open AbstractMIPS 
open Ustring.Op

(* -- Basic Block Identifiers -- *)

let exp1_   = 0
let loop_   = 1
let return_ = 2

  
(* -- Program Code -- *)
    
let
rec exp1 ps = ps        |>   
    addi  v0 zero 0     |>
    next  

and loop ps = ps        |>
    add  v0 v0 a0       |>
    addi a0 a0 (-1)     |>	
    bne	 a0 a1 loop_ 

and return ps = ps      |>
    jr	 ra           


(* -- Basic Block Info -- *)
    
let bblocks =
[|
  {func=exp1;   nextid=loop_;   dist=2; addr=0x00400000};
  {func=loop;   nextid=return_; dist=1; addr=0x00400200};
  {func=return; nextid=na_;     dist=0; addr=0x00400400};
|]

  
(* -- Start of Analysis -- *)

let main =
    analyze exp1_ bblocks
    







      
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
  




