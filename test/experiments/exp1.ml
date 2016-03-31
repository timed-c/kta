


open Printf
open AbstractMIPS 
open Ustring.Op

(* -- Basic Block Identifiers -- *)

let final_  = 0
let exp1_   = 1
let loop_   = 2

  
(* -- Program Code -- *)
    
let rec final ms = ms
    
and exp1 ms = ms        |>   
    addi  v0 zero 0     |>
    next  

and loop ms = ms        |>
    add  v0 v0 a0       |>
    addi a0 a0 (-1)     |>	
    bne	 a0 a1 loop_ 



(* -- Basic Block Info -- *)
    
let bblocks =
[|
  {func=final;  nextid=na_;    dist=0; addr=0x0};
  {func=exp1;   nextid=loop_;  dist=2; addr=0x00400000};
  {func=loop;   nextid=final_; dist=1; addr=0x00400200};
|]

  
(* -- Start of Analysis -- *)

(*let main =
    analyze exp1_ bblocks
*)  


      
let testmain =
  let ms =
    init_mstate 0x100 bblocks |>
    lii  t1 (-7) 100  |>
    addi v0 zero 77   |>
    add  t0 v0 v0     |>
    add  t2 t1 v0     |>
    add  t3 t1 t2            
        
  in
  uprint_endline (pprint_pstate ms.pstate 32)
  




