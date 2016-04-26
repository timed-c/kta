
open Printf
open AbstractMIPS 
open Ustring.Op


(* -- Basic Block Identifiers -- *)

let final_   = 0
let start_   = 1
let foo_     = 2
let foo_ret_ = 3
let double_  = 4
  
  
(* -- Program Code -- *)
    
let rec final ms = ms

and start ms = ms       |>
    call foo_
  
and foo ms = ms         |>   
    addi v0 zero 10     |>
    jal  double_
 
and foo_ret ms = ms     |>
    jr   ra
    
and double ms = ms      |>
    add  v0 a0 a0       |>
    jr   ra             	


  
(* -- Basic Block Info -- *)
    
let bblocks =
[|
  {func=final;   nextid=na_;      dist=0; addr=0x00000000};
  {func=start;   nextid=final_;   dist=1; addr=0x00400000};
  {func=foo;     nextid=foo_ret_; dist=1; addr=0x00400200};
  {func=foo_ret; nextid=na_;      dist=0; addr=0x00400200};
  {func=double;  nextid=na_;      dist=0; addr=0x00400200};
|]

  
(* -- Start of Analysis -- *)

let main =
  let args = (Array.to_list Sys.argv |> List.tl) in
  analyze exper_ bblocks args
  |> print_mstate

(* Comments
  a0=[1000,5000] a1=[400,520] takes 5.4 seconds on my laptop
   before optimized join *)










      

