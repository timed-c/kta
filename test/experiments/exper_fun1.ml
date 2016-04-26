
open Printf
open AbstractMIPS 
open Ustring.Op


(* -- Basic Block Identifiers -- *)

let final_   = 0
let foo_     = 1
let extra1_  = 2
let double_  = 3
  
  
(* -- Program Code -- *)
    
let rec final ms = ms
  
and foo ms = ms         |>   
    addi a0 zero 30     |>
    jal  double_
 
and extra1 ms = ms      |>
    jr   ra
    
and double ms = ms      |>
    add  v0 a0 a0       |>
    jr   ra             	


  
(* -- Basic Block Info -- *)
    
let bblocks =
[|
  {func=final;   nextid=na_;     dist=0; addr=0x00000000; caller=false};
  {func=foo;     nextid=extra1_; dist=1; addr=0x00400200; caller=true };
  {func=extra1;  nextid=na_;     dist=0; addr=0x00400200; caller=false};
  {func=double;  nextid=na_;     dist=0; addr=0x00400200; caller=false};
|]

  
(* -- Start of Analysis -- *)

let main =
  let args = (Array.to_list Sys.argv |> List.tl) in
  analyze foo_ bblocks args
  |> print_mstate











      

