
open Printf
open AbstractMIPS 
open Ustring.Op


(* -- Basic Block Identifiers -- *)

let final_      = 0
let foo_        = 1
let ex1_        = 2
let ex1_ret_    = 3
let double_     = 4
let double_ret_ = 5
  
  
(* -- Program Code -- *)
    
let rec final ms = ms
  
and foo ms = ms         |>   
    addi a0 zero 30     |>
    jal  double_
 
and ex1 ms = ms         |>
    jr   ra             |>
    next

and ex1_ret ms = ms     |>
    ret
    
and double ms = ms      |>
    add  v0 a0 a0       |>
    jr   ra             |>
    next        

and double_ret ms = ms  |>
    ret

  
(* -- Basic Block Info -- *)
    
let bblocks =
[|
  {func=final;      name="final";       nextid=na_;         dist=0; addr=0x00000000; caller=false};
  {func=foo;        name="foo";         nextid=ex1_;        dist=1; addr=0x00400200; caller=true };
  {func=ex1;        name="ex1";         nextid=ex1_ret_;    dist=0; addr=0x00400200; caller=false};
  {func=ex1_ret;    name="ex1_ret";     nextid=na_;         dist=0; addr=0x00400200; caller=false};
  {func=double;     name="double";      nextid=double_ret_; dist=0; addr=0x00400200; caller=false};
  {func=double_ret; name="double_rert"; nextid=na_;         dist=0; addr=0x00400200; caller=false};
|]

  
(* -- Start of Analysis -- *)

let main = analyze foo_ bblocks []













      

