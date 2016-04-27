
open Printf
open AbstractMIPS 
open Ustring.Op


(* -- Basic Block Identifiers -- *)

let final_    = 0
let fact_     = 1
let ex1_      = 2
let elsebr_   = 3
let ex2_      = 4
let fact_ret_ = 5
  
(* -- Program Code -- *)
    
let rec final ms = ms

and fact ms = ms         |>
    addi  sp sp (-8)     |>
    sw 	  a0 4(sp)       |>
    sw    ra 0(sp)       |>
    addi  t0 zero 2      |>
    slt   t0 a0 t0       |>
    beq   t0 zero elsebr_

and ex1 ms = ms          |>
    addi  v0 zero 1      |>
    addi  sp sp 8        |>
    jr 	  ra
    
and elsebr ms = ms       |>
    addi  a0 a0 (-1)     |>
    jal	  fact_

and ex2 ms = ms          |>
    lw	  ra 0(sp)       |>
    lw 	  a0 4(sp)	 |>
    addi  sp sp 8        |>
    mul	  v0 a0 v0       |>
    jr	  ra

and fact_ret ms = ms     |>
    ret
  
  
(* -- Basic Block Info -- *)
    
let bblocks =
[|
  {func=final;    nextid=na_;       dist=0; addr=0x00000000; caller=false};
  {func=fact;     nextid=ex1_;      dist=1; addr=0x00400200; caller=false};
  {func=ex1;      nextid=fact_ret_; dist=0; addr=0x00400200; caller=false};
  {func=elsebr;   nextid=ex2_;      dist=0; addr=0x00400200; caller=true};
  {func=ex2;      nextid=fact_ret_; dist=0; addr=0x00400200; caller=false};
  {func=fact_ret; nextid=na_;       dist=0; addr=0x00400200; caller=false};
|]

  
(* -- Start of Analysis -- *)

let main =
  let args = (Array.to_list Sys.argv |> List.tl) in
  analyze fact_ bblocks
    (if args = [] then ["a0=4"]
                  else args)
  |> print_mstate













      

