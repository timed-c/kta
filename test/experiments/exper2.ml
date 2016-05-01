
open Printf
open AbstractMIPS 
open Ustring.Op


(* -- Basic Block Identifiers -- *)

let final_     = 0
let exper_     = 1
let loop_      = 2
let exper_ret_ = 3

  
(* -- Program Code -- *)
    
let final ms = ms
    
let exper ms = ms       |>   
    addi  v0 zero 0     |>
    next  

let loop ms = ms        |>
    add  v0 v0 a0       |>
    addi a0 a0 (-1)     |>	
    bne	 a0 a1 loop_   

let exper_ret  ms = ms  |>
    jr   ra             |>
    ret
  
(* -- Basic Block Info -- *)
    
let bblocks =
[|
  {func=final;    name="final";     nextid=na_;         dist=0; addr=0x00000000; caller=false};
  {func=exper;    name="exper";     nextid=loop_;       dist=2; addr=0x00400000; caller=false};
  {func=loop;     name="loop";      nextid=exper_ret_;  dist=1; addr=0x00400200; caller=false};
  {func=exper_ret;name="exper_ret"; nextid=na_;         dist=0; addr=0x00400200; caller=false};
|]

  
(* -- Start of Analysis -- *)

let main =
  let args = (Array.to_list Sys.argv |> List.tl) in
  analyze exper_ bblocks
    (if args = [] then ["a0=[500,2000]";"a1=[10,300]"]
                  else args)
  |> print_mstate

(* Comments
  a0=[1000,5000] a1=[400,520] takes 5.4 seconds on my laptop
   before optimized join *)










      

