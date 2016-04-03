
open Printf
open AbstractMIPS 
open Ustring.Op


(* -- Basic Block Identifiers -- *)

let final_  = 0
let exper_  = 1
let loop_   = 2

  
(* -- Program Code -- *)
    
let rec final ms = ms
    
and exper ms = ms       |>   
    addi  v0 zero 0     |>
    next  

and loop ms = ms        |>
    add  v0 v0 a0       |>
    addi a0 a0 (-1)     |>	
    bne	 a0 a1 loop_ 



(* -- Basic Block Info -- *)
    
let bblocks =
[|
  {func=final;  nextid=na_;    dist=0; addr=0x00000000; joinvar = []};
  {func=exper;  nextid=loop_;  dist=2; addr=0x00400000; joinvar = []};
  {func=loop;   nextid=final_; dist=1; addr=0x00400200; joinvar = [a0;a1]};
|]

  
(* -- Start of Analysis -- *)

let main =
  let args = (Array.to_list Sys.argv |> List.tl) in
  analyze exper_ bblocks
    (if args = [] then ["a0=100";"a1=50"]
                  else args)
  |> print_mstate












      

