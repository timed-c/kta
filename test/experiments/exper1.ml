
(* Experiment 1
   
   Simple straight code using just one block. 
   Tests argument input and termination of analysis
*)

open Printf
open AbstractMIPS 
open Ustring.Op


(* -- Basic Block Identifiers -- *)

let final_  = 0
let exper_  = 1
  
(* -- Program Code -- *)
    
let rec final ms = ms
    
and exper ms = ms        |>   
    addi  t0 zero 10     |>
    add   v0 t0 a0       |>
    next  


(* -- Basic Block Info -- *)
    
let bblocks =
[|
  {func=final;  nextid=na_;    dist=0; addr=0x0};
  {func=exper;  nextid=final_; dist=1; addr=0x00400000};
|]

  
(* -- Start of Analysis -- *)

let main =
  let args = (Array.to_list Sys.argv |> List.tl) in
  analyze exper_ bblocks
    (if args = [] then ["a0=[10,100]"]
                  else args)
  |> print_mstate
        
