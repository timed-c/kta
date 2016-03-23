


open Printf
open AbstractMIPS 


(* -- Basic Block Identifiers -- *)

let exp1_   = 0
let loop_   = 1
let return_ = 2


(* -- Start of analysis -- *)

      
let rec main = 
    printf "Hello\n"

(* -- Program code -- *)

and exp1 ps = ps       |>   
    addi  v0 zero 0    |>
    next

and loop ps = ps       |>
    add   v0 v0 a0     |>
    addi  a0 a0 (-1)   |>	
    bne	  a0 a1 loop_  

and return ps = ps     |>
    jr	  ra           


(* -- Basic Block Info -- *)


let blocks =
[
  {func=exp1;   nextid=loop_;   addr=0};
  {func=loop;   nextid=return_; addr=4};
  {func=return; nextid=exit_;   addr=16};
]
    


(*
  priority queue


type pqueue = distance * blockid * count * list pstate

*)

