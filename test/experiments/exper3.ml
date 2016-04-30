
open Printf
open AbstractMIPS 
open Ustring.Op


(* -- Basic Block Identifiers -- *)

let final_  = 0
let exper_  = 1
let loop_   = 2

  
(* -- Program Code -- *)
    
let rec final ms = ms

and exper ms = ms        |>
  beqds   a0 zero l1_    |>
  addu    a2 zero zero   |>
  nextds  a0 zero l1_

and extra1 ms = ms       |>
  addu    v0 zero zero   |>
  next  

and l3 ms = ms           |>
  addiu   a2 a2 1        |>
  beq_ds  a2 a0 l2_      |>
  sll     zero zero 0    |>
  next_ds 

and l5 ms = ms           |>
  blezds a2,l3_         |>
  addu    v1 a2 zero     |>
  next_ds
  
and extra2 ms = ms       |>
  sll     a1 a2 1        |>
  addu    v0 v0 v1       |>
  next
  
and l4 ms = ms           |>
  addiu   v1 v1 1        |>
  bnel_ds a1 v1 l4_      |>
  addu    v0 v0 v1       |>
  next_ds

and extra3 ms = ms       |>
  addiu   a2 a2 1        |>
  bne_ds  a2 a0 l5_      |>
  sll     zero zero 0    |>
  next_ds
  
and l2 ms = ms           |>
  jr_ds   ra             |>
  sll     zero zero 0    |>
  next_ds 
    
and l1 ms = ms           |>
  jr_ds   ra             |>
  addu    v0 zero zero   |>
  next_ds


  
(* -- Basic Block Info -- *)
    
let bblocks =
[|
  {func=final;  nextid=na_;    dist=0; addr=0x00000000};
  {func=exper;  nextid=loop_;  dist=2; addr=0x00400000};
  {func=loop;   nextid=final_; dist=1; addr=0x00400200};
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



(* 

ASM code, as exported by GCC

exper:
  beq     $a0,$0,l1
  addu    $a2,$0,$0
  addu    $v0,$0,$0
l3:
  addiu   $a2,$a2,1
  beq     $a2,$a0,l2
  sll     $0,$0,0
l5:
  blez    $a2,l3
  addu    $v1,$a2,$0
  sll     $a1,$a2,1
  addu    $v0,$v0,$v1
l4:
  addiu   $v1,$v1,1
  bnel    $a1,$v1,l4
  addu    $v0,$v0,$v1
  addiu   $a2,$a2,1
  bne     $a2,$a0,l5
  sll     $0,$0,0
l2:
  jr      $ra
  sll     $0,$0,0
l1:
  jr      $ra
  addu    $v0,$0,$0

*)



      

