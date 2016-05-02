
open Printf
open AbstractMIPS 
open Ustring.Op
  
(* -- Basic Block Identifiers -- *)

let final_     = 0
let exper_     = 1
let ex1_       = 2
let l3_        = 3
let l5_        = 4
let ex2_       = 5
let l4_        = 6
let ex4_       = 7
let ex3_       = 8
let l2_        = 9
let l1_        = 10
let exper_ret_ = 11
  
(* -- Program Code -- *)

let final ms = ms  

let exper ms = ms        |>
  beqds   a0 zero l1_    |>
  addu    a2 zero zero   |>
  next

let ex1 ms = ms          |>
  addu    v0 zero zero   |>
  next  

let l3 ms = ms           |>
  addiu   a2 a2 1        |>
  beqds   a2 a0 l2_      |>
  sll     zero zero 0    |>
  next

let l5 ms = ms           |>
  blezds  a2 l3_         |>
  addu    v1 a2 zero     |>
  next
  
let ex2  ms = ms         |>
  sll     a1 a2 1        |>
  addu    v0 v0 v1       |>
  next

let l4 ms = ms           |>
  addiu   v1 v1 1        |>
  bnelds  a1 v1 ex4_     |>
  next        

let ex4  ms = ms         |>
  addu    v0 v0 v1       |>
  next     

let ex3 ms = ms          |>
  addiu   a2 a2 1        |>
  bneds   a2 a0 l5_      |>
  sll     zero zero 0    |>
  next

let l2 ms = ms           |>
  jrds    ra             |>
  sll     zero zero 0    |>
  next 
    
let l1 ms = ms           |>
  jrds    ra             |>
  addu    v0 zero zero   |>
  next

let exper_ret ms = ms    |>
  ret
   
(* -- Basic Block Info -- *)
    
let bblocks =
[|
  {func=final;    name="final";    nextid=na_;        dist=0; addr=0x00000000; caller=false};
  {func=exper;    name="exper";    nextid=ex1_;       dist=8; addr=0x00000000; caller=false};
  {func=ex1;      name="ex1";      nextid=l3_;        dist=7; addr=0x00000000; caller=false};
  {func=l3;       name="l3";       nextid=l5_;        dist=6; addr=0x00000000; caller=false};
  {func=l5;       name="l5";       nextid=ex2_;       dist=5; addr=0x00000000; caller=false};
  {func=ex2;      name="ex2";      nextid=l4_;        dist=4; addr=0x00000000; caller=false};
  {func=l4;       name="l4";       nextid=ex3_;       dist=3; addr=0x00000000; caller=false};
  {func=ex4;      name="ex4";      nextid=l4_;        dist=4; addr=0x00000000; caller=false};
  {func=ex3;      name="ex3";      nextid=l2_;        dist=2; addr=0x00000000; caller=false};
  {func=l2;       name="l2";       nextid=exper_ret_; dist=1; addr=0x00000000; caller=false};
  {func=l1;       name="l1";       nextid=exper_ret_; dist=1; addr=0x00000000; caller=false};
  {func=exper_ret;name="exper_ret";nextid=na_;        dist=0; addr=0x00000000; caller=false};
|]

  
  
(* -- Start of Analysis -- *)

let main = analyze exper_ bblocks ["a0=300"]


(* 
ASM code, compiled by GCC and exported with KTA disasm

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



      

