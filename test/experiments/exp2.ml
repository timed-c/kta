

open Printf
open AbstractMIPS

let add state rd rs rt =
  state

let addi state rt rs imm =
  state     


(* ------------------ *)

let rec exp1 state = state |> 
    addi  (v0 s) (zero s) 0  |>
    continue 
    

      


(*
exp1:
	addi	$v0,$zero,0
loop:   add 	$v0,$v0,$a0
	addi	$a0,$a0,-1	
	bne	$a0,$a1,loop
	jr	$ra
*)



let main =
    printf "Hello 2\n"

