


open Printf
open AbstractMIPS 




let rec dummy = 0

and exp1 s = s         |>   
    addi  v0 zero 0    |>
    continue

and loop s = s         |>
    add   v0 v0 a0     |>
    addi  a0 a0 (-1)   |>	
    bne	  a0 a1 loop   |>
    continue

and return s = s       |>
    jr	  ra           |>
    continue

let blocks =
[
  (exp1,3);
  (loop,2);
  (return,1);
]

let main = 
    printf "Hello\n"


