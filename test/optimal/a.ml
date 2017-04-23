open AbstractMIPS

open Printf

let gp_addr=4231264
let mem = []

(* -- Basic Block Identifiers -- *)

let final_     = 0
let square_    = 1

(* -- Program Code -- *)

let final ms = ms

(* Function: square *)

let square ms = ms                      |>
  addiu   sp sp (-8)                    |>
  sw      fp 4(sp)                      |>
  addu    fp sp zero                    |>
  sw      a0 8(fp)                      |>
  lw      v1 8(fp)                      |>
  lw      v0 8(fp)                      |>
  mult    v1 v0                         |>
  mflo    v0                            |>
  addu    sp fp zero                    |>
  lw      fp 4(sp)                      |>
  addiu   sp sp 8                       |>
  jrds    ra                            |>
  sll     zero zero 0                   |>
  ret


let bblocks =
[|
{func=final;   name="final";  nextid=na_;   dist=0;   addr=0x00000000; caller=false;};
{func=square;  name="square"; nextid=na_;   dist=0;   addr=0x00400018; caller=false;};
|]

(* -- Start of Analysis -- *)

let main = 
	let _st_time = Sys.time() in
	analyze square_ bblocks gp_addr mem [];
	printf "Time Elapsed %fs\n" (Sys.time() -. _st_time)


open AbstractMIPS

open Printf

let gp_addr=4231264
let mem = []

(* -- Basic Block Identifiers -- *)

let final_     = 0
let square_    = 1

(* -- Program Code -- *)

let final ms = ms

(* Function: square *)

let square ms = ms                      |>
  addiu   sp sp (-8)                    |>
  sw      fp 4(sp)                      |>
  addu    fp sp zero                    |>
  sw      a0 8(fp)                      |>
  lw      v1 8(fp)                      |>
  lw      v0 8(fp)                      |>
  mult    v1 v0                         |>
  mflo    v0                            |>
  addu    sp fp zero                    |>
  lw      fp 4(sp)                      |>
  addiu   sp sp 8                       |>
  jrds    ra                            |>
  sll     zero zero 0                   |>
  ret


let bblocks =
[|
{func=final;   name="final";  nextid=na_;   dist=0;   addr=0x00000000; caller=false;};
{func=square;  name="square"; nextid=na_;   dist=0;   addr=0x00400018; caller=false;};
|]

(* -- Start of Analysis -- *)

let main = 
	let _st_time = Sys.time() in
	analyze square_ bblocks gp_addr mem [];
	printf "Time Elapsed %fs\n" (Sys.time() -. _st_time)


