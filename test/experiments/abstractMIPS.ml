

open Aint32Interval

(* ------------------------ REGISTERS ------------------------------*)

type registers =
| ZERO | AT | V0 | V1 | A0 | A1 | A2 | A3
| T0   | T1 | T2 | T3 | T4 | T5 | T6 | T7
| S0   | S1 | S2 | S3 | S4 | S5 | S6 | S7
| T8   | T9 | K0 | K1 | GP | SP | FP | RA 

let zero = ZERO
let at = AT
let v0 = V0
let v1 = V1
let a0 = A0
let a1 = A1
let a2 = A2
let a3 = A3
let t0 = T0
let t1 = T1
let t2 = T2
let t3 = T3
let t4 = T4
let t5 = T5
let t6 = T6
let t7 = T7
let s0 = S0
let s1 = S1
let s2 = S2
let s3 = S3
let s4 = S4
let s5 = S5
let s6 = S6
let s7 = S7
let t8 = T8
let t9 = T9
let k0 = K0
let k1 = K1
let gp = GP
let sp = SP
let fp = FP
let ra = RA


type astate = {
  register_at : aint32;
  register_v0 : aint32;
  register_v1 : aint32;
  register_a0 : aint32;
  register_a1 : aint32;
  register_a2 : aint32;
  register_a3 : aint32;
  register_t0 : aint32;
  register_t1 : aint32;
  register_t2 : aint32;
  register_t3 : aint32;
  register_t4 : aint32;
  register_t5 : aint32;
  register_t6 : aint32;
  register_t7 : aint32;
  register_s0 : aint32;
  register_s1 : aint32;
  register_s2 : aint32;
  register_s3 : aint32;
  register_s4 : aint32;
  register_s5 : aint32;
  register_s6 : aint32;
  register_s7 : aint32;
  register_t8 : aint32;
  register_t9 : aint32;
  register_k0 : aint32;
  register_k1 : aint32;
  register_gp : aint32;
  register_sp : aint32;
  register_fp : aint32;
  register_ra : aint32;

  pc : int;
}

(* ------------------------ INSTRUCTIONS -------------------------*)

let add state rd rs rt =
    state

let addi state rt rs imm =
    state

let bne state rs rt label =
    state

let jr state rs =
    state

let continue state =
    state











