

open Aint32Interval

type registers =
| zero | at | v0 | v1 | a0 | a1 | a2 | a3
| t0   | t1 | t2 | t3 | t4 | t5 | t6 | t7
| s0   | s1 | s2 | s3 | s4 | s5 | s6 | s7
| t8   | t9 | k0 | k1 | gp | sp | fp | ra 

(* ---------------------------------------------------------------------*)
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
