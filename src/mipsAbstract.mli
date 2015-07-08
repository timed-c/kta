
open Ustring.Op
open AInt32


  
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


type distance = int array

val pprint_astate : astate -> ustring
(** [pprint_astate astate] pretty prints an abstract state [astate]. *)

  
val init : MipsAst.program -> string -> aint32 list -> astate
(** [init prog function] initializes a
    function that can then later be executed. The function takes the
    name of the [function] that should be executed is input.  [args]
    is the list 32-bit abstract values argument values. Note that right now,
    only 4 arguments are supported. These arguments must be the most general form that
    will be used when evaluating the function.  *)


val distance : MipsAst.program -> string -> aint32 list -> distance
(** [make_distance prog func args] computes the distance array that is
    used in the abstract evaluation.  This array is only needed to be
    computed once for each function *)

  
val eval : ?bigendian:bool -> MipsAst.program -> astate -> distance -> int ->
                    (bool * int * astate)   
(** [abstract_eval prog state dist timeout] evaluates program [prog],
    starting with the abstract [state], and uses the distance array
    [dist]. Parameter [timeout] states the maximal measured value before
    timeout. The function returns a triple with the following elements
    i) a result boolean where true means that there were no errors
    during evaluation, ii) a safe upper bound of the time, and iii) the abstract
    state. Note that if the result boolean is true and the measured
    value is smaller than the [timeout] parameter, the evaluation
    succeeded and the function terminated. If the WCET is
    smaller than [timeout] and the result boolean is false, the abstract
    eval failed. Note that this can be a false positive. *)

