
open LlvmAst
open Ustring.Op

exception Eval_error_identfier_not_found of string 

(* Type of runtime values in the evaluation semantics *)
type llVal =
| VConst of       (* Constant value *)
    llConst         (* Constant. See llvmAst.mli *)
| VPtr of         (* Pointer. Modeled using an index and a mutable array *)
    int *           (* Index to where in the array the pointer points *)
    llVal array     (* Array of constants. *)

type time = int
(** Time expressed in the given measured unit of time. Default is in
    nano seconds. *)

type btime = llabel -> llabel -> time
(** Expression [btime b1 b2] returns the execution time of executing
    basic block [b1] and then branching to block [b2]. If [b1] is
    equal to [b2], then the returned time is the time it takes to
    execute [b1] without branching. *)


val v32 : int -> llVal
(** Creates a constant 32 bit integer value *)


val eval_fun : llModule -> btime -> llGloId -> llVal list -> 
  time -> (time * llVal option)
(** Expression [eval_fun m bt f args timeout] evaluates function [f]
    that is defined in module [m] using the argument list
    [args]. Timing of basic blocks are defined using function
    [bt]. The function is evaluated until it terminates or the
    [timeout] is reached. Note that a negative [timeout] value means
    that there is no timeout.  The function returns (if it terminates)
    a tuple [(t,v)], where [t] is the time it took to execute the
    function and where [v] is the returned value. *)

