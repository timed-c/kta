

open Ustring.Op

exception Function_not_found of string


type machinestate = 
{
  registers : int32 array;
  data : bytes;
  bss : bytes;
  mutable ticks : int;
  mutable pc : int;
}



val pprint_state : machinestate -> ustring


(* val step : MipsAst.program -> MipsAst.inst list -> int ->  *)

val init : MipsAst.program -> machinestate


val eval : MipsAst.program -> string -> int32 list -> int -> machinestate
(** [eval program function args timeout] evaluates function named [function]. 
    [args] is the list 32-bit integer argument values. Note that right now,
    only 4 arguments are supported.
    Returns the state of the machine after execution of the function. *)

