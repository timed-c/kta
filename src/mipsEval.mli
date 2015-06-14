

open Ustring.Op

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


val eval : MipsAst.program -> string -> int -> machinestate
(** [eval program function timeout] evaluates function named [function]. 
    Returns the state of the machine after execution of the function. *)

