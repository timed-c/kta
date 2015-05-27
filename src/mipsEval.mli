


type machinestate = 
{
  registers : int32 array;
  data : bytes;
  bss : bytes;
  time : int ref;
  pc : int ref;
}



(* val step : MipsAst.program -> MipsAst.inst list -> int ->  *)


val eval : MipsAst.program -> string -> machinestate
(** [eval program function] evaluates function named [function]. 
    Returns the state of the machine after execution of the function. *)

