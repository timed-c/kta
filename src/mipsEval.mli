


type machinestate = 
{
  registers : int32 array;
  data : bytes;
}



val eval : MipsAst.program -> string -> machinestate
(** [eval program function] evaluates function named [function]. 
  Returns the state of the machine after execution of the function. *)
