

open Ustring.Op

exception Function_not_found of string


type machinestate = 
{
  registers : int32 array;
  data : bytes;
  bss : bytes;
  mutable pc : int;
}



val pprint_state : machinestate -> ustring


(* val step : MipsAst.program -> MipsAst.inst list -> int ->  *)

val init : MipsAst.program -> machinestate

val cycle_count : MipsAst.inst -> int -> MipsAst.program ->  machinestate ->
                  bool -> int -> (int * bool)
(** This is an example opfunc that counts the number of elapsed clock
    cycles during an function evaluation. This function assumes a
    5-stage pipeline without cache (each memory load or store takes one
    clock cycle) *)

val debug_print : MipsAst.inst -> int -> MipsAst.program ->  machinestate ->
                  bool -> (ustring * int32 array) -> ((ustring * int32 array) * bool)



val eval : MipsAst.program -> string -> int32 list ->  
           (MipsAst.inst -> int -> MipsAst.program -> machinestate -> 
             bool -> 'a -> ('a * bool)) -> 
            'a -> (machinestate * 'a)
(** [eval program function args opfunc opinit] evaluates function
    named [function].  [args] is the list 32-bit integer argument
    values. Note that right now, only 4 arguments are supported. The
    [opfunc] is a function that is called after that each instruction
    is executed. The [opfunc] function has 7 parameters: 1) the last
    executed instruction, 2) the program pointer for the instruction 3) the program
    record 4) the state after executing the instruction, 5) a boolean
    that is true if the instruction was executed using delay slot, and
    6) the state that should be nested through the execution using
    [opfunc]. The function returns a tuple containing the nested state
    and a boolean flag. If the returned bool is true, the eval
    function should terminate directly. The [opinit] is the first
    state value that is nested through the execution. The [eval]
    function returns the state of the machine after execution of the
    function and the nested state that is created by the [opfunc]
    function. *)


