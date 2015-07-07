

open Ustring.Op
open MipsAst
exception Function_not_found of string

exception Out_of_Bound of string
    
type machinestate = 
{
  registers  : int32 array;
  data       : bytes;
  sdata      : bytes;
  bss        : bytes;
  sbss       : bytes;
  rodata     : bytes;
  stack      : bytes;
  mutable pc : int;
  mutable hi : int32;
  mutable lo : int32;
}



val getmemptr : machinestate -> program -> int -> int -> (bytes * int * int)
(** [getmemptr state prog addr size] takes address [addr] for the machinestate
    [state] and the expected access [size]. It returns a triple
    containing i) the byte array containing the memory ii) the
    position in the byte array, and iii) the size in bytes that are
    left in the array from the pointer position. An expection
    [Out_of_bound] is raised if the accesses can be outside a
    section. *)

val pprint_state : machinestate -> ustring
(** Pretty prints the machine state *)


val cycle_count : MipsAst.inst -> int -> MipsAst.program ->  machinestate ->
                  bool -> string option -> (int * string option) -> ((int * string option) * bool )
(** This is an example opfunc that counts the number of elapsed clock
    cycles during an function evaluation. This function assumes a
    5-stage pipeline without cache (each memory load or store takes one
    clock cycle). *)


val debug_print : MipsAst.inst -> int -> MipsAst.program ->  machinestate ->
                  bool -> string option -> (ustring * int32 array) -> ((ustring * int32 array) * bool)
(** This function can be used to debug print each step of a program
    evaluation.  Note that this function should be supplied as an opfunc
    to the [eval] function. *)


val init : MipsAst.program -> string -> int32 list -> machinestate
(** [init prog function args] initializes a
    function that can then later be executed. The function takes the
    name of the [function] that should be executed is input.  [args]
    is the list 32-bit integer argument values. Note that right now,
    only 4 arguments are supported. Argument [stackaddr] is the
    address the stack memory and [stacksize] the size of the
    stack. This function returns the state of the machine before
    execution. *)


val eval : ?bigendian:bool -> MipsAst.program -> machinestate ->
           (MipsAst.inst -> int -> MipsAst.program -> machinestate -> 
             bool -> string option -> 'a -> ('a * bool)) -> 
            'a -> (machinestate * 'a)
(** [eval program state hookfunc hookinit] evaluates a program, starting
    at the machine state [state]. The [hookfunc] is a function that is
    called after that each instruction is executed. The [hookfunc]
    function takes 7 arguments: 1) the last executed instruction, 2)
    the program pointer for the instruction 3) the program record 4)
    the state after executing the instruction, 5) a boolean that is
    true if the instruction was executed using delay slot, 6) an
    option value that says if the evaluation will be terminated after
    this step, and 7) the state that should be nested through the
    execution using [opfunc]. The function returns a tuple containing
    the nested state and a boolean flag. If the returned bool is true,
    the eval function should terminate directly. The [hookinit] is the
    first state value that is nested through the execution. The [eval]
    function returns the state of the machine after execution of the
    function and the nested state that is created by the [opfunc]
    function. *)


