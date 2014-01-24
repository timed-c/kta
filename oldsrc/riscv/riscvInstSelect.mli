

exception Code_not_32_bit of string
(** Exception is raised if the input code tree is not in 32 bit form *)

exception Illegal_instruction of string
(** Returned when the LLVM instruction does not have a legal format that
    can be used for instruction selection. If this exception is raised, 
    there is a bug in the code that generated the LLVM instruction 
    sequence. *)


val maximal_munch : LlvmTree.tree list -> int -> RiscvISA.sinst list
(** [maximal_munch tlst tcount] performs instruction selection for RISC-V 
    using the maximal munch method (tree tiling using pattern matching). 
    Input to the function is a list of trees [tlst] and an integer counter 
    [tcount] stating the next available number for allocating temp variables.
    The function can raise [Illegal_instruction] if there is something wrong 
    with the LLVM tree.

   Some notes:
    - All LLVM conditional branches will be translated into one conditional 
      RISV-V branch instruction for the true branch, followed by a 
      unconditional jump (J), for the false branch. Later during address
      assigned, the unconditional jump can be eliminated if the block that
      follows in memory is the false branch.
    - Machine register r0 my be selected during instruction selection.
    - Temporary variables generated during instruction selection are named
      "tmp#<num>" where <num> is a unique integer number. Note that new
      temporary variables will never break SSA form.
*)
