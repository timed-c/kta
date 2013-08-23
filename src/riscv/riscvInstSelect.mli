


exception Illegal_instruction of string
(** Returned when the LLVM instruction does not have a legal format that
    can be used for instruction selection. If this exception is raised, 
    there is a bug in the code that generated the LLVM instruction 
    sequence. *)


val maximal_munch : LlvmTree.tree list -> RiscvISA.sinst list
(** Performs instruction selection for RISC-V using the maximal munch
    method (tree tiling using pattern matching). Can raise [Illegal_instruction]
    if there is something wrong with the LLVM tree.

   Some notes:
    - All LLVM conditional branches will be translated into one conditional 
      RISV-V branch instruction for the true branch, followed by a 
      unconditional jump (J), for the false branch. Later during address
      assigned, the unconditional jump can be eliminated if the block that
      follows in memory is the false branch.
    - Machine register r0 my be selected during instruction selection.
*)
