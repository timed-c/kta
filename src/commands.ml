(* 
Copyright (c) 2015, David Broman
All rights reserved.

Redistribution and use in source and binary forms, with or without 
modification, are permitted provided that the following conditions are met:

    * Redistributions of source code must retain the above copyright 
      notice, this list of conditions and the following disclaimer.
    * Redistributions in binary form must reproduce the above copyright notice, 
      this list of conditions and the following disclaimer in the 
      documentation and/or other materials provided with the distribution.
    * Neither the name of the Linköping University nor the names of its 
      contributors may be used to endorse or promote products derived from 
      this software without specific prior written permission.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" 
AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE 
IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE 
DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE 
FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL 
DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; 
LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON 
ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT 
(INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS 
SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
*)

open Ustring.Op
open Printf
open MipsAst
open MipsEval

(* ---------------------------------------------------------------------*)
(* Types of options. *)
type compOpTypes =
  (* General compiler options used by all commands *)
| OpCompile
| OpDisasm_Addr
| OpExec_Func
| OpExec_Args
| OpExec_RegStart
| OpExec_RegEnd
| OpExec_Res
| OpExec_All
| OpSym_CommaSep
| OpTa_Tafiles 
(*| OpTa_Func *)
(*| OpTa_Args *)
| OpTa_Exhaustive

(* List of compiler options *)
let compile_options = 
  [(OpCompile,     Uargs.No, us"-compile",     us"",
       us"Assume that the input files are C files. These files are then " ^.
       us"compiled and used as input instead of a binary ELF executable." )]


(* List of disasm command options *)
let disasm_options = 
  [(OpDisasm_Addr, Uargs.No,  us"-addr",  us"",
       us"Output the address of each instruction.")]
  @ compile_options

    
(* List of exec command options *)
let exec_options = 
  [(OpExec_Func, Uargs.Str,  us"-func",  us" <name>",
       us"Function name that is used for the execution. The default name is 'main'.");
   (OpExec_Args, Uargs.IntList,  us"-args",  us" <args>",
       us"Arguments to the function. One or more arguments can be listed after option " ^.
       us"'-args'. The argument values need to be integers.");
   (OpExec_RegStart, Uargs.No,  us"-regstart",  us"",
       us"Output the state of the registers before the execution starts.");
   (OpExec_RegEnd, Uargs.No,  us"-regend",  us"",
       us"Output the state of the registers at the end when the function terminates.");
   (OpExec_Res, Uargs.No,  us"-res",  us"",
       us"Output the result registers and the data to where it points in memory.");
   (OpExec_All, Uargs.No,  us"-all",  us"",
       us"Output all execution information. This is the same as enabling options " ^.
       us"'-regstart', '-regend', and '-res'.")]
  @ compile_options

    
(* List of symbol command options *)
let sym_options =
  [(OpSym_CommaSep, Uargs.No,  us"-commasep",  us"",
       us"Returns all symbols without addresses as a comma separated list.")]
  @ compile_options
    
(* List of timing analysis command options *)
let ta_options =
  [(OpTa_Tafiles, Uargs.StrList,  us"-tafiles",  us" <files>",
       us"One or more timing analysis files (.ta)");
 (*  (OpTa_Func, Uargs.Str,  us"-func",  us" <name>",
       us"Function name that is used in the timing analysis. " ^.
       us"Note that this parameter cannot be used together with a " ^.
       us"timing analysis file.");
   (OpTa_Args, Uargs.IntList,  us"-args",  us" <args>",
       us"Arguments to the function supplied by the -func option. " ^.
       us"One or more arguments can be listed after option " ^.
       us"'-args'. The argument values need to be integers or integer intervals " ^.
       us"using the syntax l..u, where l is the lower bound and u the upper bound."); *)
   (OpTa_Exhaustive, Uargs.No,  us"-exhaustive",  us"",
       us"Performs exhaustive search of all possible program paths.")]
  @ compile_options
    

    
(* Temp file name that is used if the program needs to be compiled *)
let tmpfile = "temp-file-090916-070704.tmp"

(* ---------------------------------------------------------------------*)
let help command toptext =
  (* Function that takes care of the string generation *)
  let pstr command desc options =     
    Some (toptext ^. us"\n" ^.
    us"Usage: kta " ^. us command ^. us" [<files>] [<options>]\n\n" ^.
    us"Description:\n" ^. us desc ^. us"\n" ^.
    us"Options: \n" ^.
    Uargs.optionstext options)
  in
  match command with
  | "disasm" -> 
       pstr "disasm" 
         ("  Disassembles the .text section and pretty prints it to stdio.\n")
        disasm_options         
  | "exec" -> 
       pstr "exec" 
         ("  Concrete execution of a function. Outputs the number of used clock cycles.\n")
        exec_options         
  | "sym" -> 
       pstr "sym" 
       ("  Prints all symbols and corresponding addresses from a a binary ELF \n" ^
        "  file, or a compiled file if option -compile is used.\n")
        sym_options
  | "ta" -> 
       pstr "ta" 
       ("  Performs timing analysis according to the requests provided in the\n" ^
        "  timing analysis files (supplied by the -tafiles option) or\n" ^
        "  by using the -func and -args options. \n")
        ta_options
  | _ -> None


(* ---------------------------------------------------------------------*)
let getBinFileName ops args =
  let sargs = List.map Ustring.to_utf8 args in
 if Uargs.has_op OpCompile ops then   
    try   
      MipsSys.pic32_compile sargs false true tmpfile; tmpfile
    with Sys_error m -> raise (Uargs.Error (us"Compilation error: " ^. us m))
 else 
  (match sargs with 
  | [fname] -> fname 
  | _ -> raise (Uargs.Error (us"One binary ELF executable file is missing.")))

(* ---------------------------------------------------------------------*)
let remove_tempfile() =
  Sys.remove tmpfile
  


(* ---------------------------------------------------------------------*)
let disasm_command args =
  (* Parse options and get the binary file name *)
  let (ops,args) = Uargs.parse args disasm_options in
  let binfile_name = getBinFileName ops args in
  
  try
    (* Read program *)
    let prog =  MipsUtils.add_branch_symbols (MipsSys.get_program binfile_name) in

    (* Clean up *)
    remove_tempfile();

    (* Pretty print the disassembled code *)
    let print_addr = Uargs.has_op OpDisasm_Addr ops in
    MipsUtils.pprint_asm prog prog.text_sec.addr prog.text_sec.size print_addr
   
  with Sys_error m -> (remove_tempfile(); 
                       raise (Uargs.Error (us"System error: " ^. us m)))


(* ---------------------------------------------------------------------*)
let exec_command args =
  (* Stack constants. Should be command options *)
  let stack_ptr = 0x80000000 - 8  in
  let stack_size = 1024*256  in
  let stack_addr = stack_ptr - stack_size + 8 in
  
  (* Parse options and get the binary file name *)
  let (ops,args) = Uargs.parse args exec_options in
  let binfile_name = getBinFileName ops args in

  (* Get the function name *)
  let func =
    if Uargs.has_op OpExec_Func ops
    then Uargs.str_op OpExec_Func ops |> Ustring.to_utf8
    else "main" in

  (* Function arguments *)
  let args = List.map Int32.of_int (Uargs.intlist_op OpExec_Args ops) in
  
  try
    (* Load the program *)
    let prog = MipsSys.assign_program_stack (MipsSys.get_program binfile_name) 
               stack_ptr stack_size stack_addr in

    (* Initialize the state *)
    let initstate = MipsEval.init prog func args in

    (* Pretty print the start register state, before execution *)
    let str =
      if Uargs.has_op OpExec_RegStart ops || Uargs.has_op OpExec_All ops then
        MipsEval.pprint_state initstate ^. us"\n\n"
      else us"" in

    (* Evaluate/execute the function *)
    let (state,(count,terminate)) =
      MipsEval.eval prog initstate MipsEval.cycle_count (0,None)  in

    
    (* Pretty print the start register state, after execution *)
    let str = str ^.
      if Uargs.has_op OpExec_RegEnd ops || Uargs.has_op OpExec_All ops then
        MipsEval.pprint_state state ^. us"\n\n"
      else us"" in

    (* Pretty print results *)
    let str = str ^. 
      if Uargs.has_op OpExec_Res ops || Uargs.has_op OpExec_All ops then 
        let res = Int32.to_int state.registers.(reg_v0) in
        us(sprintf"Register $v0 = %d (0x%x)\n" res res) ^.
        (try
           let (b,ptr,maxval) = MipsEval.getmemptr state prog res 1 in
           us"\nData memory:\n" ^.
           (MipsUtils.pprint_bytes b ptr (min maxval 32) res false)
         with _ -> us"") ^. us"\n"
      else us""
    in
        
    (* Output finished time *)
    let str = str ^.
      us(sprintf "Execution of function '%s' successfully " func) ^.
      us(sprintf "terminated after executing %d clock cycles.\n" count)
    in        

    (* Clean up *)
    remove_tempfile(); str

  with
  | Sys_error m -> (remove_tempfile(); raise (Uargs.Error (us"System error: " ^. us m)))
  | Function_not_found m -> (remove_tempfile(); raise (Uargs.Error
                         (us"Execution error: Function '" ^. us m ^. us"' cannot be found.")))
    

    

    
(* ---------------------------------------------------------------------*)
let sym_command args =
  (* Parse options and get the binary file name *)
  let (ops,args) = Uargs.parse args sym_options in
  let binfile_name = getBinFileName ops args in

  (* Read the symbol table *)
  let symtbl =
    try 
      MipsSys.symbol_table binfile_name
    with Sys_error m -> (remove_tempfile();
                         raise (Uargs.Error (us"System error: " ^. us m)))
  in
  remove_tempfile();

  if Uargs.has_op OpSym_CommaSep ops then
    (* Pretty print as a comma separated list *)
    symtbl |> List.split |> fst |> List.map us |> Ustring.concat (us",")
  else 
    (* Pretty print all symbols together with address *)
    symtbl |> List.map (fun (s,a) -> us(sprintf "0x%08x  %s\n" a s)) 
      |> List.fold_left (^.) (us"") 



(* ---------------------------------------------------------------------*)
let ta_command args =
  (* Stack constants. Should be command options *)
  let stack_ptr = 0x80000000 - 8  in
  let stack_size = 1024*256  in
  let stack_addr = stack_ptr - stack_size + 8 in
  
  (* Parse options and get the binary file name *)
  let (ops,args) = Uargs.parse args exec_options in
  let binfile_name = getBinFileName ops args in


  (* The request comes from ta-files? *)
  if Uargs.has_op OpTa_Tafiles ops then
     raise (Uargs.Error (us"TA-files not yet implemented."))

  (* Error. There is no request *)
  else
    raise (Uargs.Error (us"Error: Option -tafiles needs " ^.
                         us"to be used\nwith the 'ta' command."))
