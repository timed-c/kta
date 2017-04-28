
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
open TaFileTypes

(* ---------------------------------------------------------------------*)
(* Types of options. *)
type compOpTypes =
  (* General compiler options used by all commands *)
| OpCompile
| OpVerbose
| OpOptimize
| OpDisasm_Addr
| OpExec_Func
| OpExec_Args
| OpExec_RegStart
| OpExec_RegEnd
| OpExec_Res
| OpExec_All
| OpSym_CommaSep
| OpTa_Tafile 
(*| OpTa_Func  *)
(*| OpTa_Args  *)
| OpTa_Exhaustive
| OpTa_OutputPathInputs    
| OpWCET_CPSOCaml
| OpWCET_OCamlArgs
| OpWCET_BSConfig
| OpWCET_MCConfig  
| OpWCET_Tasks  

(* List of compiler options *)
let extra_options = 
  [(OpCompile,     Uargs.No, us"-compile",     us"",
       us"Assume that the input files are C files. These files are then " ^.
       us"compiled and used as input instead of a binary ELF executable." );
   (OpOptimize,   Uargs.Int, us"-optimize",     us"<level>",
       us"Compile the C program with optimization enabled.");
   (OpVerbose,     Uargs.No, us"-verbose",     us"",
       us"Print out extra information, such as invocation of external compilers.")]


(* List of disasm command options *)
let disasm_options = 
  [(OpDisasm_Addr, Uargs.No,  us"-addr",  us"",
       us"Output the address of each instruction.")]
  @ extra_options

    
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
  @ extra_options


(* List of sections command options *)
let sections_options =
    extra_options

    
(* List of symbol command options *)
let sym_options =
  [(OpSym_CommaSep, Uargs.No,  us"-commasep",  us"",
       us"Returns all symbols without addresses as a comma separated list.")]
  @ extra_options
    
(* List of timing analysis command options *)
let ta_options =
  [(OpTa_Tafile, Uargs.StrList,  us"-tafile",  us" <files>",
       us"One or more timing analysis files (.ta)");
 (*  (OpTa_Func, Uargs.Str,  us"-func",  us" <name>",
       us"Function name that is used in the timing analysis. " ^.
       us"Note that this parameter cannot be used together with a " ^.
       us"timing analysis file.");
   (OpTa_Args, Uargs.IntList,  us"-args",  us" <args>",
       us"Arguments to the function supplied by the -func option. " ^.
       us"One or more arguments can be listed after option " ^.
       us"'-args'. The argument values need to be integers or integer intervals " ^.
       us"usbing the syntax l..u, where l is the lower bound and u the upper bound."); *)
   (OpTa_Exhaustive, Uargs.No,  us"-exhaustive",  us"",
    us"Performs exhaustive search of all possible program paths.");
   (OpTa_OutputPathInputs, Uargs.No,  us"-output-path-inputs",  us"",
    us"For each of the visited pats, the input values are printed to stdout.")
   ]
  @ extra_options
    

(* List of disasm command options *)
let wcet_options = 
  [(OpWCET_CPSOCaml, Uargs.No,  us"-cpsocaml",  us"",
    us"Output the OCaml continuation passing style (CPS) code used for analysis.");
   (OpWCET_OCamlArgs, Uargs.StrList,  us"-args",  us"<args>",
    us"Run the OCaml Code directly with <args> in the form: <reg>=[int,int]|int. Set environment variable KTA_WCET_RUNTIME_PATH=$KTA_PATH/runtime.");
   (OpWCET_BSConfig, Uargs.Int,  us"-bsconfig",  us"",
    us"Configure maximum batch size.");
   (OpWCET_MCConfig, Uargs.Int,  us"-max_cycles",  us"",
    us"Configure maximum cycles allowed.");
   (OpWCET_Tasks, Uargs.StrList,  us"-tasks",  us"",
    us"Time interfering tasks.")
  ]
  @ extra_options

    
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

  (* Select the main command *)
  match command with
  | "disasm" -> 
       pstr "disasm" 
         ("  Disassembles the .text section and pretty prints it to stdio.\n")
        disasm_options         
  | "exec" -> 
       pstr "exec" 
         ("  Concrete execution of a function. Outputs the number of used clock cycles.\n")
        exec_options         
  | "sections" -> 
       pstr "sections" 
       ("  Prints out sections information for a binary ELF \n" ^
        "  file, or a compiled file if option -compile is used.\n")
        sections_options
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
  | "wcet" -> 
       pstr "wcet" 
       ("  Performs worst case timing analysis. \n")
        wcet_options
  | _ -> None


(* ---------------------------------------------------------------------*)
let getBinFileName ops args =
  let sargs = List.map Ustring.to_utf8 args in
  if Uargs.has_op OpCompile ops || List.exists (Ustring.ends_with (us".c")) args then   
   try
     let opt = if Uargs.has_op OpOptimize ops then
                 Uargs.int_op OpOptimize ops
               else 0 in
     MipsSys.pic32_compile sargs false opt tmpfile; tmpfile
   with Sys_error m -> raise (Uargs.Error (us"Compilation error: " ^. us m))
  else 
    (match sargs with 
     | [fname] -> fname 
     | _ -> raise (Uargs.Error (us"One binary ELF executable file is missing.")))

(* ---------------------------------------------------------------------*)
let remove_tempfile() =
  if Sys.file_exists tmpfile then Sys.remove tmpfile else ()
  

(* ---------------------------------------------------------------------*)
let parse_ops_get_filename args options =
  let rec partition_str lst files names =
    match lst with
    | n::ns ->
        if names = [] then
          try let _ = Ustring.rindex n (uc('.')) in
              partition_str ns (n::files) names
          with _ -> partition_str ns files (n::names)
        else
          partition_str ns files (n::names)
    | [] -> (List.rev files, List.rev names)
  in
  
  (* Parse options*)
  let (ops,args) = Uargs.parse args options in

  let (args,funcargs) = partition_str args [] [] in
  
  (* Enable verbose mode, if selected *)
  if Uargs.has_op OpVerbose ops then MipsSys.verbose true;

  (* Get the binary file name *)
  let binfile_name = getBinFileName ops args in
  (ops,args,binfile_name,funcargs)


(* ---------------------------------------------------------------------*)
let disasm_command args =
  try
    (* Parse options and get the binary file name *)
    let (ops,args,binfile_name,_) = parse_ops_get_filename args disasm_options in

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
  let (ops,args,binfile_name,_) = parse_ops_get_filename args exec_options in

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
let sections_command args =

  (* Parse options and get the binary file name *)
  let (ops,args,binfile_name,_) = parse_ops_get_filename args sym_options in

  (* Read the symbol table *)
  let sections_list =
    try 
      MipsSys.section_info binfile_name
    with Sys_error m -> (remove_tempfile();
                         raise (Uargs.Error (us"System error: " ^. us m)))
  in
  remove_tempfile();

  (* Pretty print all sections *)
    sections_list |> List.map (fun (k,(s,a)) -> us(sprintf "%s %d,0x%x\n" k s a)) 
      |> List.fold_left (^.) (us"") 
    

    
(* ---------------------------------------------------------------------*)
let sym_command args =

  (* Parse options and get the binary file name *)
  let (ops,args,binfile_name,_) = parse_ops_get_filename args sym_options in

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
  (* Command short cut if the user writes "kta ta myfile.ta" *)
  let newargs = 
    match args with
    | s::ss when Ustring.ends_with (us".ta") (us s) -> 
         let name = Ustring.to_utf8 
                (Ustring.sub (us s) 0 ((String.length s) - 3)) in
         [name ^ ".c"; "-compile"; "-tafile"; name ^ ".ta"] @ ss
    | _ -> args
  in                      
  
  (* Stack constants. Should be command options *)
  let stack_ptr = 0x80000000 - 8  in
  let stack_size = 1024*256  in
  let stack_addr = stack_ptr - stack_size + 8 in

  (* Parse options and get the binary file name *)
  let (ops,args,binfile_name,_) = parse_ops_get_filename newargs ta_options in
        
  (* Perform the timing analysis *)
  try (

    (* Load the program *)
    let prog = MipsSys.assign_program_stack 
               (MipsSys.get_program binfile_name |>  MipsUtils.add_branch_symbols) 
                stack_ptr stack_size stack_addr in

    (* TODO: check that all symbols in the TA files actually exists in the binary
       and that "entry" and "exit" TPPs does not exist in the binary. *)


    (* Not so fast symbol lookup function. TODO: redesign prog in MipsAST *)
    let sym2addr id = 
      MipsAst.Sym2Addr.find (id |> ustring_of_sid |> Ustring.to_utf8) prog.sym2addr  
    in
    let addr2str addr = 
      MipsAst.Addr2Sym.find addr prog.addr2sym 
    in
  
    
    (* The request comes from ta-files? *)
    if Uargs.has_op OpTa_Tafile ops then

      (* Get the file requests *)
      let f_reqs = List.map 
        (fun f -> f |> Ustring.to_utf8 |> TaFile.parse_ta_file) 
        (Uargs.strlist_op OpTa_Tafile ops) in

      (* Iterate through the file requests *)
      List.iter (fun file_ta_req -> 

        (* Iterate through ta func request *)
        let resps = List.map (fun func_ta_req -> 

          (* Get the eval function for the  functions for the MIPS binary *)
          let eval_fun = MipsSys.get_eval_func prog func_ta_req.state in

          (* Get init states *)
          let initstates = MipsSys.get_init_state_vals prog
                           (Ustring.to_utf8 func_ta_req.initfunc) 
                           func_ta_req.state in

          (* Perform the analysis for one function ta request *)
          let outputPathInputs = Uargs.has_op OpTa_OutputPathInputs ops in
          ExhaustiveTA.analyze eval_fun func_ta_req sym2addr addr2str initstates outputPathInputs
        ) file_ta_req.func_ta_reqs in
        
        (* Currently, we only support one Function request 
           TODO: Fix multiple function requests*)
        (match resps with
         | [ta_res_list] -> 
              (* Write down the ta file *)
              let name = file_ta_req.ta_filename ^ ".out" in
              TaFile.pprint_simple_ta_res ta_res_list |> Ustring.write_file name;
              printf "The analysis results are written to file '%s'\n" name
         | _ -> failwith "Only supports one single function request right now.")
      ) f_reqs;

      (* Remove the temp file used when compiling *)
      remove_tempfile();
      us""        

    (* Error. There is no request *)
    else
      raise (Uargs.Error (us"Error: Option -tafile needs " ^.
                         us"to be used with the 'ta' command."))

  )with
  | Sys_error m -> (raise (Uargs.Error (us"System error: " ^. us m)))


(* ---------------------------------------------------------------------*)
                                                                     
let wcet_command args =
  
  (* Parse options and get the binary file name *)
  let (ops,args,binfile_name,funcargs) = parse_ops_get_filename args wcet_options in

  let prog_args = Uargs.strlist_op OpWCET_OCamlArgs ops |> List.map Ustring.to_utf8 in
  let print_out_option = (Uargs.has_op OpWCET_CPSOCaml ops) in

  let record = Uargs.has_op OpWCET_Tasks ops in
  let tasks = Uargs.strlist_op OpWCET_Tasks ops in

  let bsconfig =
    if (Uargs.has_op OpWCET_BSConfig ops) then
      Some (Uargs.int_op OpWCET_BSConfig ops)
    else
      None
  in
  let max_cycles =
    if (Uargs.has_op OpWCET_MCConfig ops) then
      Some (Uargs.int_op OpWCET_MCConfig ops)
    else
      None
  in
  (* Get function name. Should be better error control of input here... TODO *)
  let func_name = List.hd funcargs in

  (* Load the program *)
  let prog = MipsSys.get_program binfile_name |>  MipsUtils.add_branch_symbols in
  if record then
    (MipsCfg.test prog (Ustring.to_utf8 func_name) (prog_args, max_cycles, bsconfig, tasks, record, print_out_option);
     List.iter (fun fname ->
       MipsCfg.test prog (Ustring.to_utf8 fname) (prog_args, max_cycles, bsconfig, tasks, record, print_out_option)) tasks;
     MipsCfg.test prog (Ustring.to_utf8 func_name) (prog_args, max_cycles, bsconfig, tasks, false, print_out_option)
    )
  else
    MipsCfg.test prog (Ustring.to_utf8 func_name) (prog_args, max_cycles, bsconfig, [], false, print_out_option);
  us""














    
