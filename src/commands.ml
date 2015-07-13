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

(* ---------------------------------------------------------------------*)
(* Types of options. *)
type compOpTypes =
  (* General compiler options used by all commands *)
| OpCompile
| OpSym_CommaSep 

(* List of compiler options *)
let compile_options = 
  [(OpCompile,     Uargs.No, us"-compile",     us"",
       us"Assume that the input files are C files. These files are then " ^.
       us"compiled and used as input instead of a binary ELF executable." )]

(* List of symbol command options *)
let sym_options =
  [(OpSym_CommaSep, Uargs.No,  us"-commasep",  us"",
       us"Returns all symbols without addresses as a comma separated list.")]
  @ compile_options

let tmpfile = "temp-file-090916-070704.tmp"

(* ---------------------------------------------------------------------*)
let help command toptext =
  (* Function that takes care of the string generation *)
  let pstr command desc options =     
    Some (toptext ^. us"\n" ^.
    us"usage: kta " ^. us command ^. us" [<files>] [<options>]\n\n" ^.
    us"description:\n" ^. us desc ^. us"\n" ^.
    us"options: \n" ^.
    Uargs.optionstext options)
  in
  match command with
  | "sym" -> 
       pstr "sym" 
       ("  Prints all symbols and corresponding addresses from a a binary ELF \n" ^
        "  file, or a compiled file if option -compile is used.\n")
       sym_options
  | _ -> None


(* ---------------------------------------------------------------------*)
let getBinFile ops args =
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
let sym_command args =
  (* Parse options and get the binary file name *)
  let (ops,args) = Uargs.parse args sym_options in
  let binfile = getBinFile ops args in

  (* Read the symbol table *)
  let symtbl = MipsSys.symbol_table binfile in
  remove_tempfile();

  if Uargs.has_op OpSym_CommaSep ops then
    (* Pretty print as a comma separated list *)
    symtbl |> List.split |> fst |> List.map us |> Ustring.concat (us",")
  else 
    (* Pretty print all symbols together with address *)
    symtbl |> List.map (fun (s,a) -> us(sprintf "0x%08x  %s\n" a s)) 
      |> List.fold_left (^.) (us"") 


   











