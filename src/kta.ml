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

(** 
    KTA - KTH's Time-aware Analyzer. Copyright (C) 2015 David Broman.

    module: Main

    This is the main file for the kta tool. The main entry function
    'main' checks which command that is used and calls the correct
    function in module 'Commands'. This function also includes the
    main help functions.
*)

open Printf
open Ustring.Op


(* ---------------------------------------------------------------------*)
(* The top text message *)
let top_text =     
  us"KTA - KTH's Time-aware Analyzer. Copyright (C) 2015 David Broman.\n" 

  

(* ---------------------------------------------------------------------*)
(* The main help menu that displays all available commands. *)
let print_main_help() =
 top_text ^. us"\n" ^.
 us"usage: kta <command> [<args>] [<options>] \n\n" ^.
 us"commands:\n" ^.
 us"  disasm   Outputs the disassembled ASM code of the .text section.\n" ^. 
(* us"  exec     Concrete execution of a function.\n" ^. *)
 us"  help     Prints out help about commands.\n" ^.
(* us"  sections Prints out information about sections.\n" ^. *)
 us"  sym      Prints out the symbol table of the executable.\n" ^.
(* us"  ta       Performs timing analysis.\n" ^. *)
(* us"  trace    Prints out a debug trace of a concrete execution.\n" ^. *)
 us"\n" ^.
 us"Run 'kta help <command>' to get help for a specific command.\n"
 |> uprint_endline

    
(* ---------------------------------------------------------------------*)
(* Handle the top commands of kta. *)      
let main =
  try 
    match Array.to_list Sys.argv with
    (* Commands *)
    | _::"disasm"::args   -> Commands.disasm_command args |> uprint_endline
    | _::"exec"::args     -> printf "Execute\n"
    | _::"sections"::args -> printf "Sections\n"
    | _::"sym"::args      -> Commands.sym_command args |> uprint_endline
    | _::"ta"::args       -> printf "Timing Analysis\n"
    | _::"trace"::args    -> printf "Trace\n"

    (* Help *) 
    | _::"help"::args -> 
      (match args with 
      | ["help"] -> print_main_help()
      | [c] -> 
        (match Commands.help c top_text with
        | Some t -> uprint_endline t
        | None -> "Unknown command '" ^ c ^ "'." |> print_endline)
      | _ -> print_main_help())
      
    (* Cannot find the command *)
    | _::c::ls -> "Unknown command '" ^ c ^ "'." |> print_endline

    (* Print out the main help menu *)
    | _ -> print_main_help()

  with
    (* Print out error message *)
    Uargs.Error s -> uprint_endline s


