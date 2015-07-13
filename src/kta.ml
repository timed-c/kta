
open Printf
open Ustring.Op

(* The top message *)
let top =     
  us"KTA - KTH's Time-aware Analyzer. Copyright (C) 2015 David Broman.\n\n" ^.
  us"usage: kta [options] ELF-file \n\n" 

(* Message that is printed when no arguments are provided *)
let nothing_msg = top ^. us"One binary executable (ELF) file must be provided.\n" ^.
                         us"Use option -help to get help.\n"

(* Message that is printed when help is requested *)
let help = top ^. us"options:\n" 

(* The header for the help menu *) 
let header = top ^. us"options:\n"

(* Types of the different options *)
type optypes =
  Op_tafile | Op_exhaustive | Op_func | Op_args | Op_help
      
(* List of options. Defines behavior and help text. *)
let options =
  [(Op_tafile,     Uargs.Str, us"-tafile",     us" <file>",
       us"Specify the timing analysis (.ta) file.");
   (Op_exhaustive, Uargs.No, us"-exhaustive", us"",
       us"Perform exhaustive search of all program paths.");
   (Op_func,       Uargs.Str, us"-func",       us" <name>",
       us"Perform analysis on a specific named function. This option is ignored " ^.
       us"if the -tafile is used.");
   (Op_args,       Uargs.StrList, us"-args",  us" <values_one_more_this_can>",
       us"Specify argument to the function to be analyzed. " ^.
       us"The argument can be a concrete integer, e.g., '1', or an interval, e.g., " ^.
       us"'3..100'. Several arguments can be specified by repeating the -arg option. ");
   (Op_help,       Uargs.No, us"-help", us"",
       us"Displays this help.")
  ]

  

(* ---------------------------------------------------------------------*)
let main =
  try 
    (* Parse the arguments *)
    let (ops,args) = Uargs.parse (Sys.argv |> Array.to_list |> List.tl) options in

    (* Check if -help *)
    if Uargs.has_op Op_help ops then
      uprint_endline (help ^. Uargs.optionstext options ^. us"\n")

    (* Check if there is no ELF-file given *)
    else if List.length args < 1 then
      uprint_endline nothing_msg 

    (* Check if we should use a tafile *)   
    else if List.mem_assoc Op_tafile ops then(
      uprint_endline (us"Use ta-file: " ^. (Uargs.str_op Op_tafile ops));
      List.iter uprint_endline args        
    )

    (* Check if direct function analysis *)
    else if List.mem_assoc Op_func ops then (
      uprint_endline (us"Use func: " ^. (List.assoc Op_func ops |> List.hd));
      (if List.mem_assoc Op_args ops then (
        uprint_endline (us"Args: ");
        List.iter uprint_endline (List.assoc Op_args ops)) else ()))
          
    (* Print help *)
    else 
      List.iter uprint_endline args
    
  with 
    (* Print error message if the options were incorrect *)
    Uargs.Parse_error s ->
      uprint_endline s


      
