

open Printf
open Ustring.Op


type arg_error_type = 
  ArgErrorUnkownFileExtension of ustring

exception Argument_error

type files_record = {
  c : ustring list;
  ll: ustring list;
}

(** The [files_record] type contains all files that are given on the command line
    to ptc. Note that all file names are given without their extension. *)
  

let compile_with_clang files =
  Sys.command ("clang -S -O1 -m32 -emit-llvm " ^ List.hd files ^ ".c -o " ^ List.hd files ^ ".ll")



let main =
  printf "ptc - precision timed compiler\n";
  compile_with_clang (List.tl (Array.to_list (Sys.argv)))

