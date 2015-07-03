
open Ustring.Op
  
open TaFileTypes
  
  
  

val parse_ta_file : string -> file_ta_req
(** [parse_ta_file filename] takes a timing analysis file [filename]
    as input, parses it, and returns timing analysis request
    structure.  Raises exception Sys_error if the file cannot be
    found. Raises exception TA_file_syntax_error if there is a parse error
    in the TA file. *)




  
