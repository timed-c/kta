
open Ustring.Op
open TaFileTypes
  
  

val pprint_simple_ta_res : ta_res list -> ustring
(** [pprint_simple_ta_res reslst] takes a list of timing analysis
    responses [reslst] and pretty prints it using a comma separation. *)

  

val pprint_full_ta_res : func_ta_req -> ta_res list -> ustring
(** [pprint_full_ta_res tareq reslst] takes a function timing request
    [tareq] and a list of timing analysis responses [reslst]. Pretty
    print full output for timing analysis response (including all text
    of the request. NOTE: This function is not yet implemented! *)

  

val parse_ta_file : string -> file_ta_req
(** [parse_ta_file filename] takes a timing analysis file [filename]
    as input, parses it, and returns timing analysis request
    structure.  Raises exception Sys_error if the file cannot be
    found. Raises exception TA_file_syntax_error if there is a parse error
    in the TA file. *)




  
