


open Ustring.Op


type argtype =
| No      (* No arguments *)
| Str     (* Argument is a normal string *)
| Int     (* The argument is an integer that can be both postive and negative *)
| StrList (* The argument can be a list of strings. The list can be empty. *)

    
val parse : string list -> ('a * argtype * ustring * ustring * ustring) list 
  -> (((('a * ustring list) list * ustring list) option) * ustring)
(** [parse argv options] parses the argument options [argv] using the
    information described in the [options] parameter. See example code
    for more information *)

val optionstext : ('a * argtype * ustring * ustring * ustring) list -> ustring
(** [optionstext options] takes the list of [options] and create a 
    readable text that can be used as part of a help text *)
        
   


  
