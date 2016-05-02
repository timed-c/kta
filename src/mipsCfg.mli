open Ustring.Op


(** [make_cfg addr p] takes a code memory address [addr] and a program
    [p] as input and generates a [cfg] for a function. *)
val make_cfg : int -> MipsAst.program -> (MipsAst.program * MipsAst.cfg * string list)

  
(** [make_cfgmap name p] takes a function [name] and a program [p] as
    input and generates a [cfgmap] containing a mapping from
    identifiers to CFGs. *)
val make_cfgmap : string -> MipsAst.program -> (MipsAst.program * MipsAst.cfgmap)
  
  
(** Generates Ocaml analyze.ml file from the a funcmap. *)  
val pprint_ocaml_cps_from_cfgmap :
  bool -> string -> MipsAst.cfgmap -> MipsAst.program -> ustring

  

val test : MipsAst.program -> string -> unit
