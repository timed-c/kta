open Ustring.Op


(** [make_cfg addr p] takes a code memory address [addr] and a program
    [p] as input and generates a [cfg] for a function. *)
val make_cfg : int -> MipsAst.program -> (MipsAst.program * MipsAst.cfg * string list)

  
(** [make_cfgmap p] takes a program [p] as input and generates a [cfgmap] containing 
    a mapping from identifiers to CFGs. *)
val make_cfgmap : MipsAst.program -> MipsAst.cfgmap

  
(** Generates Ocaml functions from the a CFG (one function). If the first 
    parameter is true, a better output is generated. *)  
val pprint_ocaml_cps_from_cfg : bool -> MipsAst.cfg -> MipsAst.program -> ustring

  
(** Generates Ocaml functions from the a funcmap. *)  
val pprint_ocaml_cps_from_funcmap : MipsAst.cfgmap -> ustring

  

val test : MipsAst.program -> string -> unit
