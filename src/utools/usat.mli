

open Ustring.Op


type cnf = (int list) array
(** Conjunction normal form *)

exception CNF_parse_error of int 
exception CNF_vars_not_match of int * int
exception CNF_clauses_not_match of int * int

val variables : cnf -> int
(** Returns the number of variables in the CNF.
    Note that this function iterates through all data. *)

val clauses : cnf -> int
(** Returns the number of clauses *)


val read_cnf : string -> cnf
(** Read a CNF file *)

val pprint_cnf : cnf -> ustring
(** Pretty print cnf in CNF file format *)
