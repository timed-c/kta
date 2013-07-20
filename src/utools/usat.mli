

open Ustring.Op



type literal = Pos of int | Neg of int 
type varstat = VSNotAvailable | VSOnlyNeg | VSOnlyPos | VSPosAndNeg

type cnf = (literal list) list
(** Conjunction normal form *)

exception CNF_parse_error of int 
exception CNF_vars_not_match of int * int
exception CNF_clauses_not_match of int * int

val varnum : literal -> int
(** Returns the variable number of a literal *)

val variables : cnf -> int
(** Returns the number of variables in the CNF.
    Note that this function iterates through all data. *)

val clauses : cnf -> int
(** Returns the number of clauses *)


val read_cnf : string -> cnf
(** Read a CNF file *)

val pprint_cnf : cnf -> ustring
(** Pretty print cnf in CNF file format *)

val var_statistics : cnf -> (varstat array * int * int * int * int)
(** Expression [var_statistics s] returns a tuple 
[(vs_array,not_available,only_neg,only_pos,pos_and_neg)]. Array [vs_array]
contains information about the appearance of each variable. *)
