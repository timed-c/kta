

(** A simple unit testing framework - Utest *)

open Ustring.Op

val init : string -> unit
(** Initiate a test module, giving it a name *)

val test : string -> bool -> unit
(** Expression [test s b] runs a test with name [s].
    The test is treated as successful if [b] is
    true, else it is seen as unsuccessful. *)

val test_ext : string -> bool -> ustring -> ustring -> unit

val test_str : string -> string -> string -> unit

val test_ustr : string -> ustring -> ustring -> unit

val test_ustr : string -> ustring -> ustring -> unit

val test_list : string -> 'a list -> 'a list -> 
                ('a -> ustring) -> unit

val test_array : string -> 'a array -> 'a array -> 
                ('a -> ustring) -> unit

val result : unit -> unit
(** Print out the unit test results on the screen *)

val summary : unit -> unit
(** Print out the summary of all tests *)
