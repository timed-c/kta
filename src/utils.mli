
(** Different utility functions that are missing in the standard library *)

val last : 'a list -> 'a
(** Returns the last element in a list. Raises [Invalid_argument "Utils.last"]
    if the list is empty. *)

val findindex : 'a -> 'a list -> int
(** Function [findindex x l] returns the index for the first occurance of [x] in list [l]. Raises [Not_found] if [x] does not exist in [l]. *)

val ( |> ) : 'a -> ('a -> 'b) -> 'b
(** Pipe-forward operator *)

val ( <| ) :  ('a -> 'b) -> 'a -> 'b
(** Pipe-backward operator *)

val ( >> ) : ('a -> 'b) -> ('b -> 'c) -> 'a -> 'c
(** Forward composition operator *)

val map_option : ('a -> 'b) -> 'a option -> 'b option

val string_of_intlist : int list -> string
(** Converts a list of integers to a string, where the least 8 significant bits
    are used of each integer. *)

val intlist_of_string : string -> int list
(** Converts a string into a list of integers *)

val write_bin_file : string -> string -> unit
(** Call [write_bin_file n d] creates a binary file named [n] and stores 
    string data [d] in the file. Raises [Sys_error] if error creating or
    writing to file. *)

    
