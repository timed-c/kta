


type graph = (int list) array
(** A graph is represented as an adjacency list. Each vertex in the
    graph is given a unique integer value *)


val topological_sort : graph -> int list 
(** [topological_sort G] takes a directed graph [G] and returns a topologically 
    sorted list of nodes. The function is implemented using depth-first search 
    according to Cormen et al. (2001) *)

