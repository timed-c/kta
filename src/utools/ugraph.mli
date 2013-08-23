

exception Not_a_DAG


type graph = (int list) array
(** A graph is represented as an adjacency list. Each vertex in the
    graph is given a unique integer value *)

val topological_sort : graph -> int list 
(** [topological_sort G] takes a directed graph [G] and returns a topologically 
    sorted list of nodes. The function is implemented using depth-first search 
    according to Cormen et al. (2001) *)

val dominator : graph -> int -> int array
(** [dominator G v] computes the dominator tree for the directed graph [G]
    with start node [v]. The returned dominator tree is stored in an integer
    array such that dom[v] = parent of note v. This function assumes that
    all vertices in the graph are reachable from [v]. *)

val strongly_connected_components : graph -> (int list) list
(** [strongly_connected_components G] returns a list of the strongly connected
    components of graph [G]. Each element in the list is a list of integers 
    representing the vertices in each component. The order of the returned 
    strongly connected components is also topologically sorted. *)

val reverse : graph -> graph
(** Returned the reverse of a directed graph, that is, each edge
    (v,w) in E is replaced with (w,v). *)

val make_undirected : graph -> graph
(** Takes a directed graph and add edges so that the graph becomes 
    undirected. If the input graph has no redundant edges, the resulting
    graph will not have any redundant edges. *)
