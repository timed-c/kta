


exception Not_a_DAG

type graph = (int list) array



(** ---------------------- Topological sort ------------------------ *)
  
type topmark = TempMark | PermMark | NoMark

let rec top_visit (graph:graph) marked acc v =
  match marked.(v) with
  | TempMark -> raise Not_a_DAG
  | PermMark -> acc
  | NoMark -> 
      marked.(v) <- TempMark;
      let acc2 = List.fold_left (top_visit graph marked) acc (graph.(v)) in
      marked.(v) <- PermMark;
      v::acc2      

let topological_sort graph = 
  let nodes = Array.length graph in
  let marked = Array.make nodes NoMark in
  Utils.fold_interval (top_visit graph marked) [] 0 nodes
