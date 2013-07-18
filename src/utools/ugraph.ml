


exception Not_a_DAG

type graph = (int list) array



(** ---------------------- Topological sort ------------------------ *)
  
type topmark = TempMark | PermMark | NoMark

let topological_sort graph = 
  let nodes = Array.length graph in
  let marked = Array.make nodes NoMark in
  let rec visit acc v =
    match marked.(v) with
    | TempMark -> raise Not_a_DAG
    | PermMark -> acc
    | NoMark -> 
        marked.(v) <- TempMark;
        let acc2 = List.fold_left visit acc graph.(v) in
        marked.(v) <- PermMark;
        v::acc2      
  in
    Utils.fold_interval visit [] 0 (nodes-1)


(** ---------------------- Dominator -------------------------------- *)

   
let dominator out_graph v = [||]
(*
  let nodes = Array.length graph in
  let visited = Array.make nodes false in
  let domtree = Array.make nodes (-1) in
  let in_graph = Array.make nodes [] in
  for v = 0 to nodes-1 do
    List.iter (fun w -> in_graph.(w) <- (v::(in_graph.(w)))) (out_graph.(v))      
  done;
  let rec visit v =
    if not visited.(v) && List.for_all (fun w -> visited.(w)) (in_graph.(v)) then      
      visited.(v) <- true;
      domtree.(v) <- find_idom (in_graph.(v));
      List.iter visit (out_graph.(v));
  in
    visit v ;
    domtree
*)
    

(** ---------------------- Strong -------------------------------- *)

let strongly_connected_components graph = []
(*  let nodes = Array.length graph in
  
  let rec strong_connect i
*)


















