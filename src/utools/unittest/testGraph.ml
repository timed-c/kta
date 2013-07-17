






open Ustring.Op
open Utest
open Ugraph


let main = 

  init "Test various graph functions";


  (** ------------------- Topological Sort ------------------- *)

  (* Topological test 1 *)  
  let graph = [| [1;2];[3];[3];[4];[] |] in
  test_intlist "Simple topological sort of digraph."
                (Ugraph.topological_sort graph) [0;2;1;3;4];

  (* Topological test 2 *)  
  let graph = [| [1;2];[0;3];[3];[4;0];[1] |] in
  let res = (try (let _ = Ugraph.topological_sort graph in false)
             with _ -> true) in
  test "Simple topological sort with cycles." res;

  (* Topological test 3 *)  
  let graph = [| [1];[3;2];[];[6];[2;5];[2];[] |] in
  test_intlist "Another topological sort"
                (Ugraph.topological_sort graph) [4;5;0;1;2;3;6];


    
  result()
