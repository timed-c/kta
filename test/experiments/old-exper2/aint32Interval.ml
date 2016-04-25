
open Ustring.Op
open Printf
type high = int
type low = int

type aint32 = high * low

let aint32_any =
   (-2147483648,2147483647)  


     
let aint32_pprint v =
  let (l,h) = v in
  if v = aint32_any then us"ANY"
  else if l = h then
    us (sprintf "%d" l)
  else
    us (sprintf "[%d,%d]" l h)


let aint32_print v =
  uprint_string (aint32_pprint v)

let aint32_add (l1,h1) (l2,h2) =
    (*    printf "add before: [%d,%d] [%d,%d]   after [%d,%d]\n" l1 h1 l2 h2  (l1+l2) (h1+h2); *)
    (l1+l2,h1+h2)
    

let aint32_const v =
    (v,v)

let aint32_interval l h =
    (l,h)


let aint32_join (l1,h1) (l2,h2) =
  (min l1 l2, max h1 h2)

  
    
let aint32_compare x y =
(*  printf "***Compare ";
  aint32_print x;
  printf "   ";
  aint32_print y;
    printf "\n"; 
*)
  compare x y
    
(* Check for equality. Returns two lists,
   one for cases when they are equal, one when they not *)    
let aint32_test_equal (l1,h1) (l2,h2) =
  (*    printf "test equal: [%d,%d] [%d,%d] \n" l1 h1 l2 h2; *)
  if h1 < l2 || h2 < l1 then
    (
     (* Not overlapping: 
         111       |   22222
             2222  |        1111
      *)
    ([],                                             (* TRUE *)
     [((l1,h1),(l2,h2))])                            (* FALSE *))
  else if h1 = l2 then
    ( (* Overlapping with one:
            11111
                22222 
      *)
      if l1 < h1 && l2 < h2 then
      (* Two cases for false *)
      ([((h1,h1),(h1,h1))],                          (* TRUE *)
       [((l1,h1),(l2+1,h2));((l1,h1-1),(l2,h2))])    (* FALSE *)
    else if l1 < h1 && l2 = h2 then
      (*  11111
              2  *)  
      ([((h1,h1),(h1,h1))],                          (* TRUE *)
       [((l1,h1-1),(l2,h2))])                        (* FALSE *)
    else if l1 = h1 && l2 < h2 then
      (*      1
              22222 *)
      ([((h1,h1),(h1,h1))],                          (* TRUE *)
       [((l1,h1),(l2+1,h2))])                        (* FALSE *)
    else
      (*      1
              2  *)
      ([((h1,h1),(h1,h1))],                          (* TRUE *)
       [])                                           (* FALSE *)
    )
  else if l1 = h2 then
    ( (* Overlapping with one:
              11111
          22222 
      *)
      if l1 < h1 && l2 < h2 then
      (* Two cases for false *)
      ([((h2,h2),(h2,h2))],                          (* TRUE *)
       [((l1+1,h1),(l2,h2));((l1,h1),(l2,h2-1))])    (* FALSE *)
    else if l2 < h2 && l1 = h1 then
      (*  22222
              1  *)  
      ([((h2,h2),(h2,h2))],                          (* TRUE *)
       [((l1,h1),(l2,h2-1))])                        (* FALSE *)
    else if l2 = h2 && l1 < h1 then
      (*      2
              11111 *)
      ([((h2,h2),(h2,h2))],                          (* TRUE *)
       [((l1+1,h1),(l2,h2))])                        (* FALSE *)
    else
      (*      1
              2  *)
      ([((h2,h2),(h2,h2))],                          (* TRUE *)
       [])                                           (* FALSE *)
    )
  else
      (* More overlapping than one. Conservative, both 
         cases. TODO: Make the true-branch less 
         concervative. *)
      ([((l1,h1),(l2,h2))],                          (* TRUE *)
       [((l1,h1),(l2,h2))])                          (* FALSE *)

       

    
