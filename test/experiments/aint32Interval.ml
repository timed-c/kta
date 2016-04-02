
open Ustring.Op
open Printf
type high = int
type low = int

type aint32 = high * low

  

let aint32_add (l1,h1) (l2,h2) =
    (l1+l2,h1+h2)

let aint32_const v =
    (v,v)

let aint32_interval l h =
    (l,h)

let aint32_any =
   (-2147483648,2147483647)

let aint32_join (l1,h1) (l2,h2) =
  (min l1 l2, max h1 h2)

(* Check for equality. Returns two lists,
   one when they are equal, one when they not *)    
let aint32_test_equal (l1,h1) (l2,h2) =
  
  if h1 < l2 || h2 < l1 then
    (* Not overlapping *)
    ([],                                             (* TRUE *)
     [((l1,h1),(l2,h2))])                            (* FALSE *)
  else if h1 = l2 then
    (
    if l1 < h1 && l2 < h2 then
      ([((h1,h1),(h1,h1))],                          (* TRUE *)
       [((l1,h1),(l2+1,h2));((l1,h1-1),(l2,h2))])    (* FALSE *)
    else if l1 < h1 && l2 = h2 then
      ([((h1,h1),(h1,h1))],                          (* TRUE *)
       [((l1,h1-1),(l2,h2))])                        (* FALSE *)
    else if l1 = h1 && l2 < h2 then
      ([((h1,h1),(h1,h1))],                          (* TRUE *)
       [((l1,h1),(l2+1,h2))])                        (* FALSE *)
    else   
      ([((h1,h1),(h1,h1))],                          (* TRUE *)
       [])                                           (* FALSE *)
    )
  else
     failwith "TODO!"
      
    
  
    
let aint32_pprint v =
  let (l,h) = v in
  if v = aint32_any then us"ANY"
  else if l = h then
    us (sprintf "%d" l)
  else
    us (sprintf "[%d,%d]" l h)
          
