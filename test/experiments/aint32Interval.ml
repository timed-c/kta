
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


let aint32_pprint v =
  if v = aint32_any then us"ANY"
  else
    let (l,h) = v in
    us (sprintf "[%d,%d]" l h)
          
