
open Ustring.Op
open Printf

type lower = int
type upper = int
type aint32 = (lower * upper) list


let int32max = 0x7fffffff
  
let int32min = (0x7fffffff + 1) * -1

let abstractval = [(int32min,int32max)]


(* ---------------------------------------------------------------------*)
let make v = ([v,v])

  
(* ---------------------------------------------------------------------*)
let make_intervals ival = ival


(*---------------------------------------------------------------------*)
let pprint lst =
  let elems = List.map (fun (l,u) -> us(if l=u then sprintf "%d" l else sprintf "%d..%d" l u)) lst
  in Ustring.concat (us", ") elems 

  

(* ---------------------------------------------------------------------*)
let add xlst ylst =
   List.fold_left
    (fun xacc (xl,xu) ->
      List.fold_left
        (fun yacc (yl,yu) ->
          let lower = xl + yl in
          let upper = xu + yu in
          let v =
            if lower < int32min || upper > int32max then (int32min,int32max)
            else (lower,upper)
          in v::yacc) xacc ylst) [] xlst

     
