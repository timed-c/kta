
open Ustring.Op
open Printf
type high = int
type low = int

type safepair = int
let nextpair = ref 1
let nopair = 0


type aint32 =
| BaseAddr
| Interval of (low * high)
| IntervalList of (low * high) list * safepair

    

let aint32_any = Interval(-2147483648,2147483647)  


     
let aint32_pprint v =
  let prn (l,h) = 
    if l = h then us (sprintf "%d" l)
    else us (sprintf "[%d,%d]" l h)
  in  
  match v with
  | BaseAddr -> us"BaseAddress"
  | Interval(l,h) -> prn (l,h)
  | IntervalList(lst,sp) ->
      us(if sp == nopair then "" else "*") ^.
      us"[" ^. Ustring.concat (us", ") (List.map prn lst) ^. us"]" 

        

let aint32_print v =
  uprint_string (aint32_pprint v)

    
let aint32_binop op v1 v2 =
  match v1,v2 with
  | BaseAddr, _ | _, BaseAddr -> failwith "Cannot perform operations on the base address"
  | Interval (l1,h1), Interval (l2,h2) ->  Interval (op (l1,h1) (l2,h2))
  | Interval(l1,h1),IntervalList(lst,sp) | IntervalList(lst,sp), Interval(l1,h1) 
    -> IntervalList(List.map (fun v2 -> op (l1,h1) v2) lst, sp)
  | IntervalList(l1,sp1), IntervalList(l2,sp2)
    when sp1 = sp2
    -> IntervalList(List.map2 (fun (l1,h1) (l2,h2) -> op (l1,h1) (l2,h2)) l1 l2, sp1)
  | IntervalList(l1,_), IntervalList(l2,_)
    -> IntervalList((List.fold_left (fun a1 v1 ->
         List.fold_left (fun a2 v2 ->
           (op v1 v2)::a2
         ) a1 l2
    ) [] l1), nopair)
   (* TODO: Limit this expansion *)
    
          
let aint32_add v1 v2 =
    aint32_binop (fun (l1,h1) (l2,h2) -> (l1+l2,h1+h2)) v1 v2
    
    
let aint32_const v =
    Interval(v,v)

let aint32_interval l h =
    Interval(l,h)


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

       

    
