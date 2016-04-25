
open Ustring.Op
open Printf
type high = int
type low = int

type safepair = int
let pairSym = ref 0  
let nopair = 0
let getPairSym() =
  pairSym := !pairSym + 1;
  !pairSym

type interval = (low * high)
type aint32 =
| BaseAddr
| Interval of interval
| IntervalList of interval list * safepair

let baseaddr_fail() = failwith "Error: cannot perform operations on base addresses"     
let fail_aint32() = failwith "Error: aint32 error that should not happen."

let anyvals = (-2147483648,2147483647)  

let aint32_any = Interval anyvals

let interval_merge (l1,h1) (l2,h2) =
  (min l1 l2, max h1 h2)

let interval_merge_list lst =
  match lst with
  | [] -> fail_aint32()
  | l::ls -> List.fold_left interval_merge l ls 
        

     
let aint32_pprint debug v =
  let prn (l,h) =
    if (l,h) = anyvals then us"Any" 
    else if l = h then us (sprintf "%d" l)
    else us (sprintf "[%d,%d]" l h)
  in  
  match v with
  | BaseAddr -> us"BaseAddress"
  | Interval(l,h) -> prn (l,h)
  | IntervalList(lst,sp) ->
    us(if debug && sp != nopair then sprintf "{%d}" sp else "") ^.
    (if debug then
      us"[" ^. Ustring.concat (us", ") (List.map prn lst) ^. us"]"
    else 
      prn (interval_merge_list lst))
        

let aint32_print_debug v =
  uprint_endline (aint32_pprint true v)


  
let aint32_binop op v1 v2 =
  match v1,v2 with
  | BaseAddr, _ | _, BaseAddr -> baseaddr_fail()
  | Interval(vv1), Interval(vv2) ->
    Interval (op vv1 vv2)
  | Interval(vv1),IntervalList(lst,sp) | IntervalList(lst,sp), Interval(vv1) ->
    IntervalList(List.map (fun vv2 -> op vv1 vv2) lst, sp)
  | IntervalList(l1,sp1), IntervalList(l2,sp2) when sp1 = sp2 ->
    IntervalList(List.rev (List.rev_map2 (fun v1 v2 -> op v1 v2) l1 l2), sp1)     
  | IntervalList(l1,_), IntervalList(l2,_) ->
    Interval (op (interval_merge_list l1) (interval_merge_list l2))
    
          
let aint32_add v1 v2 =
    aint32_binop (fun (l1,h1) (l2,h2) -> (l1+l2,h1+h2)) v1 v2
    
    
let aint32_const v =
    Interval(v,v)

let aint32_interval l h =
    Interval(l,h)


let aint32_join v1 v2 =
  match v1, v2 with
  | BaseAddr,BaseAddr -> BaseAddr
  | BaseAddr,_ | _,BaseAddr -> baseaddr_fail()
  | Interval(v1),Interval(v2) ->  IntervalList([v1;v2],nopair)
  | Interval(v1),IntervalList(lst,_) | IntervalList(lst,_),Interval(v1) ->
    IntervalList(v1::lst,nopair)
  | IntervalList(l1,_),IntervalList(l2,_) ->
    IntervalList(l1@l2,nopair)
   (* TODO: Limit this expansion *)
             
let aint32_compare x y =
  compare x y
    
(* Check for equality. Returns two lists,
   one for cases when they are equal, one when they not *)    
let test_equal (l1,h1) (l2,h2) =
  if l1=l2 && h1=h2 && h1-l1 = 1 then
  (* Binary values that are equal *)
    ([((l1,l1),(l1,l1)); ((h1,h1),(h1,h1))],
     [((l1,l1),(h1,h1)); ((h1,h1),(l1,l1))])     
  else if h1 < l2 || h2 < l1 then
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


(* Same as split in List.split, with the difference that it is 
   tail recursive and that it returns the result in reverse order *)  
let split_rev lst =
  let rec work lst acc1 acc2 =
    match lst with
    | (a,b)::ls -> work ls (a::acc1) (b::acc2)
    | [] -> (acc1,acc2)
  in
    work lst [] []
  


(* Test if two abstract integers are equal. Returns a tuple where the
   elements are options for if there is a "true" branch or a "false" 
   branch *)
let rec aint32_test_equal v1 v2 =
  match v1,v2 with
  | BaseAddr,BaseAddr -> (Some (v1,v2), None)
  | BaseAddr,_ | _,BaseAddr -> baseaddr_fail()
    
  (* Case when we just compare two intervals. May generate safe pair lists *)
  | Interval(v1),Interval(v2) ->
    let mkval vlst =
        match vlst with
        | [] -> None
        | [(v1,v2)] -> Some(Interval(v1),Interval(v2))
        | v ->
          let (v1,v2) = split_rev v in
          let s = getPairSym() in
          Some(IntervalList(v1,s),IntervalList(v2,s))
    in
    (* Make the actual equality tests *)
    let (t,f) = test_equal v1 v2 in
    (mkval t, mkval f)

      
      
  (* Case when we have two paired interval lists *)
  | IntervalList(l1,sp1), IntervalList(l2,sp2)
    when sp1=sp2       
    -> (* Tail-recursive test of interval lists *)
       let rec newlists list1 list2 acc1 acc2 =
         match list1,list2 with
         | v1::ls1,v2::ls2 ->
             let (t,f) = test_equal v1 v2 in
             newlists ls1 ls2 (t@acc1) (f@acc2)
         | [],[] -> (acc1,acc2)
         | _,_ -> fail_aint32()
       in let (t,f) = newlists l1 l2 [] [] in
       (* Remove duplicates *)
       printf "Before: t %d  f %d\n" (List.length t) (List.length f);
       let (t,f) = (List.sort_uniq compare t, List.sort_uniq compare f) in
       printf "After:  t %d  f %d\n" (List.length t) (List.length f);
       (* Split into the two variable alternatives *)
       let (t1,t2) = split_rev t in
       let (f1,f2) = split_rev f in
       (* Return results *)
       ((if t1=[] then None else
           let s = getPairSym() in
           Some(IntervalList(t1,s),IntervalList(t2,s))),
        (if f1=[] then None else
           let s = getPairSym() in
           Some(IntervalList(f1,s),IntervalList(f2,s))))

  (* Cases where there a no safe pairs. We then collaps the structure 
     and perform interval test for equality *)
  | Interval(v1),IntervalList(l1,_) | IntervalList(l1,_), Interval(v1) ->
    aint32_test_equal (Interval(v1)) (Interval(interval_merge_list l1))
  | IntervalList(l1,_), IntervalList(l2,_) ->
    aint32_test_equal (Interval(interval_merge_list l1))
                         (Interval(interval_merge_list l2))

     















           
