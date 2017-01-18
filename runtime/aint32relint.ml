
open Ustring.Op
open Printf
type high = int
type low = int

exception Exception_aint32
  
type safepair = int
let pairSym = ref 0  
let nopair = 0
let getPairSym() =
  pairSym := !pairSym + 1;
  !pairSym

type interval = (low * high)
type aint32 =
| Any 
| Interval of interval
| IntervalList of interval list * safepair

let baseaddr_fail() = failwith "Error: cannot perform operations on base addresses"     
let fail_aint32() = failwith "Error: aint32 error that should not happen."

let lowval = -2147483648
let highval = 2147483647
let aint32_any = Any
exception AnyException
  
let interval_merge (l1,h1) (l2,h2) =
  (min l1 l2, max h1 h2)

let interval_merge_list lst =
  match lst with
  | [] -> fail_aint32()
  | l::ls -> List.fold_left interval_merge l ls 

    
let aint32_to_int32 v =
  match v with
  | Any | IntervalList(_,_) -> raise Exception_aint32
  | Interval(l,h) when l <> h -> raise Exception_aint32
  | Interval(l,_) -> l

    
let aint32_pprint debug v =
  let prn (l,h) =
    if l = h then us (sprintf "%d" l)
    else us (sprintf "[%d,%d]" l h)
  in  
  match v with
  | Any -> us"Any"
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
  try 
    match v1,v2 with
    | Any,_|_,Any -> Any
    | Interval(vv1), Interval(vv2) ->
      Interval (op vv1 vv2)
    | Interval(vv1),IntervalList(lst,sp) | IntervalList(lst,sp), Interval(vv1) ->
      IntervalList(List.map (fun vv2 -> op vv1 vv2) lst, sp)
    | IntervalList(l1,sp1), IntervalList(l2,sp2) when sp1 = sp2 ->
      IntervalList(List.rev (List.rev_map2 (fun v1 v2 -> op v1 v2) l1 l2), sp1)     
    | IntervalList(l1,_), IntervalList(l2,_) ->
      Interval (op (interval_merge_list l1) (interval_merge_list l2))
  with AnyException -> Any
          
let aint32_add v1 v2 =
  aint32_binop (fun (l1,h1) (l2,h2) ->
    let l = l1+l2 in
    let h = h1+h2 in
    if l<lowval || h>highval then raise AnyException
    else (l,h)
  ) v1 v2

let aint32_and_f (l1,h1) (l2,h2) =
  let l = l1 land l2 in
  let h = max h1 h2 in
  if l<lowval || h>highval then raise AnyException
  else (l,h)
           
(*TODO step*)
let aint32_and v1 v2 =
  aint32_binop  aint32_and_f v1 v2

let aint32_or_f (l1,h1) (l2,h2) =
  let l = min l1 l2 in
  let h = h1 lor h2 in
  if l<lowval || h>highval then raise AnyException
  else (l,h)
           
(*TODO all*)
let aint32_or v1 v2 =
  aint32_binop aint32_or_f v1 v2

let aint32_not_f (l,h) =
  (lnot h, lnot l)
    
let aint32_nor_f t1 t2 =
  aint32_and_f (aint32_not_f t1) (aint32_not_f t2)

let aint32_nor v1 v2 =
  aint32_binop (aint32_nor_f) v1 v2

let aint32_xor_f t1 t2 =
  aint32_or_f
    (aint32_and_f (aint32_not_f t1) t2)
    (aint32_and_f t1 (aint32_not_f t2))
              
let aint32_xor v1 v2 =
  aint32_binop aint32_xor_f v1 v2

let aint32_mul v1 v2 =
  aint32_binop (fun (l1,h1) (l2,h2) ->
    let l = min (min (l1*l2) (l1*h2)) (min (h1*l2) (h1*h2)) in
    let h = max (max (l1*l2) (l1*h2)) (max (h1*l2) (h1*h2)) in
    if l<lowval || h>highval then raise AnyException
    else (l,h)
  ) v1 v2
    
let aint32_div v1 v2 =
  aint32_binop (fun (l1,h1) (l2,h2) ->
    if (l2<=0 && h2 >=0) then raise Division_by_zero
    else
      let l = min (min (l1/l2) (l1/h2)) (min (h1/l2) (h1/h2)) in
      let h = max (max (l1/l2) (l1/h2)) (max (h1/l2) (h1/h2)) in
      if l<lowval || h>highval then raise AnyException
      else (l,h)
  ) v1 v2

    
let aint32_const v =
    Interval(v,v)

let aint32_interval l h =
    Interval(l,h)


let aint32_join v1 v2 =
  match v1, v2 with
  | Any,_|_,Any -> Any
  | Interval(v1),Interval(v2) ->
    Interval(interval_merge v1 v2)
  | Interval(v1),IntervalList(lst,_) | IntervalList(lst,_),Interval(v1) ->
    Interval(interval_merge_list (v1::lst))
  | IntervalList(l1,_),IntervalList(l2,_) ->
    printf "Merge B:  %d  %d\n" (List.length l1) (List.length l2);
    Interval(interval_merge (interval_merge_list l1) (interval_merge_list l2)) 

             
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
  


(*  We can divide into 6 different cases when checking
      if x < y

      #1  CASE          TRUE          FALSE
          xxxxx         xxxxx         .....
                yyy            yyy          ...  


      #2  CASE          TRUE          FALSE
          xxxxx         xxxxx         ..xxx
            yyyyy         yyyyy         yyy..  


      #3  CASE          TRUE          FALSE
          xxxxxxx       xxxx...       ..xxxxx
            yyy           yyy           yyy     


      #4  CASE          TRUE          FALSE
            xxx           xxx           xxx 
          yyyyyyyy      ...yyyyy      yyyyy...  

        
      #5  CASE          TRUE          FALSE
            xxxxx         xx...         xxxxx 
          yyyyy         ...yy         yyyyy     
        

      #6  CASE          TRUE          FALSE
              xxx           ...           xxx 
          yyy           ...           yyy      

    
      This can be generalized to the following two cases:

     TRUE CASE:  if l1 < h2  then                 (cases #1-#5)
                     for X: [l1,min(h1,h2-1)]
                     for Y: [max(l1+1,l2),h2]
                 else None           

     FALSE CASE: if h1 >= l2 then                 (cases #2-#6)
                     for X: [max(l1,l2),h1]
                     for Y: [l2,min(h2,h1)]
                 else None
  *)        
let rec aint32_test_less_than v1 v2 =
  match v1,v2 with
  | Any,_|_,Any -> (Some(Any,Any),Some(Any,Any))
  | Interval((l1,h1)), Interval((l2,h2)) ->
    ((if l1 < h2 then
         Some(Interval(l1,min h1 (h2-1)), Interval(max (l1+1) l2,h2))
      else None),
     (if h1 >= l2 then
         Some(Interval(max l1 l2,h1), Interval(l2,min h2 h1))        
      else None))
  | Interval(v1),IntervalList(l1,_) | IntervalList(l1,_), Interval(v1) ->
    aint32_test_less_than (Interval(v1)) (Interval(interval_merge_list l1))
  | IntervalList(l1,_), IntervalList(l2,_) ->
    aint32_test_less_than (Interval(interval_merge_list l1))
                          (Interval(interval_merge_list l2))

(* Same as the above, but conservative for negative numbers *)      
let rec aint32_test_less_than_unsigned v1 v2 =
  match v1,v2 with
  | Any,_|_,Any -> (Some(Any,Any),Some(Any,Any))
  | Interval((l1,h1)), Interval((l2,h2)) ->
    if l1 < 0 || l2 < 0 then (Some(Any,Any),Some(Any,Any))
    else
      ((if l1 < h2 then
          Some(Interval(l1,min h1 (h2-1)), Interval(max (l1+1) l2,h2))
        else None),
       (if h1 >= l2 then
           Some(Interval(max l1 l2,h1), Interval(l2,min h2 h1))        
        else None))
  | Interval(v1),IntervalList(l1,_) | IntervalList(l1,_), Interval(v1) ->
    aint32_test_less_than_unsigned (Interval(v1)) (Interval(interval_merge_list l1))
  | IntervalList(l1,_), IntervalList(l2,_) ->
    aint32_test_less_than_unsigned (Interval(interval_merge_list l1))
                          (Interval(interval_merge_list l2))
  
      
(*  We can divide into 6 different cases when checking
      if x < y

      #1  CASE          TRUE          FALSE
          xxxxx         xxxxx         .....
                yyy            yyy          ...  


      #2  CASE          TRUE          FALSE
          xxxxx         xxxxx         ..xxx
            yyyyy         yyyyy         yyy..  


      #3  CASE          TRUE          FALSE
          xxxxxxx       xxxxx..       ...xxxx
            yyy           yyy           yyy     


      #4  CASE          TRUE          FALSE
            xxx           xxx           xxx 
          yyyyyyyy      ...yyyyy      yyyy....  

        
      #5  CASE          TRUE          FALSE
            xxxxx         xxx..         xxxxx 
          yyyyy         ...yy         yyyyy     
        

      #6  CASE          TRUE          FALSE
              xxx           ...           xxx 
          yyy           ...           yyy      

    
      This can be generalized to the following two cases:

     TRUE CASE:  if l1 <= h2  then                (cases #1-#5)
                     for X: [l1,min(h1,h2)]
                     for Y: [max(l1,l2),h2]
                 else None           

     FALSE CASE: if h1 > l2 then                 (cases #2-#6)
                     for X: [max(l1,l2+1),h1]
                     for Y: [l2,min(h2,h1-1)]
                 else None
  *)        
let rec aint32_test_less_than_equal v1 v2 =
  match v1,v2 with
  | Any,_|_,Any -> (Some(Any,Any),Some(Any,Any))
  | Interval((l1,h1)), Interval((l2,h2)) ->
    ((if l1 <= h2 then
         Some(Interval(l1,min h1 (h2)), Interval(max (l1) l2,h2))
      else None),
     (if h1 > l2 then
         Some(Interval(max l1 (l2+1),h1), Interval(l2,min h2 (h1-1)))        
      else None))
  | Interval(v1),IntervalList(l1,_) | IntervalList(l1,_), Interval(v1) ->
    aint32_test_less_than_equal (Interval(v1)) (Interval(interval_merge_list l1))
  | IntervalList(l1,_), IntervalList(l2,_) ->
    aint32_test_less_than_equal (Interval(interval_merge_list l1))
                          (Interval(interval_merge_list l2))
                          
    
      
(* Test if two abstract integers are equal. Returns a tuple where the
   elements are options for if there is a "true" branch or a "false" 
   branch *)
let rec aint32_test_equal v1 v2 =
  match v1,v2 with
  | Any,_|_,Any -> (Some(Any,Any),Some(Any,Any))
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
       let (t,f) = (List.sort_uniq compare t, List.sort_uniq compare f) in
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

     















           
