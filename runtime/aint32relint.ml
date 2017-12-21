
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

type initialized = bool
  
type interval = (low * high)
type aint32 =
  | Any of initialized  
  | Interval of interval * initialized
  | IntervalList of interval list * safepair * initialized

let baseaddr_fail() = failwith "Error: cannot perform operations on base addresses"     
let fail_aint32() = failwith "Error: aint32 error that should not happen."

let lowval = -2147483648
let highval = 2147483647

let lowval8 = -127
let highval8 = 128
let ulowval8 = 0
let uhighval8 = 255

let lowval16 = -32768
let highval16 = 32767
let ulowval16 = 0
let uhighval16 = 65535

let aint32_any = Any true

let aint32_any_set v = Any v

let get_initialized =
  function | Any i | Interval (_,i) | IntervalList(_,_,i) -> i

let set_initialized v i =
  match v with
  | Any _ -> Any i
  | Interval (v,_) -> Interval (v,i)
  | IntervalList(lv,sp,_) -> IntervalList(lv,sp,i)

exception AnyException


(***********************SPLITER**************************)
  
type spliter = | Abstract
               | AbstractL
               | AbstractR
               | ConcreteMR
               | ConcreteML
               | ConcreteL
               | ConcreteR
                   
let rec print_spliter spllist = 
  let print_spliter_internal =
    function
    | Abstract -> printf "Abstract\n%!";
    | AbstractL -> printf "AbstractL\n%!";
    | AbstractR -> printf "AbstractR\n%!";
    | ConcreteR -> printf "ConcreteR\n%!";
    | ConcreteMR -> printf "ConcreteMR\n%!";
    | ConcreteML -> printf "ConcreteML\n%!";
    | ConcreteL -> printf "ConcreteL\n%!";
  in
  match spllist with
  | [] -> ();
  | spl::spls -> print_spliter_internal spl;
    print_spliter spls
(***********************END SPLITER**************************)
      
(**********************BEGIN SPLITER**************************)


let check_aint16 v =
  let check_aint16_int v =
    let (l,h) = v in
    if (l<lowval16) || (h>highval16) then raise AnyException
    else v
  in
  try
    match v with
    | Any i -> Any i
    | Interval((v,i)) -> Interval (check_aint16_int v,i)
    | IntervalList(l,sp,i) -> IntervalList(List.map check_aint16_int l,sp,i)
  with AnyException -> Any false

let check_aint8 v =
  let check_aint8_int v =
    let (l,h) = v in
    if (l<lowval8) || (h>highval8) then raise AnyException
    else v
  in
  try
    match v with
    | Any i -> Any i
    | Interval(v,i) -> Interval ((check_aint8_int v),i)
    | IntervalList(l,sp,i) -> IntervalList(List.map check_aint8_int l,sp,i)
  with AnyException -> Any false

  
let interval_merge (l1,h1) (l2,h2) =
  (min l1 l2, max h1 h2)

let interval_merge_list lst =
  match lst with
  | [] -> fail_aint32()
  | l::ls -> List.fold_left interval_merge l ls 

    
let aint32_to_int32 v =
  match v with
  | Any _ | IntervalList(_) -> None
  | Interval((l,h),i) when l <> h -> None
  | Interval((l,_),i) -> Some l

    
let aint32_pprint debug v =
  let prn (l,h) =
    if l = h then us (sprintf "%d" l)
    else us (sprintf "[%d,%d]" l h)
  in  
  match v with
  | Any _ -> us"Any"
  | Interval((l,h),_) -> prn (l,h)
  | IntervalList(lst,sp,_) ->
    us(if debug && sp != nopair then sprintf "{%d}" sp else "") ^.
    (if debug then
      us"[" ^. Ustring.concat (us", ") (List.map prn lst) ^. us"]"
    else 
      prn (interval_merge_list lst))
        

let aint32_print_debug v =
  uprint_endline (aint32_pprint true v)


    (********************* SPLITER Functions ***********************)
let split_aint32 v slist =
  let rec split_interval v slist =
    let split_interval_one (l,h) sl =
      let half l h = (h-l)/2 + (h-l) mod 2 in
      let l,h = 
      match sl with
      | Abstract -> (l,h)
      | AbstractL ->
         let h = half l h in
         (l,h)
      | AbstractR ->
         let l = half l h + 1 in
         (l,h)
      | ConcreteL -> (l,l)
      | ConcreteR -> (h,h)
      | ConcreteML ->
         let ml = l + (half l h) in
         (ml,ml) 
      | ConcreteMR ->
         let ml = l + (half l h) + 1 in
         (ml,ml)
      in
      (l,h)
    in
    match v,slist with
    | (l,h), _  when l == h -> (v,slist)
    | (l,h), [] -> (v,[]) (* failwith "Should not happen split" *)
    | (l,h), sl::[] ->
       let v = split_interval_one (l,h) sl in
       (v,[sl])
    | (l,h), sl::sls ->
       let v = split_interval_one (l,h) sl in
       split_interval v sls
  in
  let v,sl = (
  match v with
  | Any true
  | Interval(_,true)
  | IntervalList(_,_,true) -> v,slist
  | Any false ->
     let v,sl = split_interval (lowval,highval) slist in
     
     Interval(v,true),sl
  | Interval(v,false) ->
     let v,sl = split_interval v slist in
     Interval(v,true),sl
  | IntervalList(vl,sp,false) -> failwith "Should not happen: Spliting an intervallist"
  ) in
  v,sl
    
     (* let v,sl = split_interval (lowval,1,number highval lowval 1) slist in *)
     (* Interval(v,true) *)

                   
(*********************END SPLITER*************************)



(*TODO(Romy): congruence*)
let rec aint32_mem_byte byte v =
  match v with
  | Any i | IntervalList(_,_,i) -> Any i
  | Interval (_,i) -> Any i
             
           
(*TODO(Romy): Not implemented*)
(* let rec aint32_mem_update_byte byte newv oldv = *)
(*   match newv,oldv with *)
(*   | _,_ -> Any *)
     
let aint32_binop op v1 v2 =
  try 
    match v1,v2 with
    | Any i,Any j -> Any (i && j)
    | Any i,_ | _,Any i -> Any i
    | Interval(vv1,i1), Interval(vv2, i2) ->
      (* assert (i1 = i2); *)
      Interval (op vv1 vv2,i1)
    | Interval(vv1,i1),IntervalList(lst,sp,i2) | IntervalList(lst,sp,i1), Interval(vv1,i2) ->
       (* assert (i1 = i2); *)
       IntervalList(List.map (fun vv2 -> op vv1 vv2) lst, sp, i1)
    | IntervalList(l1,sp1,i1), IntervalList(l2,sp2,i2) when sp1 = sp2 ->
       (* assert (i1 = i2); *)
      IntervalList(List.rev (List.rev_map2 (fun v1 v2 -> op v1 v2) l1 l2), sp1,i1)     
    | IntervalList(l1,_,i1), IntervalList(l2,_,i2) ->
      Interval (op (interval_merge_list l1) (interval_merge_list l2),i1)
  with AnyException -> Any false
          
let aint32_add v1 v2 =
  aint32_binop (fun (l1,h1) (l2,h2) ->
    let l = l1+l2 in
    let h = h1+h2 in
    if l<lowval || h>highval then raise AnyException
    else (l,h)
  ) v1 v2

               
let aint32_sub v1 v2 =
  aint32_binop (fun (l1,h1) (l2,h2) ->
      let l2,h2 = -h2,-l2 in
      let l = l1+l2 in
      let h = h1+h2 in
      if l<lowval || h>highval then raise AnyException
      else (l,h)
    ) v1 v2


let aint32_not_f (l,h) =
  (lnot h, lnot l)
                        
let aint32_and_f (l1,h1) (l2,h2) =
  let leading_ones l =
    let rec leading_ones_internal l n =
      if (l land n = n) then n
      else leading_ones_internal l (n lsl 1)
    in
    leading_ones_internal l (-1) in
                               
  let l = if l1 >= 0 && l2 >= 0 then 0
          else if l1*l2 < 0 then
            if h1>0 && h2>0 then 0
            else 0 (*can probably tighten*)
          else leading_ones (min l1 l2) in

  let h = if h1*h2<0 then max h1 h2
          else min h1 h2 in

  if l<lowval || h>highval then raise AnyException
  else (l,h)
           
let aint32_and v1 v2 =
  aint32_binop  aint32_and_f v1 v2

let aint32_or_f v1 v2 =
  aint32_not_f (aint32_and_f (aint32_not_f v1) (aint32_not_f v2))
               
let aint32_or v1 v2 =
  aint32_binop aint32_or_f v1 v2
  
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

let mul_f v1 v2 =
  match v1,v2 with
  | (l1,h1),(l2,h2) when l1 == h1 && l2 == h2 ->
     let l = l1 * l2 in
     (l, l)
  (* Multiply constant with interval *)
  | (l1,h1),(l2,h2) when l1 == h1 ->
     let l = min (l1*l2) (l1*h2) in
     let h = max (l1*l2) (l1*h2) in
     (l,h)     
  | ((l2,h2),(l1,h1)) when l1 == h1 ->
     let l = min (l1*l2) (l1*h2) in
     let h = max (l1*l2) (l1*h2) in
     (l,h)
  | ((l1,h1),(l2,h2)) -> 
     let l = min (min (l1*l2) (l1*h2)) (min (h1*l2) (h1*h2)) in
     let h = max (max (l1*l2) (l1*h2)) (max (h1*l2) (h1*h2)) in
     (l,h)

let aint32_mul v1 v2 =
  aint32_binop (fun (l1,h1) (l2,h2) ->
    let l = min (min (l1*l2) (l1*h2)) (min (h1*l2) (h1*h2)) in
    let h = max (max (l1*l2) (l1*h2)) (max (h1*l2) (h1*h2)) in
    if l<lowval || h>highval then raise AnyException
    else (l,h)
  ) v1 v2

(* Check for (always positive) v2*)
let aint32_sllv v1 v2 =
  aint32_binop (fun (l1,h1) (l2,h2) ->
    let l = l1 lsl l2 in
    let h = h1 lsl h2 in
    if l<lowval || h>highval then raise AnyException
    else (l,h)
  ) v1 v2

let aint32_srlv v1 v2 =
  aint32_binop (fun (l1,h1) (l2,h2) ->
    let l = l1 lsr l2 in
    let h = h1 lsr h2 in
    if l<lowval || h>highval then raise AnyException
    else (l,h)
  ) v1 v2



let aint64_binop op v1 v2 =
  try 
    match v1,v2 with
    | Any i,_|_,Any i -> (Any i,Any i)
    | Interval(vv1,i1), Interval(vv2,i2) ->
       let (r1,r2) = op vv1 vv2 in
       (Interval(r1,i1), Interval(r2,i2))
    | Interval(vv1,i1),IntervalList(lst,sp,i2) | IntervalList(lst,sp,i1), Interval(vv1,i2) ->
       let (r1,r2) = List.split (List.map (fun vv2 -> op vv1 vv2) lst) in
       (IntervalList(r1,sp,i1),IntervalList(r2,sp,i2))
    | IntervalList(l1,sp1,i1), IntervalList(l2,sp2,i2) when sp1 = sp2 ->
       let (r1,r2) = List.split (List.rev (List.rev_map2 (fun v1 v2 -> op v1 v2) l1 l2)) in
       (IntervalList(r1,sp1,i1),IntervalList(r2,sp1,i2))
    | IntervalList(l1,_,i1), IntervalList(l2,_,i2) ->
       let (r1,r2) = op (interval_merge_list l1) (interval_merge_list l2) in
       (Interval(r1,i1),Interval(r2,i2))
  with AnyException -> (Any true,Any true)

let aint64_mult v1 v2 =
  aint64_binop (
      fun v1 v2 ->
      let (l,h) = mul_f v1 v2 in
      if  l<lowval || h>highval then raise AnyException
      (*Sign extension*)
      else if l>=0 then ((0,0),(l,h))
      else if h<0 then ((-1,-1),(l,h))
      else ((-1,0),(l,h))
    ) v1 v2
    
let mod5 (l,h) =
  let maxv = (1 lsl 5) - 1  in
  if l == h then
    let v = l land 0x1f in
    (v,v)
  else if h > maxv || l < 0 then 
    (0, maxv)
  else (l,h)

let aint32_srav v1 v2 =
  aint32_binop (fun (l1,h1) (l2,h2) ->
      let (l2,h2) = mod5 (l2,h2) in
      let l = l1 asr l2 in
      let h = h1 asr h2 in
      if l<lowval || h>highval then raise AnyException
      else if (l1 == h1 && l2 == h2) then (l1 asr l2, l1 asr l2)
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



let aint32_clz v1 =
  let rec leading_zeros v n =
    match v with
    | v when v < 0 -> 0
    | 0 -> n
    | _ -> leading_zeros (v lsr 1) (n-1)
  in
  match v1 with
  (* All possible results *)
  | Interval((l,h),i) ->
     if h < 0 then Interval((0,0),true)
     else if l < 0 then Interval((0,32),true)
     else 
       ( let l1 = leading_zeros h 32 in
         let h1 = leading_zeros l 32 in
         Interval((l1,h1),i)
       )
  | _ -> Interval((0,32),true) 

let aint32_mod v1 v2 =
  match v1, v2 with
  | Any i1, Interval((l,h),i2) ->
     (* assert (i1 = i2); *)
     let modl = min l (-h) in
     let modh = max h (-l) in
     if modl<lowval || modh>highval then raise AnyException
     else Interval((modl, modh),i1)
  | Any i,_ | _,Any i -> Any i
  | _ ->
     aint32_binop (fun (l1,h1) (l2,h2) ->
         if (l2<=0 && h2 >=0) then raise Division_by_zero
         else
           if (l1 = h1 && l2 = h2) then
             let v = l1 mod l2 in
             (v,v)
           else
             (* very conservative *)
             let k = max (max (abs l1) ((abs l2)-1))
                         (max (abs h1) ((abs h2)-1)) in
             let l,h = 
               if (l1>0 && l2>0) then (0, k)
               else if (h1<0 && h2<0) then (-k,0)
               else (-k,k)
             in
             if l<lowval || h>highval then raise AnyException
             else (l,h)
       ) v1 v2

let aint32_const v =
    Interval((v,v),true)

let aint32_const_uninit v =
    Interval((v,v),false)
      
let aint32_interval l h =
    Interval((l,h),true)

let aint32_interval_uninit l h =
    Interval((l,h),false)


let aint32_join v1 v2 =
  match v1, v2 with
  | Any i,_|_,Any i -> Any i
  | Interval(v1,i1),Interval(v2,i2) ->
    Interval(interval_merge v1 v2,i1)
  | Interval(v1,i1),IntervalList(lst,_,i2) | IntervalList(lst,_,i1),Interval(v1,i2) ->
    Interval(interval_merge_list (v1::lst),i1)
  | IntervalList(l1,_,i1),IntervalList(l2,_,i2) ->
    Interval(interval_merge (interval_merge_list l1) (interval_merge_list l2),i2) 

             
let aint32_compare x y =
  compare x y



let aint_merge v1 v2 bits =
  let mask = (0x1 lsl bits) - 1 in 
  match v1, v2 with
  | Interval((v1),i1),Interval((v2),i2) ->
     (* assert (i1 = i2); *)
     (match v1,v2 with
     | (l1,h1),(l2,h2) when l1 == h1 && l2 == h2 ->
        let l = (l1 lsl bits) lor (l2 land mask) in
        Interval((l,l),i1)
     | (l1,h1),(l2,h2) when l2>=0 && h1 == l1 ->
        let l = (l1 lsl bits) lor (l2 land mask) in
        let h = (h1 lsl bits) lor (h2 land mask) in
        Interval((l,h),i1)
     | (l1,h1),(l2,h2) when l2<0 && h1 == l1->
        if h2<0 then
          let l = (l1 lsl bits) lor (h2 land mask) in
          let h = (h1 lsl bits) lor (l2 land mask) in
          Interval((l,h),i1)
        else
          let l = min (h2 land mask) (l1 land mask) in
          let h = max (h2 land mask) (l1 land mask) in
          Interval((l,h),i1)
     | (l1,h1),(l2,h2) when l2 == h2 ->
        let l = (l1 lsl bits) lor (l2 land mask) in
        let h = (h1 lsl bits) lor (l2 land mask) in
        Interval((l,h),i1)
     | _,_ -> Any i1
     ) 
  | _,_ -> Any true (*TODO(Romy):?????*) 

             
let aint16_merge v1 v2 = aint_merge v1 v2 16

let aint8_merge v1 v2 = aint_merge v1 v2 8
 
let signextend v bit =
  let sign = (1 lsl (bit-1)) in
  let mask = (1 lsl bit) - 1 in
  let newv = v land mask in
  if (v land sign) = sign then
    -(((newv lxor mask) land mask) + 1)
  else newv

let aint_split bigendian v bits =
  match v with
  | Interval((l,h),true) when l == h ->
     let l1 = l asr bits in
     let l2 = signextend l bits in
     let v1,v2 =
       (Interval((l1,l1),true),
        Interval((l2,l2),true)) in
     if bigendian then (v1,v2)
     else (v2,v1)
  | Interval((l,h),i) ->
     let l1 = l asr bits in
     let h1 = h asr bits in
     (* TODO(Romy): Tighten for s lsr bits != 0 *)
     let v1 = Interval((l1,h1),i) in
     let v2 =
       if h1 == l1 then
         Interval((l,h),i)
       else
         Any i
     in
     if bigendian then
       (v1, v2)
     else
       (v2, v1)
  | _ -> (Any true ,Any true)

let aint32_split bigendian v = aint_split bigendian v 16

let aint16_split bigendian v = aint_split bigendian v 8
             
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
  | Any i,_|_,Any i -> (Some(Any i,Any i),Some(Any i,Any i))
  | Interval((l1,h1),i1), Interval((l2,h2),i2) ->
    ((if l1 < h2 then
         Some(Interval((l1,min h1 (h2-1)),i1), Interval((max (l1+1) l2,h2),i2))
      else None),
     (if h1 >= l2 then
         Some(Interval((max l1 l2,h1),i1), Interval((l2,min h2 h1),i2))        
      else None))
  | Interval(v1,i1),IntervalList(l1,_,i2) | IntervalList(l1,_,i1), Interval(v1,i2) ->
    aint32_test_less_than (Interval(v1,i1)) (Interval(interval_merge_list l1,i2))
  | IntervalList(l1,_,i1), IntervalList(l2,_,i2) ->
    aint32_test_less_than (Interval(interval_merge_list l1,i1))
                          (Interval(interval_merge_list l2,i2))

(* Same as the above, but conservative for negative numbers *)      
let rec aint32_test_less_than_unsigned v1 v2 =
  match v1,v2 with
  | Any i,_|_,Any i -> (Some(Any i,Any i),Some(Any i,Any i))
  | Interval((l1,h1),i1), Interval((l2,h2),i2) ->
    if l1 < 0 || l2 < 0 then (Some(Any i1,Any i2),Some(Any i1,Any i2))
    else
      ((if l1 < h2 then
          Some(Interval((l1,min h1 (h2-1)),i1), Interval((max (l1+1) l2,h2),i2))
        else None),
       (if h1 >= l2 then
           Some(Interval((max l1 l2,h1),i1), Interval((l2,min h2 h1),i2))        
        else None))
  | Interval(v1,i1),IntervalList(l1,_,i2) | IntervalList(l1,_,i1), Interval(v1,i2) ->
    aint32_test_less_than_unsigned (Interval(v1,i1)) (Interval(interval_merge_list l1,i2))
  | IntervalList(l1,_,i1), IntervalList(l2,_,i2) ->
    aint32_test_less_than_unsigned (Interval(interval_merge_list l1,i1))
                          (Interval(interval_merge_list l2,i2))
  
      
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
  | Any i,_|_,Any i -> (Some(Any i,Any i),Some(Any i,Any i))
  | Interval((l1,h1),i1), Interval((l2,h2),i2) ->
    ((if l1 <= h2 then
         Some(Interval((l1,min h1 (h2)),i1), Interval((max (l1) l2,h2),i2))
      else None),
     (if h1 > l2 then
         Some(Interval((max l1 (l2+1),h1),i1), Interval((l2,min h2 (h1-1)),i2))        
      else None))
  | Interval(v1,i1),IntervalList(l1,_,i2) | IntervalList(l1,_,i1), Interval(v1,i2) ->
    aint32_test_less_than_equal (Interval(v1,i1)) (Interval(interval_merge_list l1,i2))
  | IntervalList(l1,_,i1), IntervalList(l2,_,i2) ->
    aint32_test_less_than_equal (Interval(interval_merge_list l1,i1))
                          (Interval(interval_merge_list l2,i2))
                          
    
      
(* Test if two abstract integers are equal. Returns a tuple where the
   elements are options for if there is a "true" branch or a "false" 
   branch *)
let rec aint32_test_equal v1 v2 =
  match v1,v2 with
  | Any i,_|_,Any i -> (Some(Any i,Any i),Some(Any i,Any i))
  (* Case when we just compare two intervals. May generate safe pair lists *)
  | Interval(v1,i1),Interval(v2,i2) ->
    let mkval vlst =
        match vlst with
        | [] -> None
        | [(v1,v2)] -> Some(Interval(v1,i1),Interval(v2,i2))
        | v ->
          let (v1,v2) = split_rev v in
          let s = getPairSym() in
          Some(IntervalList(v1,s,i1),IntervalList(v2,s,i2))
    in
    (* Make the actual equality tests *)
    let (t,f) = test_equal v1 v2 in
    (mkval t, mkval f)

      
  (* Case when we have two paired interval lists *)
  | IntervalList(l1,sp1,i1), IntervalList(l2,sp2,i2)
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
           Some(IntervalList(t1,s,i1),IntervalList(t2,s,i2))),
        (if f1=[] then None else
           let s = getPairSym() in
           Some(IntervalList(f1,s,i1),IntervalList(f2,s,i2))))

  (* Cases where there a no safe pairs. We then collaps the structure 
     and perform interval test for equality *)
  | Interval(v1,i1),IntervalList(l1,_,i2) | IntervalList(l1,_,i1), Interval(v1,i2) ->
    aint32_test_equal (Interval(v1,i1)) (Interval(interval_merge_list l1,i2))
  | IntervalList(l1,_,i1), IntervalList(l2,_,i2) ->
    aint32_test_equal (Interval(interval_merge_list l1,i1))
                         (Interval(interval_merge_list l2,i2))

     

let aint32_test_greater_than_equal v1 v2 =
  let t,f = aint32_test_less_than v1 v2 in
  (f,t)

let aint32_test_greater_than v1 v2 =
  let t,f = aint32_test_less_than_equal v1 v2 in
  (f,t)
