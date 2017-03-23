
open Ustring.Op
open Printf

type number = int
type low = int
type step = int
              
exception Exception_aint32
  
type safepair = int
let pairSym = ref 0  
let nopair = 0
let getPairSym() =
  pairSym := !pairSym + 1;
  !pairSym

type interval = (low * step * number)
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

let gcd a b =
  let a = abs a in
  let b = abs b in
  let (ma,mi) = if a > b then (a,b) else (b,a) in
  let rec gcd_rec a b =
    if b = 0
    then a
    else gcd_rec b (a mod b) in
  gcd_rec ma mi

let high l s n = l+s*(n-1)

let number h l s =
  if s = 0 then 1
  else (h-l)/s+1

let dec_num n = max 1 (n-1) 
(*n*)
let interval_merge (l1,s1,n1) (l2,s2,n2) =
  let h1 = high l1 s1 n1 in
  let h2 = high l2 s2 n2 in
  let l = min l1 l2 in
  let s = gcd (abs (l1-l2)) (gcd s1 s2) in
  let h = max h1 h2 in
  (l, s, number h l s)

let interval_merge_list lst =
  match lst with
  | [] -> fail_aint32()
  | l::ls -> List.fold_left interval_merge l ls 

    
let aint32_to_int32 v =
  match v with
  | Interval(l,_,1) -> Some l
  | Any | IntervalList(_,_)
    | Interval(_) -> None

    
let aint32_pprint debug v =
  let prn (l,s,n) =
    match (l,s,n) with
    | l,0,1 -> us (sprintf "%d " l)
    (* TODO(Romy): change to Error - failwith *)
    | l,_,1 | l,0,_-> us (sprintf "Error: aint32_pprint - trying to print a constant, but the parameters don't match %d:%d:%d" l s n)
    | l,1,n ->
       us (sprintf "[%d,%d] " l (high l s n))
    | l,s,n ->
       us (sprintf "[%d,%d,%d] " l s (high l s n))
  in  
  match v with
  | Any -> us"Any "
  | Interval(l,s,n) -> prn (l,s,n)
  | IntervalList(lst,sp) ->
    us(if debug && sp != nopair then sprintf "{%d}" sp else "") ^.
    (if debug then
      us"[" ^. Ustring.concat (us", ") (List.map prn lst) ^. us"]"
    else 
      prn (interval_merge_list lst))
        

let aint32_print_debug v =
  uprint_endline (aint32_pprint true v)

(*big/little endian*)                 
let rec aint32_mem_byte byte signed v =
  let get_byte byte (l,s,n) =
    let shift_val signed v =
      let newv = v lsr (byte lsl 3) in
      newv
    in
     let l = shift_val signed l in
     let s = shift_val false s in
     let h = shift_val signed (high l s n) in
     let n = number h l s in 
     (l, s, n) in
  match v with
  | Interval(v) -> Interval(get_byte byte v)
  | _ -> Any
          
let rec aint32_mem_update_byte byte newv oldv = Any
(*  let interv_set_byte byte (l1,s1,n1) (ol1,os1,on1) =
    let set_byte newv oldv =
      let newi = newv lsl (byte lsl 3)  in
      let mask = 0xff lsl (byte lsl 3)  in
      ((oldv land (lnot mask)) lor newi)
    in
     (set_byte l1 ol1, set_byte h1 oh1) in
 
  match newv,oldv with
  | _,_ -> Any
  | _,Any -> Any
  | Interval(nv1),Interval(v1) -> Interval(interv_set_byte byte nv1 v1)
  | Interval(nv1),IntervalList(l,sp) ->
     IntervalList(List.map (interv_set_byte byte nv1) l,sp)
  | _,_ -> raise Exception_aint32
 *)
          
let rec aint32_mem_update_byte byte newv oldv = Any
(*  let interv_set_byte byte (l1,s1,n1) (ol1,os1,on1) =
    let set_byte newv oldv =
      let newi = newv lsl (byte lsl 3)  in
      let mask = 0xff lsl (byte lsl 3)  in
      ((oldv land (lnot mask)) lor newi)
    in
     (set_byte l1 ol1, set_byte h1 oh1) in
  match newv,oldv with
  | _,Any -> Any
  | Interval(nv1),Interval(v1) -> Interval(interv_set_byte byte nv1 v1)
  | Interval(nv1),IntervalList(l,sp) ->
     IntervalList(List.map (interv_set_byte byte nv1) l,sp)
  | _,_ -> raise Exception_aint32
 *)

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


(*n*)
let aint32_add v1 v2 =
  aint32_binop (fun (l1,s1,n1) (l2,s2,n2) ->
      let h1 = high l1 s1 n1 in
      let h2 = high l2 s2 n2 in
      let l = l1 + l2 in
      let s = gcd s1 s2 in
      let h = h1+h2 in
      let n = number h l s in
    if l<lowval || h>highval then raise AnyException
    else (l,s,n)
  ) v1 v2


               
let aint32_sub v1 v2 =
  aint32_binop (fun (l1,s1,n1) (l2,s2,n2) ->
      let h1 = high l1 s1 n1 in
      let h2 = high l2 s2 n2 in
      let l2,h2 = -h2,-l2 in
      let s = gcd s1 s2 in
      let l = l1+l2 in
      let h = h1+h2 in
      let n = number h l s in
      if l<lowval || h>highval then raise AnyException
      else (l,s,n)
    ) v1 v2

(*TODO(step)*)
let aint32_and_f (l1,s1,n1) (l2,s2,n2) =
  let leading_ones l =
    let rec leading_ones_internal l n =
      if (l land n = n) then n
      else leading_ones_internal l (n lsl 1)
    in
    leading_ones_internal l (-1) in
  match (l1,s1,n1),(l2,s2,n2) with
  | (_,_,1),(_,_,1) -> (l1 land l2, 0, 1)
  | (l2,s2,2),(l1,s1,1) | (l1,s1,1),(l2,s2,2) ->
     let v1 = l1 land l2 in
     let v2 = l1 land (l2+s2) in
     let l = min v1 v2 in
     let s = (max v1 v2)-l in
     (l,s,2)
  | _ ->
     let h1 = high l1 s1 n1 in
     let h2 = high l2 s2 n2 in 
     let l = if l1 >= 0 && l2 >= 0 then 0
             else if l1*l2 < 0 then
               if h1>0 && h2>0 then 0
               else 0 (*TODO(Romy): (maybe) tighten*)
             else leading_ones (min l1 l2) in
     let h = if h1*h2<0 then max h1 h2
             else min h1 h2 in
     let s = if h=l then 0 else 1 in
     let n = number h l s in
     if l<lowval || h>highval then raise AnyException
     else (l,s,n)
                                     
let aint32_and v1 v2 =
  let is_power_2 l =
    if l = 0 then false
    else (l land (lnot (l-1)) = l)
  in
  let (v1,v2) = 
    match v1,v2 with
    | Any, Interval(l,0,1) when is_power_2 l -> (Interval(0,l,2),v2)
    | Interval(l,0,1),Any when is_power_2 l -> (v1,Interval(0,l,2))
    | _,_ -> (v1,v2)
  in
  aint32_binop aint32_and_f v1 v2

let aint32_not_f (l,s,n) =
  (lnot (high l s n), s, n)

let aint32_or_f v1 v2 =
  aint32_not_f (aint32_and_f
                  (aint32_not_f v1)
                  (aint32_not_f v2))
           
let aint32_or v1 v2 =
  aint32_binop aint32_or_f v1 v2
    
let aint32_nor v1 v2 =
  aint32_binop (fun t1 t2 ->
      aint32_and_f
        (aint32_not_f t1)
        (aint32_not_f t2)) v1 v2

              
let aint32_xor v1 v2 =
  let aint32_xor_f t1 t2 =
    aint32_or_f
      (aint32_and_f (aint32_not_f t1) t2)
      (aint32_and_f t1 (aint32_not_f t2))
  in
  aint32_binop aint32_xor_f v1 v2


let mul_f v1 v2 =
  match v1,v2 with
  | (l1,0,1),(l2,0,1) ->
     let l = l1 * l2 in
     (l, 0, 1)
  (* Multiply constant with interval *)
  | (l1,0,1),(l2,s2,n) | ((l2,s2,n),(l1,0,1)) ->
     let h2 = high l2 s2 n in
     let l = min (l1*l2) (l1*h2) in
     let s = abs (l1*s2) in
     let n = if s = 0 then 1 else n in
     (l,s,n)
  | ((l1,s1,n1),(l2,s2,n2)) -> 
     let h1 = high l1 s1 n1 in
     let h2 = high l2 s2 n2 in
     let l = min (min (l1*l2) (l1*h2)) (min (h1*l2) (h1*h2)) in
     let h = max (max (l1*l2) (l1*h2)) (max (h1*l2) (h1*h2)) in
     let s = gcd (abs (l2*s1)) (gcd (abs (l1*s2)) (s1*s2)) in
     let n = number h l s in
     (l,s,n)
               
let aint32_mul v1 v2 =
  aint32_binop (
      fun v1 v2 ->
      let (l,s,n) = mul_f v1 v2 in
      if l<lowval || (high l s n)>highval then raise AnyException
      else (l,s,n)
    ) v1 v2

  
let aint64_binop op v1 v2 =
  try 
    match v1,v2 with
    | Any,_|_,Any -> (Any,Any)
    | Interval(vv1), Interval(vv2) ->
       let (r1,r2) = op vv1 vv2 in
       (Interval(r1), Interval(r2))
    | Interval(vv1),IntervalList(lst,sp) | IntervalList(lst,sp), Interval(vv1) ->
       let (r1,r2) = List.split (List.map (fun vv2 -> op vv1 vv2) lst) in
       (IntervalList(r1,sp),IntervalList(r2,sp))
    | IntervalList(l1,sp1), IntervalList(l2,sp2) when sp1 = sp2 ->
       let (r1,r2) = List.split (List.rev (List.rev_map2 (fun v1 v2 -> op v1 v2) l1 l2)) in
       (IntervalList(r1,sp1),IntervalList(r2,sp1))
    | IntervalList(l1,_), IntervalList(l2,_) ->
       let (r1,r2) = op (interval_merge_list l1) (interval_merge_list l2) in
       (Interval(r1),Interval(r2))
  with AnyException -> (Any,Any)

let aint64_mult v1 v2 =
  aint64_binop (
      fun v1 v2 ->
      let (l,s,n) = mul_f v1 v2 in
      let h = high l s n in
      if  l<lowval || h>highval then raise AnyException
      (*Sign extension*)
      else if l>=0 then ((0,0,1),(l,s,n))
      else if h<0 then ((-1,0,1),(l,s,n))
      else ((-1,1,2),(l,s,n))
    ) v1 v2

let aint32_sllv v1 v2 =
  aint32_binop (fun (l1,s1,n1) (l2,s2,n2) ->
      let h1 = high l1 s1 n1 in
      let h2 = high l2 s2 n2 in
      let l = l1 lsl l2 in
      let h = h1 lsl h2 in
      let s = if n1 = 1 then 0
              else if n2 = 1 then (s1 lsl l2)
              else (gcd l1 s1) lsl l2 in
      let n = number h l s in
      if l<lowval || h>highval then raise AnyException
      else (l,s,n)
    ) v1 v2

let aint32_srlv v1 v2 =
  aint32_binop (fun (l1,s1,n1) (l2,s2,n2) ->
      let h1 = high l1 s1 n1 in
      let h2 = high l2 s2 n2 in
      let l = l1 lsr l2 in
      let h = h1 lsr h2 in
      let s = if h = l then 0 else 1 in 
      let n = number h l s in
      if l<lowval || h>highval then raise AnyException
      else if (n1 = 1 && n2 = 1) then (l1 lsr l2, 0, 1)
      else (l,s,n)
    ) v1 v2
               
(* TODO(Romy): tighen - Copy of aint32_srlv*)
let aint32_srav v1 v2 =
  aint32_binop (fun (l1,s1,n1) (l2,s2,n2) ->
      let h1 = high l1 s1 n1 in
      let h2 = high l2 s2 n2 in
      let l = l1 asr l2 in
      let h = h1 asr h2 in
      let s = if h = l then 0 else 1 in 
      let n = number h l s in
      if l<lowval || h>highval then raise AnyException
      else if (n1 = 1 && n2 = 1) then (l1 asr l2, 0, 1)
      else (l,s,n)
    ) v1 v2

               
let aint32_div v1 v2 =
  match v1, v2 with
  | Any, Interval(l,0,1) ->
     let h = highval/l in
     let l = lowval/l in
     let s = 1 in
     let n = number h l s in
     if l<lowval || h>highval then raise AnyException
     else Interval(l, s, n)
  | Any,_ | _,Any -> Any
  | _ ->
     aint32_binop (fun (l1,s1,n1) (l2,s2,n2) ->
         let h1 = high l1 s1 n1 in
         let h2 = high l2 s2 n2 in
         if (l2<=0 && h2 >=0) then raise Division_by_zero
         else
           if (n1 = 1 && n2 = 1) then (l1/l2, 0, 1)
           else
             let l = min (min (l1/l2) (l1/h2)) (min (h1/l2) (h1/h2)) in
             let h = max (max (l1/l2) (l1/h2)) (max (h1/l2) (h1/h2)) in
             let s = if h = l then 0 else 1 in 
             let n = number h l s in
             if l<lowval || h>highval then raise AnyException
             else (l,s,n)
       ) v1 v2

let aint32_mod v1 v2 =
  match v1, v2 with
  | Any, Interval(l,s,n) ->
     let h = high l s n in
     let modl = min l (-h) in
     let modh = max h (-l) in
     let mods = 1 in
     let modn = number modh modl mods in 
     if modl<lowval || modh>highval then raise AnyException
     else Interval(modl, mods, modn)
  | Any,_ | _,Any -> Any
  | _ ->
     aint32_binop (fun (l1,s1,n1) (l2,s2,n2) ->
         let h1 = high l1 s1 n1 in
         let h2 = high l2 s2 n2 in
         if (l2<=0 && h2 >=0) then raise Division_by_zero
         else
           if (n1 = 1 && n2 = 1) then (l1 mod l2, 0, 1)
           else
             (* very conservative *)
             let k = max (max (abs l1) ((abs l2)-1))
                         (max (abs h1) ((abs h2)-1)) in
             let l,h = 
               if (l1>0 && l2>0) then (0, k)
               else if (h1<0 && h2<0) then (-k,0)
               else (-k,k)
             in
             let s = if h = l then 0 else 1 in 
             let n = number h l s in
             if l<lowval || h>highval then raise AnyException
             else (l,s,n)
       ) v1 v2

    
let aint32_const v =
    Interval(v,0,1)

let aint32_interval l h =
  let h = h in
  if l = h then Interval(l,0,1)
  else Interval(l,1,number h l 1)

let aint32_join v1 v2 =
  match v1, v2 with
  | Any,_|_,Any -> Any
  | Interval(v1),Interval(v2) ->
    Interval(interval_merge v1 v2)
  | Interval(v1),IntervalList(lst,_) | IntervalList(lst,_),Interval(v1) ->
    Interval(interval_merge_list (v1::lst))
  | IntervalList(l1,_),IntervalList(l2,_) ->
    Interval(interval_merge (interval_merge_list l1) (interval_merge_list l2)) 

             
let aint32_compare x y =
  compare x y
    
(* Check for equality. Returns two lists,
   one for cases when they are equal, one when they not *)    
let test_equal (l1,s1,n1) (l2,s2,n2) =
  let h1 = high l1 s1 n1 in
  let h2 = high l2 s2 n2 in
  if l1=l2 && h1=h2 && n1=2 && n2=2 then
    (* Binary values that are equal *)
    ([((l1,0,1),(l1,0,1)); ((h1,0,1),(h1,0,1))],
     [((l1,0,1),(h1,0,1)); ((h1,0,1),(l1,0,1))])     
  else if h1 < l2 || h2 < l1 then
    (
     (* Not overlapping: 
         111       |   22222
             2222  |        1111
      *)
    ([],                                             (* TRUE *)
     [((l1,s1,n1),(l2,s2,n2))])                            (* FALSE *))
  else if h1 = l2 then
    ( (* Overlapping with one:
            11111
                22222 
       *)
      if l1 < h1 && l2 < h2 then
      (* Two cases for false *)
        let nn2 = dec_num n2 in
        let ns2 = if nn2 = 1 then 0
                    else s2 in
        ([((h1,0,1),(h1,0,1))],                          (* TRUE *)
         [((l1,s1,n1),(l2+s2,ns2,nn2));((l1,s1,dec_num n1),(l2,s2,n2))])    (* FALSE *)
      else if l1 < h1 && l2 = h2 then
      (*  11111

              2  *)  
        ([((h1,0,1),(h1,0,1))],                          (* TRUE *)
         [((l1,s1,dec_num n1),(l2,s2,n2))])                        (* FALSE *)
      else if l1 = h1 && l2 < h2 then
      (*      1
              22222 *)
        let nn2 = dec_num n2 in
        let ns2 = if nn2 = 1 then 0
                  else s2 in
        ([((h1,0,1),(h1,0,1))],                          (* TRUE *)
         [((l1,s1,n1),(l2+s2,ns2,nn2))])                        (* FALSE *)
      else
      (*      1
              2  *)
        ([((h1,0,1),(h1,0,1))],                          (* TRUE *)
         [])                                           (* FALSE *)
    )
  else if l1 = h2 then
    ( (* Overlapping with one:
              11111
          22222 
      *)
      if l1 < h1 && l2 < h2 then
        let nn1 = dec_num n1 in
        let ns1 = if nn1 = 1 then 0
                  else s1 in
      (* Two cases for false *)
      ([((h2,0,1),(h2,0,1))],                          (* TRUE *)
       [((l1+s1,ns1,nn1),(l2,s2,n2));((l1,s1,n1),(l2,s2,dec_num n2))])    (* FALSE *)
    else if l2 < h2 && l1 = h1 then
        (*  22222
              1  *)  
      ([((h2,0,1),(h2,0,1))],                          (* TRUE *)
       [((l1,s1,n1),(l2,s2,dec_num n2))]                        (* FALSE *)
        )    else if l2 = h2 && l1 < h1 then
        let nn1 = dec_num n1 in
        let ns1 = if nn1 = 1 then 0
                  else s1 in
        (*      2
              11111 *)
      ([((h2,0,1),(h2,0,1))],                          (* TRUE *)
       [((l1+s1,ns1,nn1),(l2,s2,n2))])                        (* FALSE *)
    else
      (*      1
              2  *)
      ([((h2,0,1),(h2,0,1))],                          (* TRUE *)
       [])                                           (* FALSE *)
    )
      (* TODO(Romy): Not tested *)
  (*
  else if l1 <= l2 && h1 >= h2 then
    (if s1 = 0 then
           printf "test equal: Should not happen\n%!";
     let tl1 = l1 + ((l2 - l1) / s1) * s1 in
     let th1 = l1 + ((h2 - l1) / s1) * s1 in
     let ts1 = if tl1 = th1 then 0 else s2 in 
     let tn1 = number th1 tl1 ts1 in 
     ([((tl1,ts1,tn1),(l2,s2,n2))],                   (* TRUE *)
      [((l1,s1,n1),(l2,s2,n2))])                     (* FALSE *)
    )
  else if l2 <= l1 && h2 >= h1 then(
    (if s2 = 0 then
      printf "test equal: Should not happen\n%!";
     let tl2 = l2 + ((l1 - l2) / s2) * s2 in
     let th2 = l2 + ((h1 - l2) / s2) * s2 in
     let ts2 = if tl2 = th2 then 0 else s2 in
     let tn2 = number th2 tl2 ts2 in 
     ([((l1,s1,n1),(tl2,ts2,tn2))],                   (* TRUE *)
      [((l1,s1,n1),(l2,s2,n2))])                     (* FALSE *)
    ))
 *)
  else
      (* More overlapping than one. Conservative, both 
         cases. TODO: Make the true-branch less 
         concervative. *)
      ([((l1,s1,n1),(l2,s2,n2))],                          (* TRUE *)
       [((l1,s1,n1),(l2,s2,n2))])                          (* FALSE *)


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
  | Any,Any -> (Some(Any,Any),Some(Any,Any))
  | Any,_ -> (Some(Any,v2),Some(Any,v2))
  | _,Any -> (Some(v1,Any),Some(v1,Any))
  | Interval((l1,s1,n1)), Interval((l2,s2,n2)) ->
     let h1 = high l1 s1 n1 in
     let h2 = high l2 s2 n2 in
     ((if l1 < h2 then
         let h11 = min h1 (h2-(max s2 1)) in
         let l22 = if s2 = 0 || l2 > (l1 + s1) then l2
                   else max (l2 + s2) (l2 + ((l1 + s1 - l2) / s2) * s2)
         in
         let s1 = if h11 = l1 then 0 else s1 in
         let s2 = if l22 = h2 then 0 else s2 in
         Some(Interval(l1,s1,number h11 l1 s1), Interval(l22,s2,number h2 l22 s2))
      else None),
      (if h1 >= l2 then
        let l11 = if s1 = 0 || l1 > l2
                  then l1
                  else l1 + ((l2 - l1) / s1) * s1
        in          
        let h22 = min h2 h1 in
        let s1 = if l11 = h1 then 0 else s1 in
        let s2 = if l2 = h22 then 0 else s2 in
        Some(Interval(l11,s1,number h1 l11 s1), Interval(l2,s2,number h22 l2 s2)        
         )else None))
  | Interval(v1),IntervalList(l1,_) | IntervalList(l1,_), Interval(v1) ->
    aint32_test_less_than (Interval(v1)) (Interval(interval_merge_list l1))
  | IntervalList(l1,_), IntervalList(l2,_) ->
    aint32_test_less_than (Interval(interval_merge_list l1))
                          (Interval(interval_merge_list l2))

(* Same as the above, but conservative for negative numbers *)      
let rec aint32_test_less_than_unsigned v1 v2 =
  match v1,v2 with
  | Any,Any -> (Some(Any,Any),Some(Any,Any))
  | Any,_ -> (Some(Any,v2),Some(Any,v2))
  | _,Any -> (Some(v1,Any),Some(v1,Any))
  | Interval((l1,s1,n1)), Interval((l2,s2,n2)) ->
     let h1 = high l1 s1 n1 in
     let h2 = high l2 s2 n2 in
     if l1 < 0 || l2 < 0 then (Some(Any,Any),Some(Any,Any))
     else
       (*TODO(Romy): check again (max s 1)*)
       ((if l1 < h2 then
           let h11 = min h1 (h2-(max s2 1)) in
           let s1 = if h11 = l1 then 0 else s1 in
           let l22 = if s2 = 0 || l2 > (l1 + (max s1 1))
                     then l2
                     else max (l2 + s2) (l2 + (((l1 + s1 - l2) / s2)) * s2)
           in
           let s2 = if l22 = h2 then 0 else s2 in
           
           Some(Interval(l1,s1,number h11 l1 s1), Interval(l22,s2,number h2 l2 s2))
         else None),
        (if h1 >= l2 then
           let l11 = if s1 = 0 || l1 > l2
                  then l1
                  else l1 + ((l2 - l1) / s1) * s1
           in
           let s1 = if h1 = l11 then 0 else s1 in
           let h22 = min h1 h2 in
           let s2 = if h22 = l2 then 0 else s2 in
           Some(Interval(l11,s1,number h1 l11 s2), Interval(l2,s2, number h22 l2 s2 ))        
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
  | Any,Any -> (Some(Any,Any),Some(Any,Any))
  | Any,_ -> (Some(Any,v2),Some(Any,v2))
  | _,Any -> (Some(v1,Any),Some(v1,Any))
  | Interval((l1,s1,n1)), Interval((l2,s2,n2)) ->
     let h1 = high l1 s1 n1 in
     let h2 = high l2 s2 n2 in
     ((if l1 <= h2 then
         let h11 = min h1 h2 in
         let s1 = if l1 = h11 then 0 else s1 in 
         let l22 = if s2 = 0 || l2 > (l1 + (max s1 1))
                   then l2
                   else l2 + ((l1 + s1 - l2) / s2) * s2
         in
         let s2 = if l22 = h2 then 0 else s2 in
         Some(Interval(l1,s1,number h11 l1 s1), Interval(l22,s2, number h2 l22 s2))
      else None),
     (if h1 > l2 then
        let l11 = if s1 = 0 || l1 > l2
                  then l1
                  else max (l1 + s1) (l1 + ((l2 - l1 + s1) / s1) * s1)
        in
        let h22 = min (h1-(max s1 1)) h2 in
        let s2 = if l2 = h22 then 0 else s2 in
        let s1 = if l11 = h1 then 0 else s1 in
        Some(Interval(l11,s1,number h1 l11 s1), Interval(l2,s2,number h22 l2 s2))        
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
  | Any,Any -> (Some(Any,Any),Some(Any,Any))
  | Any,v ->  (Some(v,v),Some(Any,v))
  | v,Any ->  (Some(v,v),Some(v,Any))
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

let aint32_test_greater_than_equal v1 v2 =
  let t,f = aint32_test_less_than v1 v2 in
  (f,t)

let aint32_test_greater_than v1 v2 =
  let t,f = aint32_test_less_than_equal v1 v2 in
  (f,t)
