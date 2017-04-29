
open Ustring.Op
open Printf

type num = int
type low = int
type step = int
              
exception Exception_aint32
  
type safepair = int
let pairSym = ref 0  
let nopair = 0
let getPairSym() =
  pairSym := !pairSym + 1;
  !pairSym

type initialized = bool
  
type interval = (low * step * num)
type aint32 =
  | Any of initialized
  | Interval of interval * initialized
  | IntervalList of interval list * safepair * initialized

let baseaddr_fail() = failwith "Error: cannot perform operations on base addresses"     
let fail_aint32() = failwith "Error: aint32 error that should not happen."


let lowval = -2147483648
let highval = 2147483647

(* let lowval = -9223372036854775808L *)
(* let highval = 9223372036854775807L *)

let ulowval = 0
let uhighval = 4294967295

let lowval8 = -127
let highval8 = 128
let ulowval8 = 0
let uhighval8 = 255

let lowval16 = -32768
let highval16 = 32767
let ulowval16 = 0
let uhighval16 = 65535

let aint32_any = Any false

let aint32_any_set v = Any v

  
let get_initialized =
  function | Any i | Interval (_,i) | IntervalList(_,_,i) -> i

let set_initialized v i =
  match v with
  | Any _ -> Any i
  | Interval (v,_) -> Interval (v,i)
  | IntervalList(lv,sp,_) -> IntervalList(lv,sp,i)
     

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



(***********************SPLITER**************************)
  
type spliter = | Abstract
               | AbstractL
               | AbstractR
               | ConcreteM
               | ConcreteL
               | ConcreteR
                   


let check_aint16 v =
  let check_aint16_int v =
    let (l,s,n) = v in
    let h = high l s n in
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
    let (l,s,n) = v in
    let h = high l s n in
    if (l<lowval8) || (h>highval8) then raise AnyException
    else v
  in
  try
    match v with
    | Any i -> Any i
    | Interval(v,i) -> Interval ((check_aint8_int v),i)
    | IntervalList(l,sp,i) -> IntervalList(List.map check_aint8_int l,sp,i)
  with AnyException -> Any false


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
  | Interval((l,_,1),i) -> Some l
  | Any _ | IntervalList(_,_,_)
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
  | Any _ -> us"Any "
  | Interval((l,s,n),i) -> prn (l,s,n)
  | IntervalList(lst,sp,i) ->
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
    let split_interval_one (l,s,n) sl =
      let half n = n/2 + n mod 2 in
      match sl with
      | Abstract -> (l,s,n)
      | AbstractL ->
         let n' = half n in
         let s' = if n'=1 then 0 else s in
         (l,s',n')
      | AbstractR ->
         let l' = l+s*(half n) in
         let n' = n-(half n) in
         let s' = if n'=1 then 0 else s in
         (l',s',n')
      | ConcreteL -> (l,0,1)
      | ConcreteR -> (high l s n,0,1)
      | ConcreteM -> (l+s*((half n)-1),0,1)
  in
    match v,slist with
    | (l,0,1), _ -> (v,slist)
    | (l,s,n), [] -> (v,[]) (* failwith "Should not happen split" *)
    | (l,s,n), sl::[] ->
       let v = split_interval_one (l,s,n) sl in
       (v,[sl])
    | (l,s,n), sl::sls ->
       let v = split_interval_one (l,s,n) sl in
       split_interval v sls
  in
  let v,sl = (
  match v with
  | Any true
  | Interval(_,true)
  | IntervalList(_,_,true) -> v,slist
  | Any false ->
     let v,sl = split_interval (lowval,1,number highval lowval 1) slist in
     
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
  | Interval(v,i) -> Interval(get_byte byte v,i)
  | Any i | IntervalList(_,_,i) -> Any i 
          

let aint32_binop op v1 v2 =
  try 
    match v1,v2 with
    | Any i,_|_,Any i -> Any i
    (*?????*)
    | Interval(vv1,i1), Interval(vv2,i2) ->
       (* assert (i1 = i2); *)
       Interval (op vv1 vv2,i1)
    | Interval(vv1,i1),IntervalList(lst,sp,i2) | IntervalList(lst,sp,i1), Interval(vv1,i2) ->
       (* assert (i1 = i2); *)
      IntervalList(List.map (fun vv2 -> op vv1 vv2) lst, sp,i1)
    | IntervalList(l1,sp1,i1), IntervalList(l2,sp2,i2) when sp1 = sp2 ->
       (* assert (i1 = i2); *)
       IntervalList(List.rev (List.rev_map2 (fun v1 v2 -> op v1 v2) l1 l2), sp1,i1)     
    | IntervalList(l1,_,i1), IntervalList(l2,_,i2) ->
       (* assert (i1 = i2); *)
       Interval (op (interval_merge_list l1) (interval_merge_list l2),i1)
  with AnyException -> Any false


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
     let h = max v1 v2 in
     let s = h - l in
     let n = number h l s in
     (l,s,n)
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
    | Any i1, Interval((l,0,1),i2) when is_power_2 l -> (Interval((0,l,2),i1),v2)
    | Interval((l,0,1),i1),Any i2 when is_power_2 l -> (v1,Interval((0,l,2),i2))
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
      let (l,s,n) = mul_f v1 v2 in
      let h = high l s n in
      if  l<lowval || h>highval then raise AnyException
      (*Sign extension*)
      else if l>=0 then ((0,0,1),(l,s,n))
      else if h<0 then ((-1,0,1),(l,s,n))
      else ((-1,1,2),(l,s,n))
    ) v1 v2

let mod5 (l,s,n) =
  let maxv = (1 lsl 5) - 1  in
  if n = 1 then
    (l land 0x1f, 0, 1)
  else if (high l s n) > maxv || l < 0 then 
    (0, 1, number maxv 0 1)
  else (l,s,n)
            
let aint32_sllv v1 v2 =
  aint32_binop (fun (l1,s1,n1) (l2,s2,n2) ->
      let (l2,s2,n2) = mod5 (l2,s2,n2) in
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
      let (l2,s2,n2) = mod5 (l2,s2,n2) in
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
      let (l2,s2,n2) = mod5 (l2,s2,n2) in
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
  | Any i1, Interval((l,0,1),i2) ->
     (* assert (i1 = i2); *)
     let h = highval/l in
     let l = lowval/l in
     let s = 1 in
     let n = number h l s in
     if l<lowval || h>highval then raise AnyException
     else Interval((l,s,n),i1)
  | Any i,_ | _,Any i -> Any i
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

let aint32_clz v1 =
  let rec leading_zeros v n =
    match v with
    | v when v < 0 -> 0
    | 0 -> n
    | _ -> leading_zeros (v lsr 1) (n-1)
  in
  match v1 with
  (* All possible results *)
  | Interval((l,s,n),i) ->
     let h = high l s n in
     if h < 0 then Interval((0,0,1),true)
     else if l < 0 then Interval((0,1,number 32 0 1),true)
     else 
       ( let l1 = leading_zeros h 32 in
         let h1 = leading_zeros l 32 in
         let s1 = if l1 = h1 then 0 else 1 in
         Interval((l1,s1,number h1 l1 s1),i)
       )
  | _ -> Interval((0,1,number 32 0 1),true) 
                  
let aint32_mod v1 v2 =
  match v1, v2 with
  | Any i1, Interval((l,s,n),i2) ->
     (* assert (i1 = i2); *)
     let h = high l s n in
     let modl = min l (-h) in
     let modh = max h (-l) in
     let mods = 1 in
     let modn = number modh modl mods in 
     if modl<lowval || modh>highval then raise AnyException
     else Interval((modl, mods, modn),i1)
  | Any i,_ | _,Any i -> Any i
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
    Interval((v,0,1),true)

let aint32_const_uninit v =
    Interval((v,0,1),false)
      
let aint32_interval l h =
  if l = h then Interval((l,0,1),true)
  else Interval((l,1,number h l 1),true)

let aint32_interval_uninit l h =
  if l = h then Interval((l,0,1),false)
  else Interval((l,1,number h l 1),false)

let aint32_join v1 v2 =
  match v1, v2 with
  | Any i,_|_,Any i -> Any i
  | Interval((v1),i1),Interval((v2),i2) ->
     (* assert(i1=i2); *)
     Interval(interval_merge v1 v2,i1)
  | Interval((v1),i1),IntervalList(lst,_,i2) | IntervalList(lst,_,i1),Interval(v1,i2) ->
     (* assert (i1 = i2); *)
    Interval((interval_merge_list (v1::lst),i1))
  | IntervalList(l1,_,i1),IntervalList(l2,_,i2) ->
     (* assert (i1 = i2); *)
    Interval((interval_merge (interval_merge_list l1)) (interval_merge_list l2),i1) 

let aint_merge v1 v2 bits =
  let mask = (0x1 lsl bits) - 1 in 
  match v1, v2 with
  | Interval((v1),i1),Interval((v2),i2) ->
     (* assert (i1 = i2); *)
     (match v1,v2 with
     | (l1,0,1),(l2,0,1) -> Interval(((l1 lsl bits) lor (l2 land mask),0,1),i1)
     | (l1,0,1),(l2,s2,n2) when l2>=0 -> Interval(((l1 lsl bits) lor (l2 land mask),s2,n2),i1)
     | (l1,0,1),(l2,s2,n2) when l2<0 ->
        let h2 = high l2 s2 n2 in
        if h2<0 then Interval(((l1 lsl bits) lor (h2 land mask),s2,n2),i1)
        else
          let l = min (h2 land mask) (l1 land mask) in
          let h = max (h2 land mask) (l1 land mask) in
          let s = if l=h then 0 else 1 in
          let n = number h l s in
          Interval(((l1 lsl bits) lor l,s,n),i1)
     | (l1,s1,n1),(l2,0,1) -> Interval(((l1 lsl bits) lor (l2 land mask),(s1 lsl bits),n1),i1)
     | (l1,s1,n1),(l2,s2,n2) -> Any i1
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
  | Interval((l,0,1),true) ->
     let v1,v2 = (Interval((l asr bits, 0, 1),true),
                  Interval((signextend l bits, 0, 1),true)) in
     if bigendian then (v1,v2)
     else (v2,v1)
  | Interval((l,s,n),i) ->
     let h = high l s n in
     let l1 = l asr bits in
     let h1 = h asr bits in
     (* TODO(Romy): Tighten for s lsr bits != 0 *)
     let s1 = if (l1 = h1) then 0 else 1 in
     let n1 = number h l s in
     let v1 = Interval((l1,s1,n1),i) in
     let v2 =
       if n1 = 1 then
         Interval((l,s,n),i)
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
  | Any i1,Any i2 ->
     (* assert (i1 = i2); *)
    (Some(Any i1,Any i1),Some(Any i1,Any i1))
  | Any i1,Interval((l,s,n),i2) ->
     let h = high l s n in
     let anyl1 = lowval in
     let anyh1 = h - 1 in
     let anys1 = if anyh1 = anyl1 then 0 else 1 in
     let anyn1 = number anyh1 anyl1 anys1 in
     let anyl2 = l in
     let anyh2 = highval in
     let anys2 = if anyh2 = anyl2 then 0 else 1 in
     let anyn2 = number anyh2 anyl2 anys2 in
     (Some(Interval((anyl1,anys1,anyn1),i1),v2),Some(Interval((anyl2,anys2,anyn2),i1),v2))
  | Any i,_ -> (Some(Any i,v2),Some(Any i,v2))
  | Interval((l,s,n),i1),Any i ->
     (* assert (i1 = i); *)
     let h = high l s n in
     let anyl1 = l + 1 in
     let anyh1 = highval in
     let anys1 = if anyl1 = anyh1 then 0 else 1 in
     let anyn1 = number anyh1 anyl1 anys1 in
     let anyl2 = lowval in
     let anyh2 = h in
     let anys2 = if anyl2 = anyh2 then 0 else 1 in
     let anyn2 = number anyh2 anyl2 anys2 in
     (Some(v1, Interval((anyl1,anys1,anyn1),i)),Some(v1,Interval((anyl2,anys2,anyn2),i)))               
  | _,Any i -> (Some(v1,Any i),Some(v1,Any i))
  | Interval((l1,s1,n1),i1), Interval((l2,s2,n2),i2) ->
     (* assert (i1 = i2); *)
     let h1 = high l1 s1 n1 in
     let h2 = high l2 s2 n2 in
     ((if l1 < h2 then
         let h11 = min h1 (h2-(max s2 1)) in
         let l22 = if s2 = 0 || l2 > (l1 + s1) then l2
                   else max (l2 + s2) (l2 + ((l1 + s1 - l2) / s2) * s2)
         in
         let s1 = if h11 = l1 then 0 else s1 in
         let s2 = if l22 = h2 then 0 else s2 in        
         Some(Interval((l1,s1,number h11 l1 s1),i1), Interval((l22,s2,number h2 l22 s2),i2))
      else None),
      (if h1 >= l2 then
        let l11 = if s1 = 0 || l1 > l2
                  then l1
                  else l1 + ((l2 - l1) / s1) * s1
        in          
        let h22 = min h2 h1 in
        let s1 = if l11 = h1 then 0 else s1 in
        let s2 = if l2 = h22 then 0 else s2 in
        Some(Interval((l11,s1,number h1 l11 s1),i1), Interval((l2,s2,number h22 l2 s2),i2)        
         )else None))
  | Interval(v1,i1),IntervalList(l1,_,i) | IntervalList(l1,_,i), Interval(v1,i1) ->
     (* assert (i1 = i); *)
    aint32_test_less_than (Interval(v1,i)) (Interval(interval_merge_list l1,i))
  | IntervalList(l1,_,i), IntervalList(l2,_,i1) ->
     (* assert (i1 = i); *)
     aint32_test_less_than (Interval(interval_merge_list l1,i))
                          (Interval(interval_merge_list l2,i))

(* Same as the above, but conservative for negative numbers *)      
let rec aint32_test_less_than_unsigned v1 v2 =
  match v1,v2 with
  | Any i1,Any i2 ->
     (* assert (i1 = i2); *)
     (Some(Any i1,Any i1),Some(Any i1,Any i2))
  | Any i1,Interval((l,s,n),i2) ->
     (* assert (i1 = i2); *)
     let h = high l s n in
     let anyl1 = lowval in
     let anyh1 = h - 1 in
     let anys1 = if anyl1=anyh1 then 0 else 1 in
     let anyn1 = number anyh1 anyl1 anys1 in
     let anyl2 = l in
     let anyh2 = highval in
     let anys2 = if anyl2=anyh2 then 0 else 1 in
     let anyn2 = number anyh2 anyl2 anys2 in
     (Some(Interval((anyl1,anys1,anyn1),i1),v2),Some(Interval((anyl2,anys2,anyn2),i1),v2))
  | Any i,_ -> (Some(Any i,v2),Some(Any i,v2))
  | Interval((l,s,n),i1),Any i2 ->
     (* assert (i1 = i2); *)
     let h = high l s n in
     let anyl1 = l + 1 in
     let anyh1 = highval in
     let anys1 = if anyl1=anyh1 then 0 else 1 in
     let anyn1 = number anyh1 anyl1 anys1 in
     let anyl2 = lowval in
     let anyh2 = h in
     let anys2 = if anyl2=anyh2 then 0 else 1 in
     let anyn2 = number anyh2 anyl2 anys2 in
     (Some(v1, Interval((anyl1,anys1,anyn1),i1)),Some(v1, Interval((anyl2,anys2,anyn2),i2)))               
  | _,Any i -> (Some(v1,Any i),Some(v1,Any i))
  | Interval((l1,s1,n1),i1), Interval((l2,s2,n2),i2) ->
     let h1 = high l1 s1 n1 in
     let h2 = high l2 s2 n2 in
     (*if l1 < 0 || l2 < 0 then (Some(Any,Any),Some(Any,Any))
     else*)
       (*TODO(Romy): check again (max s 1)*)
       ((if l1 < h2 then
           let h11 = min h1 (h2-(max s2 1)) in
           let s1 = if h11 = l1 then 0 else s1 in
           let l22 = if s2 = 0 || l2 > (l1 + (max s1 1))
                     then l2
                     else max (l2 + s2) (l2 + (((l1 + s1 - l2) / s2)) * s2)
           in
           let s2 = if l22 = h2 then 0 else s2 in
           
           Some(Interval((l1,s1,number h11 l1 s1),i1), Interval((l22,s2,number h2 l2 s2),i2))
         else None),
        (if h1 >= l2 then
           let l11 = if s1 = 0 || l1 > l2
                  then l1
                  else l1 + ((l2 - l1) / s1) * s1
           in
           let s1 = if h1 = l11 then 0 else s1 in
           let h22 = min h1 h2 in
           let s2 = if h22 = l2 then 0 else s2 in
           Some(Interval((l11,s1,number h1 l11 s1),i1), Interval((l2,s2, number h22 l2 s2),i2))        
         else None))
  | Interval(v1,i1),IntervalList(l1,_,i2) | IntervalList(l1,_,i1), Interval(v1,i2) ->
    aint32_test_less_than_unsigned (Interval(v1,i1)) (Interval((interval_merge_list l1),i1))
  | IntervalList(l1,_,i1), IntervalList(l2,_,i2) ->
     (* assert (i1 = i2); *)
    aint32_test_less_than_unsigned (Interval((interval_merge_list l1),i1))
                          (Interval(interval_merge_list l2,i1))
  
      
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
  | Any i1,Any i2 ->
     (* assert(i1 = i2) *)
    (Some(Any i1,Any i2),Some(Any i1,Any i2))
  | Any i1,Interval((l,s,n),i) ->
     (* assert(i1 = i) *)
     let h = high l s n in
     let anyl1 = lowval in
     let anys = 1 in
     let anyh1 = h in
     let anyn1 = number anyh1 anyl1 anys in
     let anyl2 = l + 1 in
     let anyh2 = highval in
     let anyn2 = number anyh2 anyl2 anys in
     (Some(Interval((anyl1,anys,anyn1),i),v2),Some(Interval((anyl2,anys,anyn2),i),v2))
  | Any i,_ -> (Some(Any i,v2),Some(Any i,v2))
  | Interval((l,s,n),i),Any i1 ->
     (* assert(i1 = i) *)
     let h = high l s n in
     let anyl1 = l in
     let anys = 1 in
     let anyh1 = highval in
     let anyn1 = number anyh1 anyl1 anys in
     let anyl2 = lowval in
     let anyh2 = h - 1 in
     let anyn2 = number anyh2 anyl2 anys in
     (Some(v1,Interval((anyl1,anys,anyn1),i)),Some(v1, Interval((anyl2,anys,anyn2),i)))
  | _,Any i -> (Some(v1,Any i),Some(v1,Any i))
  | Interval(((l1,s1,n1),i1)), Interval(((l2,s2,n2),i)) ->
     (* assert(i1 = i) *)
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
         Some(Interval((l1,s1,number h11 l1 s1),i), Interval((l22,s2, number h2 l22 s2),i))
      else None),
     (if h1 > l2 then
        let l11 = if s1 = 0 || l1 > l2
                  then l1
                  else max (l1 + s1) (l1 + ((l2 - l1 + s1) / s1) * s1)
        in
        let h22 = min (h1-(max s1 1)) h2 in
        let s2 = if l2 = h22 then 0 else s2 in
        let s1 = if l11 = h1 then 0 else s1 in
        Some(Interval((l11,s1,number h1 l11 s1),i), Interval((l2,s2,number h22 l2 s2),i))        
      else None))
  | Interval((v1),i1),IntervalList(l1,_,i2) | IntervalList(l1,_,i1), Interval((v1),i2) ->
     (* assert(i1 = i2) *)
    aint32_test_less_than_equal (Interval((v1),i1)) (Interval((interval_merge_list l1),i1))
  | IntervalList(l1,_,i1), IntervalList(l2,_,i2) ->
     (* assert(i1 = i2) *)
    aint32_test_less_than_equal (Interval((interval_merge_list l1),i1))
                          (Interval((interval_merge_list l2),i1))
                          
    
      
(* Test if two abstract integers are equal. Returns a tuple where the
   elements are options for if there is a "true" branch or a "false" 
   branch *)
let rec aint32_test_equal v1 v2 =
  match v1,v2 with
  | Any i1,Any i2 ->
     (* assert(i1 = i2) *)
    (Some(Any i1,Any i1),Some(Any i1,Any i1))
  | Any i,v ->  (Some(v,v),Some(Any i,v))
  | v,Any i ->  (Some(v,v),Some(v,Any i))
  (* Case when we just compare two intervals. May generate safe pair lists *)
  | Interval((v1),i1),Interval((v2),i) ->
     (* assert(i1 = i) *)
     let mkval vlst =
        match vlst with
        | [] -> None
        | [(v1,v2)] -> Some(Interval((v1),i),Interval((v2),i))
        | v ->
          let (v1,v2) = split_rev v in
          let s = getPairSym() in
          Some(IntervalList(v1,s,i),IntervalList(v2,s,i))
    in
    (* Make the actual equality tests *)
    let (t,f) = test_equal v1 v2 in
    (mkval t, mkval f)

      
  (* Case when we have two paired interval lists *)
  | IntervalList(l1,sp1,i1), IntervalList(l2,sp2,i2)
    when sp1=sp2       
    -> (* Tail-recursive test of interval lists *)
     (* assert(i1 = i2) *)
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
             Some(IntervalList(t1,s,i1),IntervalList(t2,s,i1))),
          (if f1=[] then None else
              let s = getPairSym() in
              Some(IntervalList(f1,s,i1),IntervalList(f2,s,i1))))

  (* Cases where there a no safe pairs. We then collaps the structure 
     and perform interval test for equality *)
  | Interval((v1),i1),IntervalList(l1,_,i2) | IntervalList(l1,_,i1), Interval((v1),i2) ->
     (* assert(i1 = i2) *)
    aint32_test_equal (Interval((v1),i1)) (Interval((interval_merge_list l1),i1))
  | IntervalList(l1,_,i1), IntervalList(l2,_,i2) ->
     (* assert(i1 = i2) *)
     aint32_test_equal (Interval((interval_merge_list l1),i1))
                         (Interval((interval_merge_list l2),i2))

let aint32_test_greater_than_equal v1 v2 =
  let t,f = aint32_test_less_than v1 v2 in
  (f,t)

let aint32_test_greater_than v1 v2 =
  let t,f = aint32_test_less_than_equal v1 v2 in
  (f,t)
