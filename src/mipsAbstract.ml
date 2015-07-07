
open Ustring.Op
open Printf
open MipsAst
open AbstractInt32


(* ---------------------------------------------------------------------*)
type astate = {
  register_at : aint32;
  register_v0 : aint32;
  register_v1 : aint32;
  register_a0 : aint32;
  register_a1 : aint32;
  register_a2 : aint32;
  register_a3 : aint32;
  register_t0 : aint32;
  register_t1 : aint32;
  register_t2 : aint32;
  register_t3 : aint32;
  register_t4 : aint32;
  register_t5 : aint32;
  register_t6 : aint32;
  register_t7 : aint32;
  register_s0 : aint32;
  register_s1 : aint32;
  register_s2 : aint32;
  register_s3 : aint32;
  register_s4 : aint32;
  register_s5 : aint32;
  register_s6 : aint32;
  register_s7 : aint32;
  register_t8 : aint32;
  register_t9 : aint32;
  register_k0 : aint32;
  register_k1 : aint32;
  register_gp : aint32;
  register_sp : aint32;
  register_fp : aint32;
  register_ra : aint32;

  pc : int;
  distance : int;
}

type distance = int array

(* ---------------------------------------------------------------------*)

(* Enable this flag to pretty print debug info *)
let debug = true

let sll_max_vals = 64
  
(* ---------------------------------------------------------------------*)

  
let int32max = 0x7fffffff
let int32min = (0x7fffffff + 1) * -1

let pprint_aint32 lst =
  let elems = List.map (fun (l,u) -> us(if l=u then sprintf "%d" l else sprintf "[%d,%d]" l u)) lst
  in us"{" ^. Ustring.concat (us",") elems ^. us"}"

let make_aint32 v = ([v,v])

let make_aint32_intervals ival = ival

let aint32_addu xlst ylst =
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

    
(* Returns a tuple (true_list,false_list) *)
let aint32_blez xlst =
  List.fold_left
      (fun (tacc,facc) (xl,xu) ->
        if xu <= 0 then ((xl,xu)::tacc,facc)
        else if xl > 0 then (tacc,(xl,xu)::facc)
        else ((xl,0)::tacc,(1,xu)::facc)) ([],[]) xlst

    
(* Shift into unique values if fewer than sll_max_vals. Used for jump tables *)
let aint32_sll xlst shift =
  let sleft x b = (x lsl b) land 0xffffffff in
  match xlst with
  | [(l,u)] when u - l <= sll_max_vals ->
    let rec mk k =
      let k' = sleft k shift in
      if k = u then [(k',k')] else (k',k')::(mk (k+1)) in mk l  
  | _ -> List.map (fun (l,u) -> (sleft l shift, sleft u shift)) xlst

    


    
  
 
  

let init_state =
  let reg_init = make_aint32 0 in
  {
  register_at = reg_init;
  register_v0 = reg_init;
  register_v1 = reg_init;
  register_a0 = reg_init;
  register_a1 = reg_init;
  register_a2 = reg_init;
  register_a3 = reg_init;
  register_t0 = reg_init;
  register_t1 = reg_init;
  register_t2 = reg_init;
  register_t3 = reg_init;
  register_t4 = reg_init;
  register_t5 = reg_init;
  register_t6 = reg_init;
  register_t7 = reg_init;
  register_s0 = reg_init;
  register_s1 = reg_init;
  register_s2 = reg_init;
  register_s3 = reg_init;
  register_s4 = reg_init;
  register_s5 = reg_init;
  register_s6 = reg_init;
  register_s7 = reg_init;
  register_t8 = reg_init;
  register_t9 = reg_init;
  register_k0 = reg_init;
  register_k1 = reg_init;
  register_gp = reg_init;
  register_sp = reg_init;
  register_fp = reg_init;
  register_ra = reg_init;

  pc = 0;
  distance = max_int;
  }


(* ---------------------------------------------------------------------*)
let reg0 = make_aint32 0 
let reg state reg = 
  match reg with
  | 0 -> reg0
  | 1 -> state.register_at
  | 2 -> state.register_v0 
  | 3 -> state.register_v1 
  | 4 -> state.register_a0
  | 5 -> state.register_a1 
  | 6 -> state.register_a2
  | 7 -> state.register_a3 
  | 8 -> state.register_t0 
  | 9 -> state.register_t1 
  | 10 -> state.register_t2 
  | 11 -> state.register_t3 
  | 12 -> state.register_t4 
  | 13 -> state.register_t5
  | 14 -> state.register_t6
  | 15 -> state.register_t7 
  | 16 -> state.register_s0 
  | 17 -> state.register_s1 
  | 18 -> state.register_s2 
  | 19 -> state.register_s3 
  | 20 -> state.register_s4 
  | 21 -> state.register_s5 
  | 22 -> state.register_s6 
  | 23 -> state.register_s7 
  | 24 -> state.register_t8 
  | 25 -> state.register_t9 
  | 26 -> state.register_k0
  | 27 -> state.register_k1 
  | 28 -> state.register_gp 
  | 29 -> state.register_sp 
  | 30 -> state.register_fp
  | 31 -> state.register_ra
  | _ -> failwith "Illegal register."

(* ---------------------------------------------------------------------*)
let wreg state reg v =
  match reg with
  | 0 -> state
  | 1 -> {state with register_at=v}
  | 2 -> {state with register_v0=v} 
  | 3 -> {state with register_v1=v} 
  | 4 -> {state with register_a0=v}
  | 5 -> {state with register_a1=v} 
  | 6 -> {state with register_a2=v}
  | 7 -> {state with register_a3=v} 
  | 8 -> {state with register_t0=v} 
  | 9 -> {state with register_t1=v} 
  | 10 -> {state with register_t2=v} 
  | 11 -> {state with register_t3=v} 
  | 12 -> {state with register_t4=v} 
  | 13 -> {state with register_t5=v}
  | 14 -> {state with register_t6=v}
  | 15 -> {state with register_t7=v} 
  | 16 -> {state with register_s0=v} 
  | 17 -> {state with register_s1=v} 
  | 18 -> {state with register_s2=v} 
  | 19 -> {state with register_s3=v} 
  | 20 -> {state with register_s4=v} 
  | 21 -> {state with register_s5=v} 
  | 22 -> {state with register_s6=v} 
  | 23 -> {state with register_s7=v} 
  | 24 -> {state with register_t8=v} 
  | 25 -> {state with register_t9=v} 
  | 26 -> {state with register_k0=v}
  | 27 -> {state with register_k1=v} 
  | 28 -> {state with register_gp=v} 
  | 29 -> {state with register_sp=v} 
  | 30 -> {state with register_fp=v}
  | 31 -> {state with register_ra=v} 
  | _ -> failwith "Illegal register."
  

    
(* ---------------------------------------------------------------------*)
let rec step prog s =
  let inst = prog.code.((s.pc - prog.text_sec.addr)/4) in
  let pc inc s = {s with pc = s.pc + inc} in
  let branch addr s = {s with pc = addr} in
  match inst  with 
  | MipsADDU(rd,rs,rt) ->
      [wreg s rd (aint32_addu (reg s rs) (reg s rt)) |> pc 4]
  | MipsBLEZ(rs,imm,_) ->
    (match step prog (pc 4 s) with
     | [s'] -> (  
       let (tval,fval) = aint32_blez (reg s rs) in
       let s2 = if List.length tval = 0 then []
                else [wreg s' rs tval |> branch (imm*4 + 4 + s.pc)] in
       if List.length fval = 0 then s2 else (wreg s' rs fval |> pc 4)::s2 )
     | _ -> failwith "MipsBLEZ failure")
  | MipsSLL(rd,rt,shamt) ->
     [wreg s rd (aint32_sll (reg s rt) shamt) |> pc 4]      
  | _ -> failwith ("Unknown instruction: " ^
                    Ustring.to_utf8 (MipsUtils.pprint_inst inst))

    
(* ---------------------------------------------------------------------*)
let rec multistep prog statelst dist =
  match statelst with
  | s::ls ->
    let newstates = step prog s in
    let statelst' =
      List.fold_left (fun acc s ->
        let rec insert st lst =
          match lst with
          | o::ol ->
              if st.distance >= o.distance
              then st::lst
              else o::(insert st ol)
          | [] -> [st]              
        in
          insert {s with distance = dist.((s.pc - prog.text_sec.addr)/4)} acc
      ) ls newstates
    in
      if List.length statelst' = 1 && (List.hd statelst').pc = 0
      then List.hd statelst'
      else multistep prog statelst' dist
  | [] -> failwith "multistep fail. This should not happen."

    


    
(* ---------------------------------------------------------------------*)
let init prog func args =
  (* Set the PC address to the address given by the func parameter *)
  let pc_addr = List.assoc func prog.symbols
  in    
    {init_state with pc = pc_addr}      
 

(* ---------------------------------------------------------------------*)
let distance prog func args =
  let len = Array.length prog.code in
  Array.mapi (fun i _ -> len - i) (Array.make len 0)
  

    
(* ---------------------------------------------------------------------*)
let eval  ?(bigendian=false)  prog state dist timeout =
  let state' = multistep prog [state] dist in
  let measurement = 0 in
  let ok = true in
  (ok, measurement, state')

    
    
(* ---------------------------------------------------------------------*)
let main argv =
  let s = init_state in
  let s2 = wreg s reg_t4 (make_aint32 7) in
  printf "hello:";
  uprint_endline (pprint_aint32 (reg s2 reg_t4));
  let v1 = make_aint32 7 in
  let v2 = make_aint32_intervals [(2,8);(10,20)] in
  let v3 = make_aint32_intervals [(-2,100)] in
  uprint_endline (pprint_aint32 v1);
  uprint_endline (pprint_aint32 v2);
  uprint_endline (pprint_aint32 (aint32_addu v1 v2));
  uprint_endline (pprint_aint32 (aint32_addu v2 v3));
  uprint_endline (pprint_aint32 (aint32_addu v2 v2))
  
  











    
