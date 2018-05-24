open Aregsimple
open Config
open Printf

(* 5-stage pipeline with times when each stage is free *)
type apipeline_t = int * int * int * int * int

type instruction_type =
  | Mem of registers option * registers option * registers option
  | Br of registers option * registers option
  | ND of registers option * registers option
          * registers option * registers option

type dependency_t = instruction_type * int (* registers * int  *)

type apipeline = apipeline_t * dependency_t option

let print_pipeline str ((f,d,e,m,w),_) =
  let rec print_lines ls n str =
    match ls,n,str with
    | [],n,str -> List.rev str
    | l::ls,n,str when l>n -> print_lines (l::ls) (n+1) ("_"::str)
    | l::ls,n,str -> print_lines ls (n+1) ("_|"::str)
  in
  printf "Pipeline %s:\n%s\n%!" str (String.concat
                                       "" (print_lines (f::d::e::m::w::[]) 1 []))

                                       
let normalize pf (f,d,e,m,w) = (f-pf, d-pf, e-pf, m-pf, w-pf)
  
let pipeline_update instr p dp =
    let get_dependencies dr instr = 
      match dr,instr with
    (* | (Mem (dr,_,_),t),Br(sr1,sr2) -> *)
      | Some (dr,t), Br(sr1,sr2) ->
         let dr =
           match dr with
           | Mem (dr,_,_) | ND (dr,_,_,_) -> dr
           | _ -> None
         in
         let t =
           match dr,sr1,sr2 with
           | Some dr, Some sr, _ when sr = dr -> t
           | Some dr, _, Some sr when sr = dr -> t
           | _ -> 0
         in
         (t,0)
      | Some (Mem (dr,_,_),t),instr ->
         let sr1,sr2 =
           match instr with
           | Mem (_,sr1,sr2)
           | ND (_,_,sr1,sr2)
           | Br (sr1,sr2) -> (sr1,sr2)
         in
         let t =
           match dr,sr1,sr2 with
           | Some dr, Some sr, _ when dr = sr -> t
           | Some dr, _, Some sr when dr = sr -> t
           | _ -> 0
         in
         (0,t)
      | _ -> (0,0) 
    in

  if !disable_pipeline then
    let (f,d,e,m,w) = dp in
    (f+d+e+m+w,((0,0,0,0,0),None))
  else(
    let map4 f (a,b,c,d) = (f a, f b, f c, f d) in
    let (df,dd,de,dm,dw) = dp in
    let (f,d,e,m,w),dr = p in 
    let t1,t2 = get_dependencies dr instr in
    let f' = max (max (f + df) (d)) t1 in
    let (e,m,w,t,del) =
      if f' > e then
        let delay = f' - e in
        let (e,m,w,t2) = map4 ((+) delay) (e,m,w,t2) in
          (e,m,w,t2,delay)
      else (e,m,w,t2,0)
    in
    let d' = max (max (f'+ dd) (e)) t2 in
    let e' = max (d'+ de) (m) in
    let m' = max (e'+ dm) (w) in
    let w' = m'+ dw in
    let ticks, np = (w'-w+del, normalize f (f',d',e',m',w')) in
    let p' = 
      match instr with
      | Mem (dr',_,_) ->
         let (_,_,_,m',_) = np in
         (np,Some (instr,m'))
      | ND (Some dr,_,_,_) | ND (_,Some dr,_,_) ->
         let (_,_,e',_,_) = np in
         (np, Some (instr,e'))
      | _ -> (np, None)
    in  
    if !dbg_pipeline then (
      print_pipeline "Before Update" p;
      print_pipeline "After Correction" ((f,d,e,m,w),dr);
      printf "%d\n%!" ticks;
      print_pipeline "Update" p');      
    ticks,p')
          
let pipeline_join p1 p2 =
  let pipeline_join_int p1 p2 t =
      let (f1,d1,e1,m1,w1) = p1 in
      let (f2,d2,e2,m2,w2) = p2 in
      let f = max f1 f2 in
      let d = max (max d1 d2) t in
      let e = max (max e1 e2) t in
      let m = max (max m1 m2) t in
      let w = max (max w1 w2) t in
      (f,d,e,m,w)
  in
  if !disable_pipeline then
    ((0,0,0,0,0),None)
  else
    (let p' = 
       match p1,p2 with
       | (p1,None), (p2,None) -> (pipeline_join_int p1 p2 0, None)
       | (p1,Some dr), (p2,None) | (p1,None), (p2,Some dr) ->
          (pipeline_join_int p1 p2 0, Some dr)
       | (p1,Some (_,t1)), (p2,Some (_,t2)) ->
          let t = (max t1 t2) in
          (pipeline_join_int p1 p2 t, None)
     in
     if !dbg_pipeline then
       print_pipeline "Join" p';      
     p')
                        
let pipeline_init = ((0,0,0,0,0), None)
