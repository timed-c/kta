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

type dependency_t = instruction_type * int * int (* registers * int  *)

type apipeline = apipeline_t * ( dependency_t option * dependency_t option * dependency_t option)

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
let normalize_dr pf dr = 
	match dr with 
	| None -> None
	| Some (n, ts,te) -> Some (n, ts - pf, te - pf)

  
let pipeline_update instr p dp =
    let get_dependencies dr instr = 
      match dr,instr with
    (* | (Mem (dr,_,_),t),Br(sr1,sr2) -> *)
      | (Some (dr,_,t),_,_), Br(sr1,sr2) ->
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
      | (Some (Mem (dr,_,_),_,t),_,_),instr ->
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
    (e + (f-1) + (m-1),((0,0,0,0,0),(None,None,None)))
  else(
    let map6 f (a,b,c,d,e,g) = (f a, f b, f c, f d, f e, f g) in
    let (df,dd,de,dm,dw) = dp in
    let (f,d,e,m,w),dr = p in 
    let t1,t2 = get_dependencies dr instr in
    let f' = max (max (f + df) (d)) t1 in
    let (f',d,e,m,w,t1,t2,del) =
      if df > 1 then 
	match dr with
	| (_,_,Some (Mem _,ts,te)) when (te - ts > 1 && ts < (f+df) && f <= ts) ->
			let delay = f+df -ts in
          		let (d,e,m,w,t1,t2) = map6 ((+) delay) (d,e,m,w,t1,t2) in
             		(max (f+df) t1,d,e,m,w,t1,t2,delay)
	| (_,Some (Mem _,ts,te),_) when (te - ts > 1 && ts < (f+df) && f <= ts) ->
			let delay = f+df -ts in
          		let (_,e,m,w,t1,t2) = map6 ((+) delay) (d,e,m,w,t1,t2) in
             		(max (f+df) t1,d,e,m,w,t1,t2,delay)		
	| (Some (Mem _,ts,te),_,_) when (te - ts > 1 && ts< (f+df) && f <= ts) ->
			let delay = f+df -ts in
          		let (d,e,m,w,t1,t2) = map6 ((+) delay) (0,e,m,w,t1,t2) in
             		(max (f+df) t1,d,e,m,w,t1,t2,delay)
	| _ -> (f',d,e,m,w,t1,t2,0)
      else (f',d,e,m,w,t1,t2,0)
    in
    let d' = max (max (f'+ dd) (e)) t2 in
    let e' = max (d'+ de) (m) in
    let m' = max (e'+ dm) (w) in
    let w' = m'+ dw in
    let ticks, np = (w'-w+del, normalize f (f',d',e',m',w')) in
    let p' = 
      let (dr1,dr2,dr3) = dr in
      let (dr1,dr2) = (normalize_dr f dr1, normalize_dr f dr2) in
      match instr with
      | Mem (dr',_,_) ->
         let (_,_,e',m',_) = np in
         (np,(Some (instr,e',m'),dr1,dr2))
      | ND (Some dr',_,_,_) | ND (_,Some dr',_,_) ->
         let (_,d',e',_,_) = np in
         (np, (Some (instr,d',e'),dr1,dr2))
      | _ -> (np, (None,dr1,dr2))
    in  
    if (!dbg_pipeline) then (
	printf "TICKS %d delay %d \n%!" ticks del;
      	print_pipeline "Before Update" p;
      	print_pipeline "After Correction" ((f,d,e,m,w),dr);
      	print_pipeline "Update" p';);      
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
  let rec max_dr m l = 
	match l with
	| Some(_,_,t)::ls -> max_dr (max m t) ls
	| None::ls -> max_dr m ls
	| [] -> m
  in
  if !disable_pipeline then
    ((0,0,0,0,0),(None,None,None))
  else
    (let p' = 
       match p1,p2 with
       | (p1,(None,None,None)), (p2,(None,None,None)) -> (pipeline_join_int p1 p2 0, (None,None,None))
       (*| (p1,Some dr), (p2,None) | (p1,None), (p2,Some dr) ->
          (pipeline_join_int p1 p2 0, Some dr)*)
       | (p1,(dr1,dr2,dr3)), (p2,(dr4,dr5,dr6)) -> 
		let t = max_dr 0 [dr1;dr2;dr3;dr4;dr5;dr6] in
          	(pipeline_join_int p1 p2 t, (None,None,None))
			
       (*| (p1,Some (_,t1)), (p2,Some (_,t2)) ->
          let t = (max t1 t2) in
          (pipeline_join_int p1 p2 t, None)
        *)
     in
     if !dbg_pipeline then
       print_pipeline "Join" p';      
     p')
                        
let pipeline_init = ((0,0,0,0,0), (None,None,None))
