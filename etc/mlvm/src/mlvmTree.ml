
open MlvmAst
open Ustring.Op


type tree = 
| TExp of llInst * tree list 
| TId of llId 
| TConst of llConst  

type forest = tree list

let make instlst used_outside = 
  let id_use_count = MlvmUtils.count_ids_uses instlst in
  let (rev_forest,_) = List.fold_left (fun (acc_forest,tmptree) inst ->
    let (rev_childlist,tmptree') = List.fold_left (fun (childlist,tmptree) exp -> 
      match exp with
      | ExpId(LocalId(id),_) -> (
        try ((List.assoc id tmptree)::childlist,List.remove_assoc id tmptree)
        with Not_found -> (TId(LocalId(id))::childlist,tmptree))
      | ExpId(GlobalId(id),_) -> (TId(GlobalId(id))::childlist, tmptree)
      | ExpConst(c) -> (TConst(c)::childlist,tmptree)        
      | ExpConstExpr(_) -> failwith "Not implemented"
    ) ([],tmptree) (MlvmUtils.using_explst_in_instruction inst) in 
    let exp = TExp(inst,List.rev rev_childlist) in
    match MlvmUtils.defining_id_in_instruction inst with
    | Some(id) when SidSet.mem id used_outside || List.assoc id id_use_count > 1
        -> (exp::acc_forest, tmptree')
    | Some(id) -> (acc_forest,(id,exp)::tmptree')
    | None -> (exp::acc_forest, tmptree')
  ) ([],[]) instlst 
  in
    List.rev rev_forest


    

