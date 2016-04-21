
open Aint32Interval


type registers = |R0 |R1 |R2 |R3 |R4 |R5 |R6 |R7
                 |R8 |R9 |R10|R11|R12|R13|R14|R15
                 |R16|R17|R18|R19|R20|R21|R22|R23
                 |R24|R25|R26|R27|R28|R29|R30|R31



type aregister = {
  areg1  : aint32 option;
  areg2  : aint32 option;
  areg3  : aint32 option;
  areg4  : aint32 option;
  areg5  : aint32 option;
  areg6  : aint32 option;
  areg7  : aint32 option;
  areg8  : aint32 option;
  areg9  : aint32 option;
  areg10 : aint32 option;
  areg11 : aint32 option;
  areg12 : aint32 option;
  areg13 : aint32 option;
  areg14 : aint32 option;
  areg15 : aint32 option;
  areg16 : aint32 option;
  areg17 : aint32 option;
  areg18 : aint32 option;
  areg19 : aint32 option;
  areg20 : aint32 option;
  areg21 : aint32 option;
  areg22 : aint32 option;
  areg23 : aint32 option;
  areg24 : aint32 option;
  areg25 : aint32 option;
  areg26 : aint32 option;
  areg27 : aint32 option;
  areg28 : aint32 option;
  areg29 : aint32 option;
  areg30 : aint32 option;
  areg31 : aint32 option;
  areg32 : aint32 option;

  (* List of abstract registers that are to be joined *)
  ajoins : aregister list;
}
    
let areg_init = {
  areg1  = ARegNoVal;
  areg2  = ARegNoVal;
  areg3  = ARegNoVal;
  areg4  = ARegNoVal;
  areg5  = ARegNoVal;
  areg6  = ARegNoVal;
  areg7  = ARegNoVal;
  areg8  = ARegNoVal;
  areg9  = ARegNoVal;
  areg10 = ARegNoVal;
  areg11 = ARegNoVal;
  areg12 = ARegNoVal;
  areg13 = ARegNoVal;
  areg14 = ARegNoVal;
  areg15 = ARegNoVal;
  areg16 = ARegNoVal;
  areg17 = ARegNoVal;
  areg18 = ARegNoVal;
  areg19 = ARegNoVal;
  areg20 = ARegNoVal;
  areg21 = ARegNoVal;
  areg22 = ARegNoVal;
  areg23 = ARegNoVal;
  areg24 = ARegNoVal;
  areg25 = ARegNoVal;
  areg26 = ARegNoVal;
  areg27 = ARegNoVal;
  areg28 = ARegNoVal;
  areg29 = ARegNoVal;
  areg30 = ARegNoVal;
  areg31 = ARegNoVal;
  areg32 = ARegNoVal;

  ajoins = [];
}

let areg_join reglist =
  {areg_init with ajoins = reglist}
  
  
(** Returns the abstract value for a specific register
   r = register symbol, ps = program state
   returns the abstract value for the register r *)

let get_aregval r areg =
  match r with
  | R0  -> Some aint32_const(0) | R16 -> areg.areg16 
  | R1  -> areg.areg1           | R17 -> areg.areg17 
  | R2  -> areg.areg2           | R18 -> areg.areg18 
  | R3  -> areg.areg3           | R19 -> areg.areg19 
  | R4  -> areg.areg4           | R20 -> areg.areg20 
  | R5  -> areg.areg5           | R21 -> areg.areg21 
  | R6  -> areg.areg6           | R22 -> areg.areg22 
  | R7  -> areg.areg7           | R23 -> areg.areg23 
  | R8  -> areg.areg8           | R24 -> areg.areg24 
  | R9  -> areg.areg9           | R25 -> areg.areg25 
  | R10 -> areg.areg10          | R26 -> areg.areg26 
  | R11 -> areg.areg11          | R27 -> areg.areg27 
  | R12 -> areg.areg12          | R28 -> areg.areg28 
  | R13 -> areg.areg13          | R29 -> areg.areg29 
  | R14 -> areg.areg14          | R30 -> areg.areg30 
  | R15 -> areg.areg15          | R31 -> areg.areg31 


let set_aregval r v areg =
  match r with
  | R0   -> areg                    | R16 -> {areg with areg16 = v}
  | R1   -> {areg with areg1 = v}   | R17 -> {areg with areg17 = v}
  | R2   -> {areg with areg2 = v}   | R18 -> {areg with areg18 = v}
  | R3   -> {areg with areg3 = v}   | R19 -> {areg with areg19 = v}
  | R4   -> {areg with areg4 = v}   | R20 -> {areg with areg20 = v}
  | R5   -> {areg with areg5 = v}   | R21 -> {areg with areg21 = v}
  | R6   -> {areg with areg6 = v}   | R22 -> {areg with areg22 = v}
  | R7   -> {areg with areg7 = v}   | R23 -> {areg with areg23 = v}
  | R8   -> {areg with areg8 = v}   | R24 -> {areg with areg24 = v}
  | R9   -> {areg with areg9 = v}   | R25 -> {areg with areg25 = v}
  | R10  -> {areg with areg10 = v}  | R26 -> {areg with areg26 = v}
  | R11  -> {areg with areg11 = v}  | R27 -> {areg with areg27 = v}
  | R12  -> {areg with areg12 = v}  | R28 -> {areg with areg28 = v}
  | R13  -> {areg with areg13 = v}  | R29 -> {areg with areg29 = v}
  | R14  -> {areg with areg14 = v}  | R30 -> {areg with areg30 = v}
  | R15  -> {areg with areg15 = v}  | R31 -> {areg with areg31 = v}
    
    
and getreg r areg =      
  match get_aregval r areg with
  | Some v -> (v,areg)
  | None ->
    let v = 
      (match areg.ajoins with
      | [] ->
        let areg = set_aregval r aint32_any areg in
        (aint32_any, areg)        
      | l::ls -> List.fold_left
        let (v,ajoins') = (fun acc areg ->
           aint32_join acc (_reg r areg)
        ) l ls
        in
        
    in
      
        

      




  
