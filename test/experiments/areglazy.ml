
open Aint32relint
open Printf

type registers = |R0 |R1 |R2 |R3 |R4 |R5 |R6 |R7
                 |R8 |R9 |R10|R11|R12|R13|R14|R15
                 |R16|R17|R18|R19|R20|R21|R22|R23
                 |R24|R25|R26|R27|R28|R29|R30|R31


type aregister = {
  reg1  : aint32 option;
  reg2  : aint32 option;
  reg3  : aint32 option;
  reg4  : aint32 option;
  reg5  : aint32 option;
  reg6  : aint32 option;
  reg7  : aint32 option;
  reg8  : aint32 option;
  reg9  : aint32 option;
  reg10 : aint32 option;
  reg11 : aint32 option;
  reg12 : aint32 option;
  reg13 : aint32 option;
  reg14 : aint32 option;
  reg15 : aint32 option;
  reg16 : aint32 option;
  reg17 : aint32 option;
  reg18 : aint32 option;
  reg19 : aint32 option;
  reg20 : aint32 option;
  reg21 : aint32 option;
  reg22 : aint32 option;
  reg23 : aint32 option;
  reg24 : aint32 option;
  reg25 : aint32 option;
  reg26 : aint32 option;
  reg27 : aint32 option;
  reg28 : aint32 option;
  reg29 : aint32 option;
  reg30 : aint32 option;
  reg31 : aint32 option;
  reg32 : aint32 option;

  (* List of abstract registers that are to be joined *)
  ajoins : aregister list;
}
    
let areg_init = {
  reg1  = None;
  reg2  = None;
  reg3  = None;
  reg4  = None;
  reg5  = None;
  reg6  = None;
  reg7  = None;
  reg8  = None;
  reg9  = None;
  reg10 = None;
  reg11 = None;
  reg12 = None;
  reg13 = None;
  reg14 = None;
  reg15 = None;
  reg16 = None;
  reg17 = None;
  reg18 = None;
  reg19 = None;
  reg20 = None;
  reg21 = None;
  reg22 = None;
  reg23 = None;
  reg24 = None;
  reg25 = None;
  reg26 = None;
  reg27 = None;
  reg28 = None;
  reg29 = None;
  reg30 = None;
  reg31 = None;
  reg32 = None;

  ajoins = [];
}

  
(** Returns the abstract value for a specific register
   r = register symbol, ps = program state
   returns the abstract value for the register r *)
let get_aregval r areg =
  match r with
  | R0  -> Some (aint32_const(0)) | R16 -> areg.reg16 
  | R1  -> areg.reg1            | R17 -> areg.reg17 
  | R2  -> areg.reg2            | R18 -> areg.reg18 
  | R3  -> areg.reg3            | R19 -> areg.reg19 
  | R4  -> areg.reg4            | R20 -> areg.reg20 
  | R5  -> areg.reg5            | R21 -> areg.reg21 
  | R6  -> areg.reg6            | R22 -> areg.reg22 
  | R7  -> areg.reg7            | R23 -> areg.reg23 
  | R8  -> areg.reg8            | R24 -> areg.reg24 
  | R9  -> areg.reg9            | R25 -> areg.reg25 
  | R10 -> areg.reg10           | R26 -> areg.reg26 
  | R11 -> areg.reg11           | R27 -> areg.reg27 
  | R12 -> areg.reg12           | R28 -> areg.reg28 
  | R13 -> areg.reg13           | R29 -> areg.reg29 
  | R14 -> areg.reg14           | R30 -> areg.reg30 
  | R15 -> areg.reg15           | R31 -> areg.reg31 


let set_aregval r v areg =
  match r with
  | R0   -> areg                   | R16 -> {areg with reg16 = v}
  | R1   -> {areg with reg1 = v}   | R17 -> {areg with reg17 = v}
  | R2   -> {areg with reg2 = v}   | R18 -> {areg with reg18 = v}
  | R3   -> {areg with reg3 = v}   | R19 -> {areg with reg19 = v}
  | R4   -> {areg with reg4 = v}   | R20 -> {areg with reg20 = v}
  | R5   -> {areg with reg5 = v}   | R21 -> {areg with reg21 = v}
  | R6   -> {areg with reg6 = v}   | R22 -> {areg with reg22 = v}
  | R7   -> {areg with reg7 = v}   | R23 -> {areg with reg23 = v}
  | R8   -> {areg with reg8 = v}   | R24 -> {areg with reg24 = v}
  | R9   -> {areg with reg9 = v}   | R25 -> {areg with reg25 = v}
  | R10  -> {areg with reg10 = v}  | R26 -> {areg with reg26 = v}
  | R11  -> {areg with reg11 = v}  | R27 -> {areg with reg27 = v}
  | R12  -> {areg with reg12 = v}  | R28 -> {areg with reg28 = v}
  | R13  -> {areg with reg13 = v}  | R29 -> {areg with reg29 = v}
  | R14  -> {areg with reg14 = v}  | R30 -> {areg with reg30 = v}
  | R15  -> {areg with reg15 = v}  | R31 -> {areg with reg31 = v}
    

let getreg r areg =        
  let rec getreg_internal r areg =      
    match get_aregval r areg with
    | Some v ->
      printf "** Getreg SOME\n";      
      v
    | None ->
      printf "** Getreg JOIN\n";
      (match areg.ajoins with
      | [] -> aint32_any        
      | a::rest ->
        List.fold_left (fun v1 areg -> aint32_join v1 (getreg_internal r areg))
                       (getreg_internal r a) rest)
  in
  let v = getreg_internal r areg in
  let areg' = set_aregval r (Some v) areg in
  (areg',v)


let setreg r v areg =
  set_aregval r (Some v) areg

let areg_join reglist =
  {areg_init with ajoins = reglist}
  
      




  
