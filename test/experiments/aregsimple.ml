
open Aint32Interval


type aregister = {
                 reg8  : aint32; reg16 : aint32; reg24 : aint32;
  reg1 : aint32; reg9  : aint32; reg17 : aint32; reg25 : aint32;
  reg2 : aint32; reg10 : aint32; reg18 : aint32; reg26 : aint32;
  reg3 : aint32; reg11 : aint32; reg19 : aint32; reg27 : aint32;
  reg4 : aint32; reg12 : aint32; reg20 : aint32; reg28 : aint32;
  reg5 : aint32; reg13 : aint32; reg21 : aint32; reg29 : aint32;
  reg6 : aint32; reg14 : aint32; reg22 : aint32; reg30 : aint32;
  reg7 : aint32; reg15 : aint32; reg23 : aint32; reg31 : aint32;
}
  

type registers = |R0 |R1 |R2 |R3 |R4 |R5 |R6 |R7
                 |R8 |R9 |R10|R11|R12|R13|R14|R15
                 |R16|R17|R18|R19|R20|R21|R22|R23
                 |R24|R25|R26|R27|R28|R29|R30|R31




let areg_init =
  {
                         reg16 = aint32_any;
    reg1  = aint32_any;  reg17 = aint32_any;
    reg2  = aint32_any;  reg18 = aint32_any;
    reg3  = aint32_any;  reg19 = aint32_any;
    reg4  = aint32_any;  reg20 = aint32_any;
    reg5  = aint32_any;  reg21 = aint32_any;
    reg6  = aint32_any;  reg22 = aint32_any;
    reg7  = aint32_any;  reg23 = aint32_any;
    reg8  = aint32_any;  reg24 = aint32_any;
    reg9  = aint32_any;  reg25 = aint32_any;
    reg10 = aint32_any;  reg26 = aint32_any;
    reg11 = aint32_any;  reg27 = aint32_any;
    reg12 = aint32_any;  reg28 = aint32_any;
    reg13 = aint32_any;  reg29 = aint32_any;
    reg14 = aint32_any;  reg30 = aint32_any;
    reg15 = aint32_any;  reg31 = aint32_any;    
}

     

let reg r areg =
  match r with
  | R0  -> aint32_const(0) | R16 -> areg.reg16 
  | R1  -> areg.reg1         | R17 -> areg.reg17 
  | R2  -> areg.reg2         | R18 -> areg.reg18 
  | R3  -> areg.reg3         | R19 -> areg.reg19 
  | R4  -> areg.reg4         | R20 -> areg.reg20 
  | R5  -> areg.reg5         | R21 -> areg.reg21 
  | R6  -> areg.reg6         | R22 -> areg.reg22 
  | R7  -> areg.reg7         | R23 -> areg.reg23 
  | R8  -> areg.reg8         | R24 -> areg.reg24 
  | R9  -> areg.reg9         | R25 -> areg.reg25 
  | R10 -> areg.reg10        | R26 -> areg.reg26 
  | R11 -> areg.reg11        | R27 -> areg.reg27 
  | R12 -> areg.reg12        | R28 -> areg.reg28 
  | R13 -> areg.reg13        | R29 -> areg.reg29 
  | R14 -> areg.reg14        | R30 -> areg.reg30 
  | R15 -> areg.reg15        | R31 -> areg.reg31 

(** Sets the value of a register.
    r = register symbol, v = abstract value to be set
    areg = program state
    returns the new abstract program state. *)
let setreg r v areg =
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

    

      
let areg_two_join areg1 areg2 =
  {reg1  = aint32_join areg1.reg1  areg2.reg1;  reg17 = aint32_join areg1.reg17 areg2.reg17;
   reg2  = aint32_join areg1.reg2  areg2.reg2;  reg18 = aint32_join areg1.reg18 areg2.reg18;
   reg3  = aint32_join areg1.reg3  areg2.reg3;  reg19 = aint32_join areg1.reg19 areg2.reg19;
   reg4  = aint32_join areg1.reg4  areg2.reg4;  reg20 = aint32_join areg1.reg20 areg2.reg20;
   reg5  = aint32_join areg1.reg5  areg2.reg5;  reg21 = aint32_join areg1.reg21 areg2.reg21;
   reg6  = aint32_join areg1.reg6  areg2.reg6;  reg22 = aint32_join areg1.reg22 areg2.reg22;
   reg7  = aint32_join areg1.reg7  areg2.reg7;  reg23 = aint32_join areg1.reg23 areg2.reg23;
   reg8  = aint32_join areg1.reg8  areg2.reg8;  reg24 = aint32_join areg1.reg24 areg2.reg24;
   reg9  = aint32_join areg1.reg9  areg2.reg9;  reg25 = aint32_join areg1.reg25 areg2.reg25;
   reg10 = aint32_join areg1.reg10 areg2.reg10; reg26 = aint32_join areg1.reg26 areg2.reg26;
   reg11 = aint32_join areg1.reg11 areg2.reg11; reg27 = aint32_join areg1.reg27 areg2.reg27;
   reg12 = aint32_join areg1.reg12 areg2.reg12; reg28 = aint32_join areg1.reg28 areg2.reg28;
   reg13 = aint32_join areg1.reg13 areg2.reg13; reg29 = aint32_join areg1.reg29 areg2.reg29;
   reg14 = aint32_join areg1.reg14 areg2.reg14; reg30 = aint32_join areg1.reg30 areg2.reg30;
   reg15 = aint32_join areg1.reg15 areg2.reg15; reg31 = aint32_join areg1.reg31 areg2.reg31;
   reg16 = aint32_join areg1.reg16 areg2.reg16;
}


let areg_join lst =
  match lst with
  | [] -> areg_init
  | l::ls -> List.fold_left areg_two_join l ls
    
  












    
