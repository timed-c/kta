
open MipsAst
open Ustring.Op
open Printf

exception Function_not_found of string

type machinestate = 
{
  registers : int32 array;
  data : bytes;
  bss : bytes;
  mutable pc : int;
  mutable hi : int32;
  mutable lo : int32;
}



(* Functions not implemented 
   - Trapping for addu and subu is not implemented.
   - Conditional trap 'teq' is implemented as NOP
*)
(* ---------------------------------------------------------------------*)
let rec step prog state opfunc opval is_a_delay_slot =
  let reg r = if r = 0 then Int32.zero else state.registers.(r) in
  let wreg r v = if r = 0 then () else state.registers.(r) <- v in
  let pc pc = state.pc <- state.pc + pc in
  let inst = prog.code.((state.pc - prog.text_addr)/4) in
  let thispc = state.pc in
  let op() = opfunc inst thispc prog state is_a_delay_slot opval in
  let branch dst = 
       pc 4;
       let (opval', term) = step prog state opfunc opval true in
       if term then (opval',term) 
       else(
         state.pc <- dst;
         opfunc inst thispc prog state false opval')
  in
  let sethi_lo v =
    state.lo <- Int64.to_int32 (Int64.shift_right_logical (Int64.shift_left v 32) 32);
    state.hi <- Int64.to_int32 (Int64.shift_right_logical v 32)
  in    
  let to64sig v =  Int64.of_int32 (reg v) in
  let to64unsig v =  Int64.shift_right_logical (Int64.shift_left 
                    (Int64.of_int32 (reg v)) 32) 32 in
  match inst  with 
  | MipsADD(rd,rs,rt) -> 
       wreg rd (Int32.add (reg rs) (reg rt)); pc 4; op()
  | MipsADDIU(rt,rs,imm) -> 
       wreg rt (Int32.add (reg rs) (Int32.of_int (imm land 0xffff))); pc 4; op()
  | MipsADDU(rd,rs,rt) -> 
       wreg rd (Int32.add (reg rs) (reg rt)); pc 4; op()
  | MipsBEQ(rs,rt,imm,s) ->
       if Int32.compare (reg rs) (reg rt) = 0 then branch (imm*4 + 4 + state.pc)
       else (pc 4; op())
  | MipsBLEZ(rs,imm,s) -> 
       if Int32.compare (reg rs) (Int32.zero) <= 0 then branch (imm*4 + 4 + state.pc)
       else (pc 4; op())
  | MipsBNE(rs,rt,imm,s) ->
       if Int32.compare (reg rs) (reg rt) <> 0 then branch (imm*4 + 4 + state.pc)
       else (pc 4; op())
  | MipsDIV(rs,rt) -> 
       state.lo <- Int64.to_int32 (Int64.div (to64sig rs) (to64sig rt));
       state.hi <- Int64.to_int32 (Int64.rem (to64sig rs) (to64sig rt));
       pc 4; op()
  | MipsDIVU(rs,rt) -> 
       state.lo <- Int64.to_int32 (Int64.div (to64unsig rs) (to64unsig rt));
       state.hi <- Int64.to_int32 (Int64.rem (to64unsig rs) (to64unsig rt));
       pc 4; op()
  | MipsJR(rs) -> 
       branch (Int32.to_int state.registers.(rs))
  | MipsMFHI(rd) -> 
       wreg rd (state.hi); pc 4; op()
  | MipsMFLO(rd) -> 
       wreg rd (state.lo); pc 4; op()
  | MipsMTHI(rs) -> 
       state.hi <- reg rs; pc 4; op()
  | MipsMTLO(rs) -> 
       state.lo <- reg rs; pc 4; op()
  | MipsMUL(rd,rs,rt) -> 
       wreg rd (Int32.mul (reg rs) (reg rt)); pc 4; op()
  | MipsMULT(rs,rt) -> 
       sethi_lo (Int64.mul (Int64.of_int32 (reg rs)) (Int64.of_int32 (reg rt)));
       pc 4; op()
  | MipsMULTU(rs,rt) -> 
       sethi_lo (Int64.mul (to64unsig rs) (to64unsig rt));
       pc 4; op()
  | MipsSLL(rd,rt,shamt) -> 
       wreg rd (Int32.shift_left (reg rt) shamt); pc 4; op() 
  | MipsSLT(rd,rs,rt) ->
       wreg rd (Int32.shift_right_logical (Int32.sub (reg rs) (reg rt)) 31); 
       pc 4; op() 
  | MipsSUB(rd,rs,rt) -> 
       wreg rd (Int32.sub (reg rs) (reg rt)); pc 4; op()
  | MipsSUBU(rd,rs,rt) -> 
       wreg rd (Int32.sub (reg rs) (reg rt)); pc 4; op()
  | MipsTEQ(rs,rt,code) -> 
       pc 4; op()
  | MipsXOR(rd,rs,rt) -> 
       wreg rd (Int32.logxor (reg rs) (reg rt)); pc 4; op()
  | _ -> failwith ("Unknown instruction: " ^
                    Ustring.to_utf8 (MipsUtils.pprint_inst inst))
   



(* ---------------------------------------------------------------------*)
(* TODO: Update with correct handling for a 5 stage pipeline *)
let cycle_count inst pc prog state is_a_delay_slot count =
  (count + 1, false )


(* ---------------------------------------------------------------------*)
let debug_print inst pc prog state is_a_delay_slot (acc,prev_regfile) =
  let (dreg,sreg1,sreg2) = 
    match inst with
    | MipsADD(rd,rs,rt) -> (rd,rs,rt)
    | MipsADDI(rt,rs,_) -> (rt,rs,0)
    | MipsADDIU(rt,rs,_) -> (rt,rs,0)
    | MipsADDU(rd,rs,rt) -> (rd,rs,rt)   
    | MipsAND(rd,rs,rt) -> (rd,rs,rt)
    | MipsANDI(rt,rs,_) -> (rt,rs,0)
    | MipsBEQ(rs,rt,_,_) -> (0,rs,rt)
    | MipsBLEZ(rs,_,_) -> (0,rs,0)
    | MipsBNE(rs,rt,_,_) -> (0,rs,rt)
    | MipsJALR(rs) -> (0,rs,0)
    | MipsJR(rs) -> (0,rs,0)
    | MipsJ(_) -> (0,0,0)
    | MipsJAL(_) -> (0,0,0)
    | MipsLB(rt,imm,rs) -> (rt,rs,0)
    | MipsLBU(rt,_,rs) -> (rt,rs,0)
    | MipsLUI(rt,_) -> (rt,0,0)
    | MipsLW(rt,_,rs) -> (rt,rs,0)
    | MipsMFHI(rd) -> (rd,0,0)
    | MipsMFLO(rd) -> (rd,0,0)
    | MipsMTHI(rs) -> (0,rs,0)
    | MipsMTLO(rs) -> (0,rs,0)
    | MipsMUL(rd,rs,rt) -> (rd,rs,rt)
    | MipsMULT(rs,rt) -> (0,rs,rt)
    | MipsMULTU(rs,rt) -> (0,rs,rt)
    | MipsDIV(rs,rt) -> (0,rs,rt)
    | MipsDIVU(rs,rt) -> (0,rs,rt)
    | MipsNOR(rd,rs,rt) -> (rd,rs,rt)
    | MipsOR(rd,rs,rt) -> (rd,rs,rt) 
    | MipsORI(rt,rs,_) -> (rt,rs,0)
    | MipsSLT(rd,rs,rt) -> (rd,rs,rt) 
    | MipsSLTU(rd,rs,rt) -> (rd,rs,rt)  
    | MipsSLTI(rt,rs,_) -> (rt,rs,0)   
    | MipsSLTIU(rt,rs,_) -> (rt,rs,0) 
    | MipsSLL(rd,rt,_) -> (rd,rt,0)
    | MipsSLLV(rd,rs,rt) -> (rd,rs,rt)
    | MipsSRA(rd,rt,_) -> (rd,rt,0)    
    | MipsSRL(rd,rt,_) -> (rd,rt,0)  
    | MipsSRLV(rd,rs,rt) -> (rd,rs,rt)  
    | MipsSB(rt,_,rs) -> (rt,rs,0) 
    | MipsSW(rt,_,rs) -> (rt,rs,0)    
    | MipsSUB(rd,rs,rt) -> (rd,rs,rt) 
    | MipsSUBU(rd,rs,rt) -> (rd,rs,rt)  
    | MipsTEQ(rs,rt,code) -> (0,rs,rt)
    | MipsXOR(rd,rs,rt) -> (rd,rs,rt)   
    | MipsXORI(rt,rs,_) -> (rt,rs,0)   
    | MipsUnknown(_) -> (0,0,0)
  in
  let pad_right str int =
    Ustring.concat (us"\n") 
      (match List.rev (Ustring.split str (us"\n")) with
        | x::xs -> List.rev ((Ustring.spaces_after x 36)::xs)
        | [] -> [])
  in
  let preg reg sign regfile = 
    if reg = 0 then us"" else
      Ustring.spaces_after (MipsUtils.pprint_reg reg ^. 
      us(sprintf "%s%d" sign (Int32.to_int regfile.(reg)))) 14
  in
  let str = 
    acc ^. pad_right (MipsUtils.pprint_inst_ext inst prog pc true) 36 ^. 
    us"   " ^.
    preg dreg " := " state.registers ^.
    preg sreg1 " = " prev_regfile ^.
    preg sreg2 " = " prev_regfile ^.
    us"\n" ^.    
    if pc + 4 <> state.pc then us"\n" else us""    
    
  in
   ((str,Array.copy state.registers),false)        





(* ---------------------------------------------------------------------*)
let pprint_state state =
  let p_no no = let x = MipsUtils.pprint_reg no in 
                if (Ustring.length x) < 3 then x ^. us" " else x in
  let p_reg r = us(sprintf " 0x%08x      " (0xffffffff land (Int32.to_int r))) in
  let rec regs no str =
    if no >= 8 then str else
      regs (no+1) (str ^.
      p_no no      ^. p_reg state.registers.(no) ^.
      p_no (no+8)  ^. p_reg state.registers.(no+8) ^.
      p_no (no+16) ^. p_reg state.registers.(no+16) ^.
      p_no (no+24) ^. p_reg state.registers.(no+24) ^. us"\n")
  in
    us (sprintf "PC  0x%08x \n" state.pc) ^.
    regs 0 (us"")

  

(* ---------------------------------------------------------------------*)
let init prog func args = 
  let state = {
    registers = Array.make 32 Int32.zero;
    data = Bytes.copy prog.data_sec;
    bss = Bytes.make prog.bss_size (char_of_int 0);
    pc = 0;
    hi = Int32.zero;
    lo = Int32.zero;
  } 
  in 

  (* Set the PC address to the address given by the func parameter *)
  (try state.pc <- List.assoc func prog.symbols 
  with
    Not_found -> raise (Function_not_found func));   

  (* Set return address to 0. Used for checking termination. *)
  state.registers.(31) <- Int32.zero; 
  
  (* Set the arguments. For now, max 4 arguments. *)
  List.iteri (fun i x ->
    if i < 4 then state.registers.(i + 4) <- x else () 
  ) args;
  
  (* Return the state *)
  state




(* ---------------------------------------------------------------------*)
(* Evaluate a program *)
let eval prog state opfunc opinit = 
  
  (* Call the step function *)
  let rec multistep opval =
     let (opval',terminate) = step prog state opfunc opval false in
     if state.pc = 0 || terminate then opval'
     else multistep opval'
  in
     let opval = multistep opinit in
     (state,opval)










