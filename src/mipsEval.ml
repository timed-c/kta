
open MipsAst
open Ustring.Op
open Printf

exception Function_not_found of string

type machinestate = 
{
  registers : int32 array;
  data : bytes;
  bss : bytes;
  mutable ticks : int;
  mutable pc : int;
}



(* Functions not implemented 
   - Trapping for addu and subu is not implemented.
*)
(* ---------------------------------------------------------------------*)
let rec step prog state opfunc opval delay_slot =
  let reg r = state.registers.(r) in
  let wreg r v = state.registers.(r) <- v in
  let tick t = state.ticks <- state.ticks + t in
  let pc pc = state.pc <- state.pc + pc in
  let inst = prog.code.((state.pc - prog.text_addr)/4) in
  let op() = opfunc inst delay_slot state opval in
  match inst  with 
  | MipsADD(rd,rs,rt) -> 
       wreg rd (Int32.add (reg rs) (reg rt)); 
       tick 1; pc 4; op()
  | MipsADDIU(rt,rs,imm) -> 
       wreg rt (Int32.add (reg rs) (Int32.of_int (imm land 0xff))); 
       tick 1; pc 4; op()
  | MipsADDU(rd,rs,rt) -> 
       wreg rd (Int32.add (reg rs) (reg rt)); 
       tick 1; pc 4; op()
  | MipsJR(rs) -> 
       state.pc <- state.pc + 4; 
       let (opval', term) = step prog state opfunc opval true in
       if term then (opval',term) 
       else(
         state.pc <- Int32.to_int state.registers.(rs);
         tick 1;
         opfunc inst false state opval')
  | MipsSLL(rd,rt,shamt) -> 
       wreg rd (Int32.shift_left (reg rt) shamt); 
       tick 1; pc 4; op() 
  | MipsSUB(rd,rs,rt) -> 
       wreg rd (Int32.sub (reg rs) (reg rt)); 
       tick 1; pc 4; op()
  | MipsSUBU(rd,rs,rt) -> 
       wreg rd (Int32.sub (reg rs) (reg rt)); 
       tick 1; pc 4; op()
  | _ -> failwith "Unknown instruction."
   
  



(* ---------------------------------------------------------------------*)
let cycle_count inst delay_slot state count =
  ((if delay_slot then count + 1 else count + 1), false)



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
    us (sprintf "PC  0x%08x      ticks %d\n" state.pc state.ticks) ^.
    regs 0 (us"")

  

(* ---------------------------------------------------------------------*)
let init prog = 
{
  registers = Array.make 32 Int32.zero;
  data = Bytes.copy prog.data_sec;
  bss = Bytes.make prog.bss_size (char_of_int 0);
  ticks = 0;
  pc = 0;
}



(* ---------------------------------------------------------------------*)
(* Evaluate a program *)
let eval prog func args opfunc opinit = 
  (* Create the initial state. Init all registers to zero *)
  let state = init prog in

  (* Set the PC address to the address given by the func parameter *)
  (try state.pc <- List.assoc func prog.symbols 
  with
    Not_found -> raise (Function_not_found func));   

  (* Set return address to 0. Used for checking termination. *)
  let ra_reg = 31 in
  state.registers.(ra_reg) <- Int32.zero; 
  
  (* Set the arguments. For now, max 4 arguments. *)
  List.iteri (fun i x ->
    if i < 4 then state.registers.(i + 4) <- x else () 
  ) args;
  
  (* Call the step function *)
  let rec multistep opval =
     let (opval',terminate) = step prog state opfunc opval false in
     if state.pc = 0 || terminate then opval'
     else multistep opval'
  in
     let opval = multistep opinit in
     (state,opval)










