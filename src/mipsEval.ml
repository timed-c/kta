
open MipsAst
open Ustring.Op
open Printf

type machinestate = 
{
  registers : int32 array;
  data : bytes;
  bss : bytes;
  mutable ticks : int;
  mutable pc : int;
}

let rec step prog state =
  let reg r = state.registers.(r) in
  let wreg r v = state.registers.(r) <- v in
  let tickpc() = state.pc <- state.pc + 4; state.ticks <- state.ticks + 1 in
  match prog.code.((state.pc - prog.text_addr)/4) with 
  | MipsADD(rd,rs,rt) -> wreg rd (Int32.add (reg rt) (reg rt)); tickpc()
(*  | MipsADDIU(rt,rs,imm) -> *)
  | MipsJR(rs) -> state.pc <- state.pc + 4; step prog state; state.pc <- rs        
  | _ -> failwith "Unknown instruction."


let pprint_state state =
  let p_no no = let x = MipsUtils.pprint_reg no in 
                if (Ustring.length x) < 3 then x ^. us" " else x in
  let p_reg r = us(sprintf " 0x%08x      " (Int32.to_int r)) in
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
  

let init prog = 
{
  registers = Array.make 32 Int32.zero;
  data = Bytes.copy prog.data_sec;
  bss = Bytes.make prog.bss_size (char_of_int 0);
  ticks = 0;
  pc = 0;
}


let eval prog func timeout = 
  let state = init prog in
  let ra_reg = 31 in
  state.registers.(ra_reg) <- Int32.zero; 
  let rec multistep() =
     step prog state;
     if state.pc = 0 || state.ticks >= timeout then ()
     else multistep()
  in
   (* multistep(); *)
    state










