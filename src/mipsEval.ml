
open MipsAst

type machinestate = 
{
  registers : int32 array;
  data : bytes;
  bss : bytes;
  time : int ref;
  pc : int ref;
}


(*
let rec step prog code state =
  let reg r = state.registers.(r) in
  let wreg r v = state.registers.(r) <- v in
  match code with 
  | MipsADD(rd,rs,rt)::next -> wreg rd (Int32.add (reg rt) (reg rt))
  | MipsJR(rs)::next -> 
      let _ = step prog next state in
      prog.code 
  | _ -> failwith "Unknown instruction."
*)

let eval prog func = 
{
  registers = Array.make 32 Int32.zero;
  data = Bytes.empty;
  bss = Bytes.empty;
  time = ref 0;
  pc = ref 0;
}



