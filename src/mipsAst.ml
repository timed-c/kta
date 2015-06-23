


type rd = int
type rs = int
type rt = int
type code = int (* 10 bits of information *)
type imm = int (* Stored sign extended in the AST *)
type addr = int
type shamt = int

type inst =
  | MipsADD     of rd * rs  * rt
  | MipsADDI    of rt * rs  * imm
  | MipsADDIU   of rt * rs  * imm
  | MipsADDU    of rd * rs  * rt
  | MipsAND     of rd * rs  * rt
  | MipsANDI    of rt * rs  * imm
  | MipsBEQ     of rs * rt  * imm * string
  | MipsBLEZ    of rs * imm * string
  | MipsBNE     of rs * rt  * imm * string
  | MipsDIV     of rs * rt
  | MipsDIVU    of rs * rt
  | MipsJALR    of rs
  | MipsJR      of rs
  | MipsJ       of addr
  | MipsJAL     of addr
  | MipsLB      of rt * imm * rs
  | MipsLBU     of rt * imm * rs
  | MipsLUI     of rt * imm
  | MipsLW      of rt * imm * rs
  | MipsMFHI    of rd 
  | MipsMFLO    of rd 
  | MipsMTHI    of rs 
  | MipsMTLO    of rs 
  | MipsMUL     of rd * rs  * rt
  | MipsMULT    of rs * rt
  | MipsMULTU   of rs * rt
  | MipsNOR     of rd * rs  * rt
  | MipsOR      of rd * rs  * rt
  | MipsORI     of rt * rs  * imm
  | MipsSLT     of rd * rs  * rt
  | MipsSLTU    of rd * rs  * rt
  | MipsSLTI    of rt * rs  * imm
  | MipsSLTIU   of rt * rs  * imm
  | MipsSLL     of rd * rt  * shamt
  | MipsSLLV    of rd * rt  * rs
  | MipsSRA     of rd * rt  * shamt
  | MipsSRL     of rd * rt  * shamt
  | MipsSRLV    of rd * rt  * rs
  | MipsSB      of rt * imm * rs
  | MipsSW      of rt * imm * rs
  | MipsSUB     of rd * rs  * rt
  | MipsSUBU    of rd * rs  * rt
  | MipsTEQ     of rs * rt  * code
  | MipsXOR     of rd * rs  * rt
  | MipsXORI    of rt * rs  * imm
  | MipsUnknown of int
      
module Sym2Addr = Map.Make(String)
type sym2addr_map = int Sym2Addr.t
module Addr2Sym = Map.Make(Utils.Int)
type addr2sym_map = string Addr2Sym.t

(** A MIPS program object that contains all information needed to 
    execute the object. *)
type program =
{
  filename : string;                      (* Name of the original MIPS binary file *)
  symbols : (string * int) list;          (* Symbol table *)
  sym2addr : sym2addr_map;                (* Fast lookup using module Sym2Addr *) 
  addr2sym : addr2sym_map;                (* Fast lookup using module Addr2Sym *) 
  sections : (string * (int * int)) list; (* Section info. Names, size, address. *)
  text_sec : bytes;                       (* Text section (code) *)
  data_sec : bytes;                       (* Data section *)
  text_addr : int;                        (* Virtual address to the .text section *)
  text_size : int;                        (* Size in bytes of the .text section *)
  data_addr : int;                        (* Virtual address to the .sdata section *)
  data_size : int;                        (* Size in bytes of the .sdata section *)
  bss_addr : int;                         (* Virtual address to the .sbss section *)
  bss_size : int;                         (* Size in bytes of the .sbss section *)
  gp : int;                               (* Initial value for the global pointer, gp *)
  code : inst array;                      (* Array of decoded instructions *)
}


let reg_0 = 0
let reg_at = 1
let reg_v0 = 2
let reg_v1 = 3
let reg_a0 = 4
let reg_a1 = 5
let reg_a2 = 6
let reg_a3 = 7
let reg_t0 = 8
let reg_t1 = 9
let reg_t2 = 10
let reg_t3 = 11
let reg_t4 = 12
let reg_t5 = 13
let reg_t6 = 14
let reg_t7 = 15
let reg_s0 = 16
let reg_s1 = 17
let reg_s2 = 18
let reg_s3 = 19
let reg_s4 = 20
let reg_s5 = 21
let reg_s6 = 22
let reg_s7 = 23
let reg_t8 = 24
let reg_t9 = 25
let reg_k0 = 26
let reg_k1 = 27
let reg_gp = 28
let reg_sp = 29
let reg_fp = 30
let reg_ra = 31


