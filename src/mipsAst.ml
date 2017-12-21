
open Ustring.Op


type rd = int
type rs = int
type rt = int
type code = int (* 10 bits of information *)
type imm = int (* Stored sign extended in the AST *)
type addr = int
type shamt = int
type pos = int
type size = int
              
type inst =
  | MipsADD     of rd * rs  * rt
  | MipsADDI    of rt * rs  * imm
  | MipsADDIU   of rt * rs  * imm
  | MipsADDU    of rd * rs  * rt
  | MipsAND     of rd * rs  * rt
  | MipsANDI    of rt * rs  * imm
  | MipsBEQ     of rs * rt  * imm * string
  | MipsB       of imm * string
  | MipsBEQL    of rs * rt  * imm * string
  | MipsBGEZ    of rs * imm * string
  | MipsBGEZL   of rs * imm * string
  | MipsBGTZ    of rs * imm * string
  | MipsBGTZL   of rs * imm * string
  | MipsBLEZ    of rs * imm * string
  | MipsBLEZL   of rs * imm * string
  | MipsBLTZ    of rs * imm * string
  | MipsBLTZL   of rs * imm * string
  | MipsBNE     of rs * rt  * imm * string
  | MipsBNEL    of rs * rt  * imm * string
  | MipsCLZ     of rd * rs
  | MipsDIV     of rs * rt
  | MipsDIVU    of rs * rt
  | MipsEXT     of rt * rs  * pos * size
  | MipsINS     of rt * rs  * pos * size
  | MipsJALR    of rs
  | MipsJR      of rs
  | MipsJ       of addr * string
  | MipsJAL     of addr * string
  | MipsLB      of rt * imm * rs
  | MipsLBU     of rt * imm * rs
  | MipsLH      of rt * imm * rs
  | MipsLHU     of rt * imm * rs
  | MipsLUI     of rt * imm
  | MipsLW      of rt * imm * rs
  | MipsLWL     of rt * imm * rs
  | MipsLWR     of rt * imm * rs
  | MipsMADD    of rs * rt
  | MipsMFHI    of rd 
  | MipsMFLO    of rd 
  | MipsMOVN    of rd * rs  * rt
  | MipsMOVZ    of rd * rs  * rt
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
  | MipsSRAV    of rd * rt  * rs
  | MipsSRL     of rd * rt  * shamt
  | MipsSRLV    of rd * rt  * rs
  | MipsSB      of rt * imm * rs
  | MipsSH      of rt * imm * rs
  | MipsSW      of rt * imm * rs
  | MipsSWL     of rt * imm * rs
  | MipsSUB     of rd * rs  * rt
  | MipsSUBU    of rd * rs  * rt
  | MipsTEQ     of rs * rt
  | MipsXOR     of rd * rs  * rt
  | MipsXORI    of rt * rs  * imm
  | MipsUnknown of int
      
module Sym2Addr = Map.Make(String)
type sym2addr_map = int Sym2Addr.t
module Addr2Sym = Map.Make(Utils.Int)
type addr2sym_map = string Addr2Sym.t

type section = 
{ 
  d    : bytes;
  addr : int;
  size : int;
}

(** A MIPS program object that contains all information needed to 
    execute the object. *)
type program =
{
  filename   : string;                      (* Name of the original MIPS binary file *)
  symbols    : (string * int) list;         (* Symbol table *)
  sym2addr   : sym2addr_map;                (* Fast lookup using module Sym2Addr *) 
  addr2sym   : addr2sym_map;                (* Fast lookup using module Addr2Sym *) 
  sections   : (string * (int * int)) list; (* Section info. Names, size, address. *)
  text_sec   : section;                     (* .text section (code) *)
  data_sec   : section;                     (* .data section *)
  sdata_sec  : section;                     (* .sdata (small data) section *)
  bss_sec    : section;                     (* .bss section. No data is allocated. *)
  sbss_sec   : section;                     (* .sbss (small bss) section *)
  rodata_sec : section;                     (* .rodata section *)
  stack_sec  : section;                     (* stack section (not part of ELF) *)
  gp         : int;                         (* Initial value for the global pointer, gp *)
  sp         : int;                         (* Initial value for the stack pointer, sp *)
  code       : inst array;                  (* Array of decoded instructions *)
}

(* The basic block ID is using the Ustring identifier for performance reason *)
type bblockid = string
  
type exittype =
| ExitTypeNext     of bblockid            (* Next block, no actual jump instruction *)
| ExitTypeBranch   of bblockid * bblockid (* true branch, false (branch not taken, 
                                             not a likely inst *)
| ExitTypeBrLikely of bblockid * bblockid (* true branch, false (branch not taken), 
                                             Used for likely instructions *)
| ExitTypeJump     of bblockid            (* Unconditional jump *)
| ExitTypeCall     of bblockid * bblockid (* Function call and next id *)
| ExitTypeReturn                          (* The type of the exit node *)
  
type bblock =
{   
  block_addr : int;           (* Memory address of the first instruction in the block *)
  block_code : inst list;     (* Assembly code instructions of the bloc *)
  block_exit : exittype;      (* Exist variants with string block ids to the next nodes *)
  block_dist : int;           (* Shortest distance to the exit node in the CFG *)
}    


(* Creates a mapping: bblockid -> bblock *)
module BlockMap = Map.Make(String)
type blockmap = bblock BlockMap.t
  
(* A control flow graph represents one function. It has always one entry node
   and one exit node. During construction, extra nodes are inserted to make
   sure of this property *)  
type cfg =
{
  entry_node : bblockid;  (* ID to the entry node. This is the same as the function name *)
  exit_node  : bblockid;  (* ID to the exit node *)
  cfg_graph  : blockmap;  (* The mapping that represents the actual graph *)
}

(* Creates a mapping: bblockid -> cfg *)
module CfgMap = Map.Make(String)

(* The function map contains all functions in a program *)  
type cfgmap = cfg CfgMap.t


 

  

(* Convenient names *)
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


 
