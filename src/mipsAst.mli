

type rd = int
type rs = int
type rt = int
type imm = int (* Stored sign extended in the AST *)
type addr = int

type inst =
  | MipsADD     of rd * rs  * rt
  | MipsADDI    of rt * rs  * imm
  | MipsADDIU   of rt * rs  * imm
  | MipsADDU    of rd * rs  * rt
  | MipsJR      of rs
  | MipsJ       of addr
  | MipsJAL     of addr
  | MipsLB      of rt * imm * rs
  | MipsLBU     of rt * imm * rs
  | MipsLW      of rt * imm * rs
  | MipsSB      of rt * imm * rs
  | MipsSW      of rt * imm * rs
  | MipsNOP    
  | MipsUnknown of int
      
