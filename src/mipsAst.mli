

type rd = int
type rs = int
type rt = int
type imm = int (* Stored sign extended in the AST *)
type addr = int
type shamt = int

type inst =
  | MipsADD     of rd * rs  * rt
  | MipsADDI    of rt * rs  * imm
  | MipsADDIU   of rt * rs  * imm
  | MipsADDU    of rd * rs  * rt
  | MipsBEQ     of rs * rt  * imm
  | MipsBNE     of rs * rt  * imm
  | MipsJR      of rs
  | MipsJ       of addr
  | MipsJAL     of addr
  | MipsSLT     of rd * rs  * rt
  | MipsSLTU    of rd * rs  * rt
  | MipsSLTI    of rt * rs  * imm
  | MipsSLTIU   of rt * rs  * imm
  | MipsSLL     of rd * rt  * shamt
  | MipsSLLV    of rd * rt  * rs
  | MipsSRA     of rd * rt  * shamt
  | MipsSRL     of rd * rt  * shamt
  | MipsSRLV    of rd * rt  * rs
  | MipsLB      of rt * imm * rs
  | MipsLBU     of rt * imm * rs
  | MipsLW      of rt * imm * rs
  | MipsSB      of rt * imm * rs
  | MipsSW      of rt * imm * rs
  | MipsNOP    
  | MipsUnknown of int
      
