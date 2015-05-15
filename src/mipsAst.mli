

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
  | MipsAND     of rd * rs  * rt
  | MipsANDI    of rt * rs  * imm
  | MipsBEQ     of rs * rt  * imm
  | MipsBNE     of rs * rt  * imm
  | MipsJALR    of rs
  | MipsJR      of rs
  | MipsJ       of addr
  | MipsJAL     of addr
  | MipsLB      of rt * imm * rs
  | MipsLBU     of rt * imm * rs
  | MipsLUI     of rt * imm
  | MipsLW      of rt * imm * rs
  | MipsMUL     of rd * rs  * rt
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
  | MipsXOR     of rd * rs  * rt
  | MipsXORI    of rt * rs  * imm
  | MipsUnknown of int
      
