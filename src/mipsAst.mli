

type rd = int
type rs = int
type rt = int
type imm = int (* Stored sign extended in the AST *)


type inst =
  | MipsADD    of rd * rs * rt
  | MipsADDI   of rt * rs * imm
  | MipsADDIU  of rt * rs * imm
