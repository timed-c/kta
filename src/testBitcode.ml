

open Printf



let main =
  LlvmBitcode.decode (Utils.read_binfile "llvmtest/intrinsics.bc")
