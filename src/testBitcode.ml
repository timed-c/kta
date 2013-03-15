

open Printf



let main =
  LlvmBitcode.decode (Utils.read_binfile "llvmtest/intrinsics.bc") 
(*    LlvmBitcode.decode (Utils.read_binfile "llvmtest/bsort100.bc") *)
