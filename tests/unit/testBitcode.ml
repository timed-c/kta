

open Printf
open Ustring.Op
open LlvmBitcode


let main =
  (*let blocks = LlvmBitcode.decode (Utils.read_binfile "llvmtest/intrinsics.bc") in *)
  let blocks = LlvmBitcode.decode (Utils.read_binfile "llvmtest/minimal.bc") in
  uprint_endline (LlvmBitcode.debug_sprint blocks)

