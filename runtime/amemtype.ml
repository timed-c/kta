(* open Aint32congruence *)
open Aint32congruence
open Printf
       
type amem_t =
  | AInt32 of aint32
  | AInt16 of aint32 * aint32
  | AInt8 of aint32 * aint32 * aint32 * aint32
  | AAny

let amem_pprint debug v =
  match v with
  | AInt32 v -> Ustring.to_utf8 (aint32_pprint debug v)
  | AInt16 (v1,v2) -> sprintf "(%s,%s)" (Ustring.to_utf8 (aint32_pprint debug v1)) (Ustring.to_utf8 (aint32_pprint debug v2))
  | AInt8 (v1,v2,v3,v4) -> (sprintf "(%s,%s,%s,%s)" (Ustring.to_utf8 (aint32_pprint debug v1)) (Ustring.to_utf8 (aint32_pprint debug v2)) (Ustring.to_utf8 (aint32_pprint debug v3)) (Ustring.to_utf8 (aint32_pprint debug v4)))
  | AAny -> "Any"


let amem_join bigendian v1 v2 =
  let convert_to_aint32 bigendian v = 
    (match v with
     | AInt32 v -> v
     | AInt16 (v0,v2) ->
        if bigendian then aint16_merge v0 v2
        else aint16_merge v2 v0
     | AInt8 (v0,v1,v2,v3) ->
        if bigendian then
          aint16_merge (aint8_merge v0 v1) (aint8_merge v2 v3)
        else
          aint16_merge (aint8_merge v3 v2) (aint8_merge v1 v0)
     | AAny -> aint32_any_set true)
  in
  match v1,v2 with
  | AInt32 v1, AInt32 v2 -> AInt32 (aint32_join v1 v2)
  | AInt16 (v11,v12), AInt16 (v21,v22) -> AInt16 (aint32_join v11 v21, aint32_join v21 v22)
  | AInt8 (v11,v12,v13,v14), AInt8(v21,v22,v23,v24) -> AInt8 (aint32_join v11 v21,aint32_join v12 v22,aint32_join v13 v23,aint32_join v14 v24)
  | v1,v2 ->
     let v1 = convert_to_aint32 bigendian v1 in
     let v2 = convert_to_aint32 bigendian v2 in
     AInt32 (aint32_join v1 v2) 
