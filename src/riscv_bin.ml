


type big_endian = bool
type parcel = int

exception Incomplete_parcel

let parcels_of_bytes bige lst =
  let rec loop lst acc =  
    match bige,lst with
    | true, (high::low::rest) -> loop rest (((high lsl 8) lor low)::acc)
    | false, (low::high::rest) -> loop rest (((high lsl 8) lor low)::acc)
    | _,[] -> List.rev acc
    | _,_ -> raise Incomplete_parcel 
  in loop lst []    


let bytes_of_parcels bige lst = 
  List.rev (List.fold_left 
               (fun acc p -> if bige then (p land 0xff)::(p lsr 8)::acc
                             else (p lsr 8)::(p land 0xff)::acc) [] lst)


let decode bin = []



let encode inst = []
  

