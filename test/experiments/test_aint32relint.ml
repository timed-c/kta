


open Printf
open Aint32relint

let debug = false

let test_relation relation testfun low high =  
  let rec work l1 h1 l2 h2 = (
    let testpoints truecase falsecase =
      for x = l1 to h1 do
        for y = l2 to h2 do
          if relation x y then
            if not (truecase x y) then 
              failwith (sprintf "True case [%d,%d] [%d,%d] point %d %d failed.\n"
                          l1 h2 l2 h2 x y)
            else
              if debug then printf "TRUE  x:%d y:%d OK.\n" x y else ()
          else
            if not (falsecase x y) then 
              failwith (sprintf "False case [%d,%d] [%d,%d] point %d %d failed.\n"
                          l1 h2 l2 h2 x y)
            else
              if debug then printf "FALSE x:%d y:%d OK.\n" x y else ()
        done
      done
    in
  
    let (tcase,fcase) = testfun (Interval(l1,h1)) (Interval(l2,h2)) in
    (match tcase with
    | Some(Interval(rl1,rh1),Interval(rl2,rh2)) ->
       if debug then printf "True case: [%d,%d] [%d,%d]\n" rl1 rh1 rl2 rh2;
       testpoints (fun x y -> x >= rl1 && x <= rh1 && y >= rl2 && y <= rh2)
                  (fun x y -> true)
     | None ->
       if debug then printf "True case: NONE\n";
       testpoints (fun x y -> false) (fun x y -> true)
     | _ -> failwith "Should not happen."
     );
    (match fcase with
     | Some(Interval(rl1,rh1),Interval(rl2,rh2)) ->
       if debug then printf "False case: [%d,%d] [%d,%d]\n" rl1 rh1 rl2 rh2;
       testpoints (fun x y -> true)
                  (fun x y -> x >= rl1 && x <= rh1 && y >= rl2 && y <= rh2)
     | None ->
       if debug then printf "False case: NONE\n";
       testpoints (fun x y -> true) (fun x y -> false)
     | _ -> failwith "Should not happen."
    );)
  in
  for h1 = low to high do
    for l1 = low to h1 do
      for h2 = low to high do
        for l2 = low to h2 do
          work l1 h1 l2 h2
        done
      done
    done
  done 
       



let main =    
  test_relation (<) aint32_test_less_than (-12) 12 

(*  test_relation (=) aint32_test_equal 3 7 *)







