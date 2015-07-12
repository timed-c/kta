

open Ustring.Op
open Printf

type argtype =
| No      (* No arguments *)
| Str     (* Argument is a normal string *)
| Int     (* The argument is an integer that can be both postive and negative *)
| StrList (* The argument can be a list of strings. The list can be empty. *)


(* ---------------------------------------------------------------------*)
let parse argv options =
  
  (* Function for inserting a new argument *)
  let rec insert oplist op args =
    match oplist with
    | (o,str)::ls ->
      if op = o then (o,List.append str args)::ls
      else (o,str)::(insert ls op args)
    | [] -> [(op,args)]
  in

  (* Work function that iterates through all arguments *)
  let rec work args last_op exp_argtype acc_ops acc_args =
(*    printf "-----\n";
    printf "args: "; Ustring.concat (us",") args |> uprint_endline;
    printf "exp_argtype: %s\n" (match exp_argtype with | No -> "No" | Str -> "Str" | 
        Int -> "Int" | StrList -> "StrList"); *)
    match args with
    | a::al -> (
      try 
        let (k,opargty,_,_,_) = List.find (fun (_,_,x,_,_) -> x =. a) options  in       
        
        (* We have found an option *)
        (match exp_argtype with
        | No | StrList -> work al k opargty (insert acc_ops k []) acc_args
        | Str | Int -> 
           let (_,_,x,_,_) = List.find (fun (k,_,_,_,_) -> k = last_op) options  in 
           (None, us"Option " ^. x ^. us" needs an option argument."))
    
      (* Not a known option *)
      with Not_found -> 

        (* Check if starts with dash. If so, error *)
        if Ustring.starts_with (us"-") a then
          (None, us"Incorrect or unknown argument '" ^. a ^. us"'") 
        else
          (match exp_argtype with
          | No ->
            (* This is a normal argument *)
              work al last_op No acc_ops (a::acc_args)
          | Str ->
              (* We have one string argument. Next will not be option argument *)
              work al last_op No (insert acc_ops last_op [a]) acc_args
          | Int ->
              (* We have one int argument. Next will not be option argument *)
             (try let _ = int_of_string (Ustring.to_utf8 a) in
                 work al last_op No (insert acc_ops last_op [a]) acc_args
              with _ -> (None, us"Option argument '" ^. a ^. us"' is not an integer."))
          | StrList ->
              (* We have string argument of list. *)
              work al last_op exp_argtype (insert acc_ops last_op [a]) acc_args))


    
      | [] -> (Some (acc_ops,List.rev acc_args), us"")
            

  in
    if List.length options = 0 
    then (Some([],List.map us argv),us"")
    else            
      let (dummyop,_,_,_,_) = List.hd options in
      work (List.map us argv) dummyop No [] []

  
(* ---------------------------------------------------------------------*)
let optionstext options = us"Options..."
  

