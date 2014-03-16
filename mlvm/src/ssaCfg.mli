


open LlvmAst
open Ugraph

type var = int
type node = var

type def_use_map = Hashtbl.t 

type ssacfg = {
  cfg : graph;  (* Control flow graph *)
  rcfg : graph; (* Reversed control flow graph *)
  var2node : node array; (* Mapping from variable to node *)
  first_var : var array; (* Defines the first variable in a node *)
  last_var : var array; (* Defines the last variable in a node *)
  prev_var : var array; (* Previous var in a block. -1 if no previous *)
  next_var : var array; (* Next var in a block. -1 if last*)
  using_var : (var list) array; (* List of used variables at a certain 
                                  program point *)  
  pp_var : bool array; (* True if var is only marking a program point, i.e.,
                         it is not defining a value. For instance a call
                         without return value *)
  def_use : (var list) array; (* Definition-use chain. *)  
  phi : ((var * node) list) array; (* Defines the list of arguments if 
                                     a variable defines a phi node *)
}

(* Two artificial nodes are always available. Node 0 is the artificial 
   entry node that includes definitions of parameters. This node points
   to the actual entry node. Node 1 is the artificial exit node. All
   nodes with return statements points to this node. *)

(*val construct : llFunc -> ssacfg *)
val construct : int -> int
(** Takes an LLVM function and creates a static single assignment
control flow graph (SSACFG). *)



val def_use_in_blocks : Llvm.llFunc -> def_use_map  
