(*
    STAN - a STatic ANalysis tool for PL/SQL. Copyright (C) 2010 Miron Brezuleanu

    This program is free software: you can redistribute it and/or
    modify it under the terms of the GNU General Public License as
    published by the Free Software Foundation, either version 3 of the
    License, or (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
    General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with this program.  If not, see
    <http://www.gnu.org/licenses/>.
*)

(* Absint Compiler Monad - in fact, another specialized state monad. *)

open Utils;;
open Absint.Ir;;
open Printf;;

type ('a, 'b) compiler_monad = CompilerM of ('a -> 'a * 'b);;

let run_compiler (CompilerM fn) state = fn state;;

let bind m f =
  CompilerM (fun s ->
             let new_state, result = run_compiler m s in
             let new_m = f result in
               run_compiler new_m new_state);;

(* Syntactic sugar for bind. *)
let (>>=) = bind;;

let result k = CompilerM (fun s -> (s, k));;

let get_state = CompilerM (fun s -> (s, s));;

let set_state s = CompilerM (fun _ -> (s, ()));;

type acm_label_stack_level = { depth: int;
                               labels: string list;
                             };;

type acm_private_state = { label_stack: acm_label_stack_level list;
                           env_depth: int;
                           label_depths: int StringMap.t;
                           active_label: string option;
                           gensym_counter: int;
                         };;

let empty_private_state = { label_stack = [];
                            env_depth = 0;
                            label_depths = StringMap.empty;
                            active_label = None;
                            gensym_counter = 1;
                          };;

type acm_public_state = { ir_list: ir list;
                          messages: string list;
                        };;

let empty_public_state = { ir_list = []; messages = [] };;

(* An empty state is an empty stack of labels, an empty list of IR
instructions and an empty list of error messages. *)
let start_state label_depths = ({ empty_private_state with label_depths = label_depths },
                                empty_public_state);;

let add_ir ir =
  get_state >>= fun (private_state, public_state) ->
    set_state (private_state,
               { public_state with ir_list = ir :: public_state.ir_list });;

let push_labels labels =
  get_state >>= fun (private_state, public_state) ->
    let new_label_stack = { depth = private_state.env_depth;
                            labels = labels } :: private_state.label_stack in
      set_state ({ private_state with label_stack = new_label_stack },
                 public_state);;

let pop_labels () =
  get_state >>= fun (private_state, public_state) ->
    match private_state.label_stack with
      | _ :: tl ->
          set_state ({ private_state with label_stack = tl },
                     public_state)
      | [] ->
          failwith "STAN Abstract Interpreter internal error (empty label stack).";;

let labels_top () =
  get_state >>= fun (private_state, public_state) ->
    match private_state.label_stack with
      | hd :: _ ->
          result hd
      | [] ->
          failwith "STAN Abstract Interpreter internal error (empty label stack).";;

let add_message message =
  get_state >>= fun (private_state, public_state) ->
    set_state (private_state,
               { public_state with messages = message :: public_state.messages });;

let add_frame () =
  get_state >>= fun (private_state, public_state) ->
    set_state ({ private_state with env_depth = private_state.env_depth + 1 },
               public_state);;

let delete_frame () =
  get_state >>= fun (private_state, public_state) ->
    set_state ({ private_state with env_depth = private_state.env_depth - 1 },
               public_state);;

let get_env_depth () =
  get_state >>= fun (private_state, _) ->
    result private_state.env_depth;;

let get_label_depth label =
  get_state >>= fun (private_state, _) ->
    if StringMap.mem label private_state.label_depths
    then result (StringMap.find label private_state.label_depths)
    else failwith "STAN FIXME: Label not found. Should add message to public state."

let set_active_label label =
  get_state >>= fun (private_state, public_state) ->
    set_state ({ private_state with active_label = Some label },
               public_state);;

let clear_active_label () =
  get_state >>= fun (private_state, public_state) ->
    set_state ({ private_state with active_label = None },
               public_state);;

let get_active_label () =
  get_state >>= fun (private_state, _) ->
    result private_state.active_label;;

let (<+>) m1 m2 = m1 >>= fun _ -> m2;;

let gensym prefix =
  get_state >>= fun (private_state, public_state) ->
    let gensym_result = sprintf "%s_%d" prefix private_state.gensym_counter in
      set_state ({ private_state with gensym_counter = private_state.gensym_counter + 1; },
                 public_state)
      <+> (result gensym_result)


