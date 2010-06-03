
(* Absint Compiler Monad - in fact, another specialized state monad. *)

open Absint.Ir;;

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

type acm_private_state = { label_stack: string list list;
                           env_depth: int;
                         };;

let empty_private_state = { label_stack = []; env_depth = 0 };;

type acm_public_state = { ir_list: ir list;
                          messages: string list;
                        };;

let empty_public_state = { ir_list = []; messages = [] };;

(* An empty state is an empty stack of labels, an empty list of IR
instructions and an empty list of error messages. *)
let empty_state = (empty_private_state, empty_public_state);;

let add_ir ir =
  get_state >>= fun (private_state, public_state) ->
    set_state (private_state,
               { public_state with ir_list = ir :: public_state.ir_list });;

let push_labels labels =
  get_state >>= fun (private_state, public_state) ->
    set_state ({ private_state with label_stack = labels :: private_state.label_stack },
               public_state);;

let pop_labels () =
  get_state >>= fun (private_state, public_state) ->
    match private_state.label_stack with
      | _ :: tl ->
          set_state ({ private_state with label_stack = tl },
                     public_state)
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
  get_state >>= fun (private_state, public_state) ->
    result private_state.env_depth;;

let (<+>) m1 m2 = m1 >>= fun _ -> m2;;
