
(* Absint Compiler Monad - in fact, another specialized state monad. *)

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

(* An empty state is an empty stack of labels, an empty list of IR
instructions and an empty list of error messages. *)
let empty_state = ([], [], []);;

let add_ir ir =
  get_state >>= fun (label_stack, ir_list, messages) ->
    set_state (label_stack, ir :: ir_list, messages);;

let push_labels labels =
  get_state >>= fun (label_stack, ir_list, messages) ->
    set_state (labels :: label_stack, ir_list, messages);;

let pop_labels () =
  get_state >>= fun (label_stack, ir_list, messages) ->
    match label_stack with
      | _ :: tl ->
          set_state (tl, ir_list, messages)
      | [] ->
          failwith "STAN Abstract Interpreter internal error (empty label stack).";;

let add_message message =
  get_state >>= fun (labels, ir_list, messages) ->
    set_state (labels, ir_list, message :: messages);;

let (<+>) m1 m2 = m1 >>= fun _ -> m2;;
