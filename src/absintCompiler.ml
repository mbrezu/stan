
(* This file contains the PLSQL to abstract interpreter intermediate
   representation. *)

open Utils;;
open Absint.Ir;;
open PlsqlParser.Ast;;
open Acm;;
open Printf;;

module Gensym : sig
  val gensym: string -> string;;
  val reset_gensym: unit -> unit;;
end = struct
  let gensym_counter = ref 0;;

  let gensym prefix =
    gensym_counter := !gensym_counter + 1;
    sprintf "%s_%d" prefix !gensym_counter;;

  let reset_gensym () =
    gensym_counter := 0;;

end;;

open Gensym;;

let loop_label_user label = sprintf "UserLabel_%s_BeforeLoop" label;;

let loop_exit_label_user label = sprintf "UserLabel_%s_AfterLoop" label;;

let rec compile ast =
  match ast with
    | Program(stmts), _ ->
        compile_stmt_list stmts
    | Block(declarations, statements), _ ->
        add_ir AddFrame
        <+> compile_stmt_list declarations
        <+> compile_stmt_list statements
        <+> add_ir DeleteFrame
    | StmtNull, _ ->
        result ()
    | VarDecl(var_name, var_type), _ ->
        add_ir <| Declare(var_name, var_type)
    | StmtAssignment(target, expr), _ ->
        add_ir <| Assignment(target, expr)
    | StmtCall(subprogram, arguments), _ ->
        add_ir <| Absint.Ir.Call(subprogram, arguments)
    | StmtIf(cond, then_clause, else_clause), _ ->
        compile_if cond then_clause else_clause
    | StmtLabeled(label, stmt), _ ->
        (match stmt with
           | StmtLoop(stmts, _), _ ->
               let loop_label = loop_label_user label in
               let exit_label = loop_exit_label_user label in
                 compile_loop loop_label exit_label stmts
           | stmt ->
               (add_ir <| Label ("UserLabel_" ^ label))
               <+> compile stmt)
    | StmtLoop(stmts, _), _ ->
        let loop_label = gensym "BeforeLoop" in
        let exit_label = gensym "AfterLoop" in
          compile_loop loop_label exit_label stmts
    | StmtWhile(expr, body), _ ->
        let loop_label = gensym "BeforeWhile" in
        let exit_label = gensym "AfterWhile" in
          (match body with
             | StmtLoop(stmts, _), _ ->
                 compile_while loop_label exit_label expr stmts
             | _ ->
                 failwith "STAN internal error.")
    | StmtExitWhen(expr, maybe_label), _ ->
        (get_state >>= fun (labels, _, _) ->
           let next_insn = gensym "Next" in
             match (labels, maybe_label) with
               | _, Some label ->
                   (add_ir <| GotoIf(expr, loop_exit_label_user label, next_insn))
                   <+> (add_ir <| Label next_insn)
               | [_; after] :: _, _ ->
                   (add_ir <| GotoIf(expr, after, next_insn))
                   <+> (add_ir <| Label next_insn)
               | _ ->
                   failwith "STAN internal error.")
    | _ ->
        failwith "Unknown ast type."

and compile_while loop_label exit_label expr stmts =
  push_labels [loop_label; exit_label]
  <+> (add_ir <| Label loop_label)
  (* <+> (add_ir <| GotoIf( *)
  <+> (compile_stmt_list stmts)
  <+> (add_ir <| Goto(loop_label, None))
  <+> (add_ir <| Label exit_label)
  <+> pop_labels ()

and compile_loop loop_label exit_label stmts =
  push_labels [loop_label; exit_label]
  <+> (add_ir <| Label loop_label)
  <+> (compile_stmt_list stmts)
  <+> (add_ir <| Goto(loop_label, None))
  <+> (add_ir <| Label exit_label)
  <+> pop_labels ()

and compile_if cond then_clause else_clause =
  let then_label = gensym "Then" in
  let else_label = gensym "Else" in
  let after_if_label = gensym "AfterIf" in
    (add_ir <| GotoIf(cond, then_label, else_label))
    <+> (add_ir <| Label then_label)
    <+> (compile_stmt_list then_clause)
    <+> (add_ir <| Goto(after_if_label, None))
    <+> (add_ir <| Label else_label)
    <+> (match else_clause with
           | NoElse ->
               result ()
           | Else(stmts) ->
               compile_stmt_list stmts
           | ElsIf(cond, then_clause, else_clause) ->
               compile_if cond then_clause else_clause)
    <+> (add_ir <| Label after_if_label)

and compile_stmt_list stmts =
  match stmts with
    | hd :: tl ->
        compile hd
        <+> compile_stmt_list tl
    | [] ->
        result ();;

let compile_for_absint ast =
  reset_gensym ();
  let (_, irs, _), _ = run_compiler (compile ast) Acm.empty_state in
    List.rev irs;;

let compile_helper str =
  let _, result = PlsqlParser.parse2 str in
    match result with
      | Some (_, ast) ->
          compile_for_absint ast
      | None ->
          failwith "Parse failed.";;

