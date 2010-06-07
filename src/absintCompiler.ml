
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

let genpos = ParserTypes.Pos(0, 0);;

let loop_label_user label = sprintf "UserLabel_%s_BeforeLoop" label;;

let loop_exit_label_user label = sprintf "UserLabel_%s_AfterLoop" label;;

let compute_label_depth ast =
  let rec cld_impl ast map depth =
    match ast with
      | Program(stmts), _
      | StmtLoop(stmts, _), _ ->
          cld_statement_list stmts map depth
      | Block(_, stmts), _ ->
          cld_statement_list stmts map (depth + 1)
      | StmtLabeled(label, stmt), _ ->
          let new_map = StringMap.add label depth map in
            cld_impl stmt new_map depth
      | StmtWhile(expr, body), _ ->
          (match body with
             | StmtLoop(stmts, _), _ ->
                 cld_statement_list stmts map depth
             | _ ->
                 failwith "STAN internal error.")
      | _ ->
          map
  and cld_statement_list stmts map depth =
    match stmts with
      | hd :: tl ->
          let new_map = cld_impl hd map depth in
            cld_statement_list tl new_map depth
      | [] ->
          map
  in
    cld_impl ast StringMap.empty 0;;

let rec compile ast =
  match ast with
    | Program(stmts), _ ->
        compile_stmt_list stmts
    | Block(declarations, statements), _ ->
        add_ir AddFrame
        <+> (add_frame ())
        <+> compile_stmt_list declarations
        <+> compile_stmt_list statements
        <+> add_ir DeleteFrame
        <+> (delete_frame ())
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
    | StmtFor(var, reverse, start_expr, end_expr, body), _ ->
        let loop_label = gensym "BeforeFor" in
        let exit_label = gensym "AfterFor" in
          (match body with
             | StmtLoop(stmts, _), _ ->
                 compile_for var reverse start_expr end_expr loop_label exit_label stmts
             | _ ->
                 failwith "STAN internal error.")
    | StmtExitWhen(expr, maybe_label), _ ->
        (get_state >>= fun (private_state, public_state) ->
           let next_insn = gensym "Next" in
             match (private_state.label_stack, maybe_label) with
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

and compile_for var reverse start_expr end_expr loop_label exit_label stmts =
  let extract_var_str =
    match var with
      | Identifier(str), _ ->
          str
      | _ ->
          failwith "STAN internal error."
  in
  let compare_operator = if not reverse then "<=" else ">=" in
  let increment_operator = if not reverse then "+" else "-" in
  let body_start_label = gensym "ForBodyStart" in
    push_labels [loop_label; exit_label]
    <+> (add_ir <| AddFrame)
    <+> (add_frame ())
    <+> (add_ir <| Declare(extract_var_str, (Number(38, 127), genpos)))
    <+> (add_ir <| Assignment(var, start_expr))
    <+> (add_ir <| Label loop_label)
    <+> (add_ir <| GotoIf((BinaryOp(compare_operator, var, end_expr), genpos),
                          body_start_label,
                          exit_label))
    <+> (add_ir <| Label body_start_label)
    <+> (compile_stmt_list stmts)
    <+> (add_ir <| Assignment(var, (BinaryOp(increment_operator,
                                             var,
                                             (NumericLiteral("1"), genpos)),
                                    genpos)))
    <+> (add_ir <| Goto(loop_label, None))
    <+> (add_ir <| Label exit_label)
    <+> (add_ir <| DeleteFrame)
    <+> (delete_frame ())
    <+> pop_labels ()

and compile_while loop_label exit_label expr stmts =
  let body_start_label = gensym "WhileBodyStart" in
    push_labels [loop_label; exit_label]
    <+> (add_ir <| Label loop_label)
    <+> (add_ir <| GotoIf(expr, body_start_label, exit_label))
    <+> (add_ir <| Label body_start_label)
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
  let (_, public_state), _ = run_compiler (compile ast) Acm.empty_state in
    List.rev public_state.ir_list;;

let parse2_cont str cont =
  let _, result = PlsqlParser.parse2 str in
    match result with
      | Some (_, ast) ->
          cont ast
      | None ->
          failwith "Parse failed.";;

let compile_helper str =
  parse2_cont str compile_for_absint;;

