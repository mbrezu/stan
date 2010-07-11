
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

(* This file contains the compiler which translates from PLSQL syntax
   trees to abstract interpreter intermediate representation. *)

open Utils;;
open Absint.Ir;;
open PlsqlParser.Ast;;
open Acm;;
open Printf;;

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
          let label_depth =
            match stmt with
              | StmtFor(_, _, _, _, _), _ ->
                  depth + 1
              | _ ->
                  depth
          in
          let new_map = StringMap.add label label_depth map in
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

let repeat_acm acm n =
  let rec ra_impl n =
    if n = 0
    then result ()
    else acm <+> (ra_impl (n - 1))
  in
    ra_impl n;;

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
        (* TODO: add active label handling for while and for, add
           tests for labeled while, fix broken tests. *)
        (add_ir <| Label ("UserLabel_" ^ label))
        <+> (set_active_label label)
        <+> (compile stmt)
        <+> (clear_active_label ())
    | StmtLoop(stmts, _), _ ->
        compile_loop stmts
    | StmtWhile(expr, body), _ ->
        (match body with
           | StmtLoop(stmts, _), _ ->
               compile_while expr stmts
           | _ ->
               failwith "STAN internal error (body of WHILE must be LOOP).")
    | StmtFor(var, reverse, start_expr, end_expr, body), _ ->
        (match body with
           | StmtLoop(stmts, _), _ ->
               compile_for var reverse start_expr end_expr stmts
           | _ ->
               failwith "STAN internal error (body of FOR must be LOOP).")
    | StmtExitWhen(expr, maybe_label), _ ->
        (gensym "Next" >>= fun next_insn ->
           get_env_depth () >>= fun current_depth ->
             labels_top () >>= fun { depth = top_labels_depth; labels = top_labels } ->
               let discharger label_depth after_label =
                 (* FIXME: should add an error message if
                    current_depth < label_depth *)
                 if current_depth > label_depth
                 then
                   (let depth_diff = current_depth - label_depth in
                    let delete_frame_block =
                      repeat_acm (add_ir <| DeleteFrame) depth_diff
                    in
                      (gensym "DischargeEnv") >>= fun discharge_env_label ->
                        (add_ir <| GotoIf(expr, discharge_env_label, next_insn))
                        <+> (add_ir <| Label discharge_env_label)
                        <+> delete_frame_block
                        <+> (add_ir <| Goto(after_label, None))
                        <+> (add_ir <| Label next_insn))
                 else
                   (add_ir <| GotoIf(expr, after_label, next_insn))
                   <+> (add_ir <| Label next_insn)
               in
                 match (top_labels, maybe_label) with
                   | _, Some label ->
                       (get_label_depth label >>= fun label_depth ->
                          discharger label_depth (loop_exit_label_user label))
                   | [_; after], _ ->
                       discharger top_labels_depth after
                   | _ ->
                       failwith "STAN internal error.")
    | _ ->
        failwith "Unknown ast type."

and compile_for var reverse start_expr end_expr stmts =
  let extract_var_str =
    match var with
      | Identifier(str), _ ->
          str
      | _ ->
          failwith "STAN internal error."
  in
  let compare_operator = if not reverse then "<=" else ">=" in
  let increment_operator = if not reverse then "+" else "-" in
    get_labels () >>= fun (loop_label, exit_label) ->
      gensym "ForBodyStart" >>= fun body_start_label ->
        (add_ir <| AddFrame)
        <+> (add_frame ())
        <+> (push_labels [loop_label; exit_label])
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
        <+> (pop_labels ())
        <+> (add_ir <| DeleteFrame)
        <+> (delete_frame ())

and compile_while expr stmts =
  get_labels () >>= fun (loop_label, exit_label) ->
    gensym "WhileBodyStart" >>= fun body_start_label ->
      push_labels [loop_label; exit_label]
      <+> (add_ir <| Label loop_label)
      <+> (add_ir <| GotoIf(expr, body_start_label, exit_label))
      <+> (add_ir <| Label body_start_label)
      <+> (compile_stmt_list stmts)
      <+> (add_ir <| Goto(loop_label, None))
      <+> (add_ir <| Label exit_label)
      <+> pop_labels ()

and get_labels () =
  get_active_label () >>= function
    | None ->
        (gensym "BeforeLoop" >>= fun loop_label ->
           gensym "AfterLoop" >>= fun exit_label ->
             result (loop_label, exit_label))
    | Some label ->
        clear_active_label ()
        <+> result (loop_label_user label,
                    loop_exit_label_user label)

and compile_loop stmts =
  get_labels () >>= fun (loop_label, exit_label) ->
    push_labels [loop_label; exit_label]
    <+> (add_ir <| Label loop_label)
    <+> (compile_stmt_list stmts)
    <+> (add_ir <| Goto(loop_label, None))
    <+> (add_ir <| Label exit_label)
    <+> pop_labels ()

and compile_if cond then_clause else_clause =
  gensym "Then" >>= fun then_label ->
    gensym "Else" >>= fun else_label ->
      gensym "AfterIf" >>= fun after_if_label ->
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
  let label_depths = compute_label_depth ast in
  let (_, public_state), _ = run_compiler (compile ast) (Acm.start_state label_depths) in
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

