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

open ParserTypes;;
open Utils;;
open Pwm;;
open Lexer;;
open Printf;;

let parse_semicolon =
  consume_or_fake ";";;

let combine_ast_pos start_ast end_ast =
  match start_ast, end_ast with
    | (_, Pos(start_pos, _)), (_, Pos(_, end_pos)) ->
        Pos(start_pos, end_pos);;

let string_item =
  item >>= fun (Token(item_content, _)) ->
    result item_content;;

let wrap_pos p =
  get_next_pos >>= fun start_pos ->
    p >>= fun res ->
      get_previous_pos >>= fun end_pos ->
        result (res, Pos(start_pos, end_pos));;

let wrap_pos_from ast p =
  let _, Pos(start_pos, _) = ast in
    p >>= fun res ->
      get_previous_pos >>= fun end_pos ->
        result (res, Pos(start_pos, end_pos));;

let extract_limits ast_list =
  let rec last list =
    match list with
      | [hd] -> hd
      | hd :: tl -> last tl
      | [] -> failwith "Empty list."
  in
    match ast_list with
      | hd :: tl ->
          let _, Pos(start_pos, _) = hd in
          let last = last ast_list in
          let _, Pos(_, end_pos) = last in
            Pos(start_pos, end_pos)
      | [] -> Pos(0, 0);;

let check_all pred str =
  let rec check_all_iter pos =
    if pos >= String.length str
    then true
    else if not (pred str.[pos])
    then false
    else check_all_iter (pos + 1)
  in
    check_all_iter 0;;

let check_sequence seq =
  lookahead_many (List.length seq) >>= function
    | Some tokens ->
        let content = List.map (fun (Token(content, _)) -> content) tokens in
          result (seq = content)
    | None ->
        result false;;

let matching_sequence seqs =
  let parse_seq seq =
    check_sequence seq >>= function
      | true ->
          (consume_many (List.length seq) >>= fun tokens ->
             let entire_op =
               String.concat "" <| List.map (fun (Token(content, _)) -> content) tokens
             in
               result entire_op)
      | false ->
          fail
  in
  let parsers = List.map (fun seq -> parse_seq seq) seqs in
    (List.fold_left (</>) (List.hd parsers) (List.tl parsers) >>= fun str ->
       result <| Some str)
    </> result None;;

module Ast =
struct
  type cursor_expr_type =
    | CursorFound
    | CursorNotFound
    | CursorRowCount
    | CursorIsOpen
    | CursorBulkRowCount
    | CursorBulkExceptions
  type expression_ast =
    | NumericLiteral of string
    | StringLiteral of string
    | Identifier of string
    | BinaryOp of string * expression_ast_with_pos * expression_ast_with_pos
    | UnaryOp of string * expression_ast_with_pos
    | Call of expression_ast_with_pos * expression_ast_with_pos list
    | IsNull of expression_ast_with_pos
    | IsNotNull of expression_ast_with_pos
    | Like of expression_ast_with_pos * expression_ast_with_pos
    | Between of expression_ast_with_pos * expression_ast_with_pos * expression_ast_with_pos
    | NotBetween of
        expression_ast_with_pos * expression_ast_with_pos * expression_ast_with_pos
    | Subquery of select_ast_with_pos
    | Exists of expression_ast_with_pos
    | Any of expression_ast_with_pos
    | SqlSome of expression_ast_with_pos
    | All of expression_ast_with_pos
    | In of expression_ast_with_pos * expression_ast_with_pos
    | NotIn of expression_ast_with_pos * expression_ast_with_pos
    | List of expression_ast_with_pos list
    | SimpleCase of (expression_ast_with_pos
                     * case_when list
                     * expression_ast_with_pos option)
    | SearchedCase of (case_when list * expression_ast_with_pos option)
    | CursorExpr of expression_ast_with_pos * cursor_expr_type
  and case_when = CaseWhen of (expression_ast_with_pos * expression_ast_with_pos)
  and expression_ast_with_pos = expression_ast * pos

  and join_type = Join | InnerJoin | FullOuterJoin | LeftOuterJoin | RightOuterJoin
  and table_expression_ast =
    | TableAlias of string * string
    | TableName of string
    | TableJoin of join_type *
        table_expression_ast_with_pos *
        table_expression_ast_with_pos *
        expression_ast_with_pos option *
        string list option
  and table_expression_ast_with_pos = table_expression_ast * pos

  and select_ast =
    | Select of select_components
    | IntoClause of expression_ast_with_pos list
    | FromClause of table_expression_ast_with_pos list
    | WhereClause of expression_ast_with_pos
    | GroupByClause of expression_ast_with_pos list
    | HavingClause of expression_ast_with_pos
    | OrderByClause of expression_ast_with_pos * string
    | Column of expression_ast_with_pos
    | ColumnAlias of string * expression_ast_with_pos * string
  and select_ast_with_pos = select_ast * pos
  and select_components = { fields : select_ast_with_pos list;
                            clauses : select_ast_with_pos list };;

  type typ =
    | Number of int * int
    | Varchar of int
    | Varchar2 of int
    | VarcharNoSize
    | Varchar2NoSize
    | Date
    | RowTypeAnchor of expression_ast_with_pos
    | TypeAnchor of expression_ast_with_pos
    | TypeName of expression_ast_with_pos
  and typ_with_pos = typ * pos;;

  type creation_type =
    | Create
    | CreateOrReplace;;

  type plsql_ast =
    | Program of plsql_ast_with_pos list
    | Block of plsql_ast_with_pos list * plsql_ast_with_pos list
    | VarDecl of string * typ_with_pos
    | ArgDecl of string * typ_with_pos
    | FieldDecl of string * typ_with_pos
    | RecordDecl of expression_ast_with_pos * plsql_ast_with_pos list
    | CursorDecl of expression_ast_with_pos * select_ast_with_pos
    | ParameterizedCursorDecl of expression_ast_with_pos *
        plsql_ast_with_pos list *
        select_ast_with_pos
    | TableDecl of expression_ast_with_pos * typ_with_pos
    | StmtAssignment of expression_ast_with_pos * expression_ast_with_pos
    | StmtSelect of select_ast_with_pos
    | StmtIf of if_args
    | StmtLoop of plsql_ast_with_pos list * string option
    | StmtLabeled of string * plsql_ast_with_pos
    | StmtExit of string option
    | StmtExitWhen of expression_ast_with_pos * string option
    | StmtContinue of string option
    | StmtContinueWhen of expression_ast_with_pos * string option
    | StmtFor of expression_ast_with_pos *
        bool *
        expression_ast_with_pos *
        expression_ast_with_pos *
        plsql_ast_with_pos
    | StmtForCursor of expression_ast_with_pos *
        bool *
        expression_ast_with_pos *
        plsql_ast_with_pos
    | StmtForParameterizedCursor of expression_ast_with_pos *
        bool *
        expression_ast_with_pos *
        expression_ast_with_pos list *
        plsql_ast_with_pos
    | StmtWhile of expression_ast_with_pos * plsql_ast_with_pos
    | StmtGoto of expression_ast_with_pos
    | StmtNull
    | StmtCall of expression_ast_with_pos * expression_ast_with_pos list
    | StmtCreateProcedure of creation_type *
        expression_ast_with_pos *
        plsql_ast_with_pos list *
        string *
        plsql_ast_with_pos
    | StmtCreateFunction of creation_type *
        expression_ast_with_pos *
        plsql_ast_with_pos list *
        typ_with_pos *
        string *
        plsql_ast_with_pos
    | StmtReturn of expression_ast_with_pos option
    | StmtCreateTable of expression_ast_with_pos * plsql_ast_with_pos list
    | StmtOpen of expression_ast_with_pos
    | StmtOpenParameterized of expression_ast_with_pos * expression_ast_with_pos list
    | StmtClose of expression_ast_with_pos
    | StmtFetch of expression_ast_with_pos * expression_ast_with_pos list
    | StmtFetchBulkCollect of expression_ast_with_pos * expression_ast_with_pos list
  and if_args = expression_ast_with_pos
      * plsql_ast_with_pos list
      * else_elseif
  and else_elseif =
    | NoElse
    | Else of (plsql_ast_with_pos list)
    | ElsIf of if_args
  and plsql_ast_with_pos = plsql_ast * pos;;

end;;

open Ast;;

module ExpressionAndSelect :
sig
  val parse_expression : (token, expression_ast_with_pos) parser;;
  val parse_dotted_identifier : (token, expression_ast_with_pos) parser;;
  val parse_select : (token, select_ast_with_pos) parser;;
end =
struct

  let parse_binary_op_left_assoc_generic ops term_parser combiner =
    let rec bin_op_iter left_term =
      matching_sequence ops >>= function
        | Some found_op ->
            (term_parser >>= fun right_term ->
               bin_op_iter (combiner found_op left_term right_term,
                            combine_ast_pos left_term right_term))
        | None -> result left_term
    in
      term_parser >>= fun first_term -> bin_op_iter first_term

  let rec parse_identifier () =
    wrap_pos (item >>= fun (Token(content, _)) ->
                if is_letter content.[0] || content.[0] = '_' || content = "*"
                then result <| Identifier(content)
                else warning (sprintf "Expected an identifier, but got '%s'." content) <+>
                  (result <| Identifier(content)))

  and parse_dotted_identifier () =
    parse_binary_op_left_assoc [["."]] (parse_identifier ())

  and parse_number () =
    wrap_pos (item >>= fun expr ->
                let content = token_content expr in
                  if (check_all is_digit content)
                  then result (NumericLiteral content)
                  else fail)

  and parse_parenthesis () =
    wrap_pos (consume "(" >>= fun _ ->
                lookahead >>= function
                  | Some (Token("SELECT", _)) ->
                      (parse_select () >>= fun subquery ->
                         consume ")" <+> (result <| Subquery (subquery)))
                  | _ ->
                      sep_by "," (parse_expression ()) >>= function
                        | [(expr, _)] -> consume ")" <+> (result expr)
                        | exprs -> consume ")" <+> (result <| List(exprs)))

  and parse_dotted_identifier_or_function_call_or_cursor_expression () =
    wrap_pos (parse_dotted_identifier () >>= fun (ident, ident_pos as ident_with_pos) ->
                lookahead >>= function
                  | Some(Token("(", _)) ->
                      (consume "("
                       <+> sep_by "," (parse_expression ()) >>= fun args ->
                         consume ")" <+> (result <| Call((ident, ident_pos), args)))
                  | Some(Token("%", _)) ->
                      (consume "%" <+> string_item >>= fun cursor_expr ->
                         match cursor_expr with
                           | "ISOPEN" -> result <| CursorExpr(ident_with_pos, CursorIsOpen)
                           | "FOUND" -> result <| CursorExpr(ident_with_pos, CursorFound)
                           | "NOTFOUND" -> result <| CursorExpr(ident_with_pos,
                                                                CursorNotFound)
                           | "ROWCOUNT" -> result <| CursorExpr(ident_with_pos,
                                                                CursorRowCount)
                           | "BULK_ROWCOUNT" -> result <| CursorExpr(ident_with_pos,
                                                                     CursorBulkRowCount)
                           | "BULK_EXCEPTIONS" -> result <| CursorExpr(ident_with_pos,
                                                                       CursorBulkExceptions)
                           | _ ->
                               warning "Unknown cursor expression. Assumed '%ISOPEN'."
                               <+> (result <| CursorExpr(ident_with_pos, CursorIsOpen)))
                  | _ ->
                      result ident)

  and parse_string () =
    wrap_pos (string_item >>= fun str -> result <| StringLiteral str)

  and parse_case () =
    let rec parse_whens whens =
      lookahead >>= function
        | Some (Token("WHEN", _)) ->
            (consume "WHEN" <+> parse_expression () >>= fun cond ->
               consume_or_fake "THEN" <+> parse_expression () >>= fun result ->
                 parse_whens <| CaseWhen(cond, result) :: whens)
        | _ ->
            result <| List.rev whens
    in
    let parse_else =
      lookahead >>= function
        | Some (Token("ELSE", _)) ->
            (consume "ELSE" <+> parse_expression () >>= fun else_expr ->
               consume_or_fake "END" <+>
                 (result <| Some else_expr))
        | _ ->
            (consume_or_fake "END" <+>
               (result None))
    in
      wrap_pos (consume "CASE" <+> lookahead >>= function
                  | Some (Token("WHEN", _)) ->
                      (parse_whens [] >>= fun whens ->
                         parse_else >>= fun else_expr ->
                           result <| SearchedCase(whens, else_expr))
                  | _ ->
                      (parse_expression () >>= fun expr ->
                         parse_whens [] >>= fun whens ->
                           parse_else >>= fun else_expr ->
                             result <| SimpleCase(expr, whens, else_expr)))

  and parse_unary_op () =
    wrap_pos (lookahead >>= function
                | Some(Token("-", _)) | Some(Token("+", _)) ->
                    (string_item >>= fun op ->
                       parse_unary () >>= fun expr ->
                         result <| UnaryOp(op, expr))
                | _ -> failwith "Internal error.")

  and parse_unary () =
    lookahead >>= function
      | Some (Token("-", _)) | Some (Token("+", _)) -> parse_unary_op ()
      | Some (Token("(", _)) -> parse_parenthesis ()
      | Some (Token(num, _)) when check_all is_digit num -> parse_number ()
      | Some (Token(str, _)) when str.[0] = '\'' -> parse_string ()
      | Some (Token("ANY", _)) ->
          wrap_pos (consume "ANY" <+> parse_expression () >>= fun expr ->
                      result <| Any(expr))
      | Some (Token("ALL", _)) ->
          wrap_pos (consume "ALL" <+> parse_expression () >>= fun expr ->
                      result <| All(expr))
      | Some (Token("SOME", _)) ->
          wrap_pos (consume "SOME" <+> parse_expression () >>= fun expr ->
                      result <| SqlSome(expr))
      | Some (Token("EXISTS", _)) ->
          wrap_pos (consume "EXISTS" <+> parse_expression () >>= fun expr ->
                      result <| Exists(expr))
      | Some (Token("CASE", _)) ->
          parse_case ()
      | Some (Token(id, _)) when is_letter id.[0] || id.[0] = '_' || id.[0] = '*' ->
          parse_dotted_identifier_or_function_call_or_cursor_expression ()
      | _ ->
          warning "Expected an identifier, number or '('. Inserted a '_'." <+>
            get_next_pos >>= fun pos ->
              result <| (Identifier "_", Pos(pos, pos))

  and parse_binary_op_left_assoc ops term_parser =
    let combiner op left right = BinaryOp(op, left, right) in
      parse_binary_op_left_assoc_generic ops term_parser combiner

  and parse_maybe_unary unary_op term_parser =
    lookahead >>= function
      | Some(Token(content, _)) when content = unary_op ->
          wrap_pos (consume unary_op <+> term_parser >>= fun expr ->
                      result <| UnaryOp(unary_op, expr))
      | _ -> term_parser

  and parse_is_null p_result =
    wrap_pos_from p_result (lookahead_many 3 >>= function
                              | Some [Token("IS", _); Token("NULL", _); _] ->
                                  (consume_many 2) <+> (result <| IsNull(p_result) )
                              | Some [Token("IS", _); Token("NOT", _); Token("NULL", _)] ->
                                  (consume_many 3) <+> (result <| IsNotNull(p_result))
                              | _ ->
                                  error "parse_is_null: Internal error")

  and parse_like p_result p =
    wrap_pos_from p_result (lookahead >>= function
                              | Some (Token("LIKE", _)) ->
                                  (consume "LIKE" <+> p >>= fun expr ->
                                     result <| Like(p_result, expr))
                              | _ ->
                                  error "parse_like: Internal error")

  and parse_in p_result p useNot =
    wrap_pos_from p_result (lookahead >>= function
                              | Some (Token("IN", _)) ->
                                  (consume "IN" <+> p >>= fun expr ->
                                     let in_result =
                                       if useNot
                                       then NotIn(p_result, expr)
                                       else In(p_result, expr)
                                     in
                                       result in_result)
                              | _ ->
                                  error "parse_in: Internal error")

  and parse_between p_result p useNot =
    wrap_pos_from p_result (lookahead >>= function
                              | Some (Token("BETWEEN", _)) ->
                                  (consume "BETWEEN" <+> p >>= fun expr_low ->
                                     consume "AND" <+> p >>= fun expr_high ->
                                       let between_result =
                                         if useNot
                                         then NotBetween(p_result, expr_low, expr_high)
                                         else Between(p_result, expr_low, expr_high)
                                       in
                                         result between_result)
                              | _ ->
                                  error "parse_between: Internal error")

  and parse_comparison p =
    p >>= fun p_result ->
      lookahead >>= function
        | Some (Token("IS", _)) ->
            parse_is_null p_result
        | Some (Token("LIKE", _)) ->
            parse_like p_result p
        | Some (Token("IN", _)) ->
            parse_in p_result p false
        | Some (Token("BETWEEN", _)) ->
            parse_between p_result p false
        | Some (Token("NOT", _)) ->
            (consume "NOT" <+> lookahead >>= function
               | Some (Token("BETWEEN", _)) ->
                   parse_between p_result p true
               | Some (Token("IN", _)) ->
                   parse_in p_result p true
               | _ ->
                   result p_result)
        | _ ->
            result p_result

  and parse_expression () =
    let parse_term = parse_binary_op_left_assoc [["*"]; ["/"]] (parse_unary ()) in
    let parse_arithmetic = parse_binary_op_left_assoc [["+"]; ["-"]; ["|"; "|"]] parse_term in
    let rel_ops = [["<"; "="]; [">"; "="]; ["<"; ">"]; ["<"]; [">"]; ["="]] in
    let parse_relational = parse_binary_op_left_assoc rel_ops  parse_arithmetic in
    let parse_comparison = parse_comparison parse_relational in
    let parse_logical_factor = parse_maybe_unary "NOT" parse_comparison in
    let parse_logical_term = parse_binary_op_left_assoc [["AND"]] parse_logical_factor in
      parse_binary_op_left_assoc [["OR"]] parse_logical_term

  (* Expression ends, Select begins. *)

  and parse_select () =
    wrap_pos (consume "SELECT" <+> parse_select_fields () >>= fun fields ->
                parse_select_clauses [] >>= fun clauses ->
                  result <| Select { fields = fields; clauses = clauses })

  and parse_select_clauses clauses =
    let clause_parser p =
      p () >>= fun new_clause ->
        parse_select_clauses (new_clause :: clauses)
    in
      lookahead_many 2 >>= function
        | Some [Token ("FROM", _); _] -> clause_parser parse_from_clause
        | Some [Token ("INTO", _); _] -> clause_parser parse_into_clause
        | Some [Token ("HAVING", _); _] -> clause_parser parse_having_clause
        | Some [Token ("GROUP", _); Token("BY", _)] -> clause_parser parse_group_by_clause
        | Some [Token ("ORDER", _); Token("BY", _)] -> clause_parser parse_order_by_clause
        | Some [Token ("WHERE", _); _] -> clause_parser parse_where_clause
        | _ -> result <| List.rev clauses

  and parse_having_clause () =
    wrap_pos (consume "HAVING" <+> (parse_expression ()) >>= fun expr ->
                result <| HavingClause(expr))

  and parse_group_by_clause () =
    wrap_pos (consume_many 2 <+> sep_by "," (parse_expression ()) >>= fun exprs ->
                result <| GroupByClause(exprs))

  and parse_order_by_clause () =
    wrap_pos (consume_many 2 <+> (parse_expression ()) >>= fun expr ->
                lookahead >>= function
                  | Some (Token("ASC", _)) ->
                      consume "ASC" <+> (result <| OrderByClause(expr, "ASC"))
                  | Some (Token("DESC", _)) ->
                      consume "DESC" <+> (result <| OrderByClause(expr, "DESC"))
                  | _ ->
                      result <| OrderByClause(expr, ""))

  and parse_where_clause () =
    wrap_pos (consume "WHERE" <+> (parse_expression ()) >>= fun expr ->
                result <| WhereClause(expr))

  and parse_select_fields () =
    let parse_column =
      wrap_pos (parse_expression () >>= fun expr ->
                  let possible_alias tok =
                    tok <> "FROM"
                    && tok <> "INTO"
                    && tok <> ","
                  in
                    lookahead >>= function
                      | Some(Token(tok, _)) when possible_alias tok ->
                          let make_alias alias_connector =
                            string_item >>= fun alias ->
                              result (ColumnAlias(alias_connector, expr, alias))
                          in
                            if tok = "AS"
                            then consume "AS" <+> make_alias "AS"
                            else make_alias ""
                      | _ ->
                          result <| Column(expr))
    in
      sep_by "," parse_column

  and parse_from_clause () =
    wrap_pos (consume "FROM"
              <+> parse_table_list () >>= fun tables ->
                result <| FromClause (tables))

  and parse_into_clause () =
    wrap_pos (consume "INTO"
              <+> sep_by "," (parse_dotted_identifier ()) >>= fun idents ->
                result <| IntoClause(idents))

  and parse_table_with_alias () =
    let validate alias =
      alias <> ","
      && alias <> ";"
      && alias <> "WHERE"
      && alias <> ")"
      && alias <> "INNER"
      && alias <> "OUTER"
      && alias <> "FULL"
      && alias <> "RIGHT"
      && alias <> "LEFT"
    in
      wrap_pos (string_item >>= fun ident ->
                  lookahead >>= function
                    | Some(Token(alias, _)) when validate alias ->
                        (string_item >>= fun alias ->
                           result (TableAlias(ident, alias)))
                    | _ ->
                        result <| TableName(ident))

  and parse_table_expression () =
    let rec pte_iter left =
      lookahead_many 3 >>= function
        | Some[Token("JOIN", _); _; _] ->
            join_parser 1 left Join
        | Some[Token("INNER", _); Token("JOIN", _); _] ->
            join_parser 2 left InnerJoin
        | Some[Token("FULL", _); Token("OUTER", _); Token("JOIN", _)] ->
            join_parser 3 left FullOuterJoin
        | Some[Token("FULL", _); Token("JOIN", _); _] ->
            join_parser 2 left FullOuterJoin
        | Some[Token("LEFT", _); Token("OUTER", _); Token("JOIN", _)] ->
            join_parser 3 left LeftOuterJoin
        | Some[Token("LEFT", _); Token("JOIN", _); _] ->
            join_parser 2 left LeftOuterJoin
        | Some[Token("RIGHT", _); Token("OUTER", _); Token("JOIN", _)] ->
            join_parser 3 left RightOuterJoin
        | Some[Token("RIGHT", _); Token("JOIN", _); _] ->
            join_parser 2 left RightOuterJoin
        | _ -> result left
    and join_parser operator_length left join_kind =
      consume_many operator_length <+> (parse_table_with_alias ()) >>= fun right ->
        lookahead >>= function
          | Some(Token("ON", _)) ->
              (consume "ON" <+> (parse_expression ()) >>= fun expr ->
                 pte_iter (TableJoin(join_kind, left, right, Some expr, None),
                           combine_ast_pos left expr))
          | Some(Token("USING", _)) ->
              (consume "USING"
               <+> consume_or_fake "("
               <+> (sep_by "," string_item) >>= fun columns ->
                 consume_or_fake ")"
                 <+> get_previous_pos >>= fun end_pos ->
                   let (_, Pos(start_pos, _)) = left in
                     pte_iter (TableJoin(join_kind, left, right, None, Some columns),
                               Pos(start_pos, end_pos)))
          | _ ->
              warning "Expected 'ON' or 'USING'."
              <+> wrap_pos (result <| TableJoin(join_kind, left, right, None, None))
    in
      parse_table_with_alias () >>= fun left ->
        pte_iter left

  and parse_table_list () =
    sep_by "," (parse_table_expression ());;

  let parse_select = parse_select ();;

  let parse_expression = parse_expression ();;

  let parse_dotted_identifier = parse_dotted_identifier ();

end;;

open ExpressionAndSelect;;

let parse_type =
  let parse_number_type =
    consume "NUMBER" <+> lookahead >>= function
      | Some(Token("(", _)) ->
          (consume "(" <+> string_item >>= fun precision ->
             lookahead >>= function
               | Some(Token(",", _)) ->
                   (consume "," <+> string_item >>= fun scale ->
                      consume ")"
                      <+> (result <| Number(int_of_string(precision),
                                            int_of_string(scale))))
               | _ ->
                   (consume ")" <+> (result <| Number(int_of_string(precision), 0))))
      | _ -> result <| Number(38, 127)
  in
  let parse_varchar_type one_or_two =
    (if one_or_two = 1
     then consume "VARCHAR"
     else consume "VARCHAR2")
    <+> lookahead >>= function
      | Some(Token("(", _)) ->
          (consume "(" <+> string_item >>= fun size ->
             let varchar_type =
               if one_or_two = 1
               then Varchar(int_of_string(size))
               else  Varchar2(int_of_string(size))
             in
               consume ")" <+> result varchar_type)
      | _ ->
          (let varchar_type =
             if one_or_two = 1
             then VarcharNoSize
             else  Varchar2NoSize
           in
             result varchar_type)
  in
    wrap_pos (lookahead >>= function
                | Some(Token("NUMBER", _)) ->
                    parse_number_type
                | Some(Token("VARCHAR", _)) ->
                    parse_varchar_type 1
                | Some(Token("VARCHAR2", _)) ->
                    parse_varchar_type 2
                | Some(Token("DATE", _)) ->
                    consume "DATE" <+> (result <| Date)
                | _ ->
                    (parse_dotted_identifier >>= fun ident ->
                       lookahead >>= function
                         | Some(Token("%", _)) ->
                             consume "%" <+> (lookahead >>= function
                                                | Some(Token("ROWTYPE", _)) ->
                                                    consume "ROWTYPE"
                                                    <+> (result <| RowTypeAnchor(ident))
                                                | Some(Token("TYPE", _)) ->
                                                    consume "TYPE"
                                                    <+> (result <| TypeAnchor(ident))
                                                | _ ->
                                                    error "Expected 'TYPE' or 'ROWTYPE'.")
                         | _ ->
                             result <| TypeName(ident)))

let parse_is_as =
  lookahead >>= function
    | Some(Token("IS", _)) -> consume "IS" <+> result "IS"
    | Some(Token("AS", _)) -> consume "AS" <+> result "AS"
    | _ -> warning "Expected 'IS' or 'AS', inserted 'IS'." <+> result "IS";;

let parse_creation_type =
  lookahead_many 2 >>= function
    | Some([Token("OR", _); Token("REPLACE", _)]) ->
        (consume_many 2 <+> result CreateOrReplace)
    | _ ->
        result Create;;

let parse_arg_decl =
  wrap_pos (string_item >>= fun arg_name ->
              parse_type >>= fun arg_type ->
                result <| ArgDecl(arg_name, arg_type));;

let parse_subprogram_arguments =
  lookahead >>= function
    | Some(Token("(", _)) ->
        (consume "(" <+> sep_by "," parse_arg_decl >>= fun arguments ->
           consume ")" <+> (result arguments))
    | _ ->
        result [];;

let rec parse_statement () =
  lookahead >>= function
    | Some (Token("BEGIN", _))
    | Some (Token("DECLARE", _)) -> parse_block ()
    | Some (Token("SELECT", _)) -> parse_select_statement ()
    | Some (Token("IF", _)) -> parse_if_statement ()
    | Some (Token("LOOP", _)) -> parse_loop_statement ()
    | Some (Token("FOR", _)) -> parse_for_statement ()
    | Some (Token("WHILE", _)) -> parse_while_statement ()
    | Some (Token("GOTO", _)) -> parse_goto_statement ()
    | Some (Token("NULL", _)) -> parse_null_statement ()
    | Some (Token("<", _)) -> parse_label ()
    | Some (Token("EXIT", _)) ->
        let simple maybe_label = StmtExit(maybe_label) in
        let with_when cond maybe_label = StmtExitWhen(cond, maybe_label) in
          parse_exit_or_continue "EXIT" simple with_when
    | Some (Token("CONTINUE", _)) ->
        let simple maybe_label = StmtContinue(maybe_label) in
        let with_when cond maybe_label = StmtContinueWhen(cond, maybe_label) in
          parse_exit_or_continue "CONTINUE" simple with_when
    | Some (Token("CREATE", _)) -> parse_create ()
    | Some (Token("RETURN", _)) -> parse_return ()
    | Some (Token("OPEN", _)) -> parse_open ()
    | Some (Token("CLOSE", _)) -> parse_close ()
    | Some (Token("FETCH", _)) -> parse_fetch ()
        (* Ignored statements (from SQL specification). *)
    | Some (Token("ALTER", _))
    | Some (Token("ANALYZE", _))
    | Some (Token("ASSOCIATE", _))
    | Some (Token("AUDIT", _))
    | Some (Token("CALL", _))
    | Some (Token("COMMENT", _))
    | Some (Token("COMMIT", _))
    | Some (Token("DELETE", _))
    | Some (Token("DISASSOCIATE", _))
    | Some (Token("DROP", _))
    | Some (Token("EXECUTE", _))
    | Some (Token("EXPLAIN", _))
    | Some (Token("FLASHBACK", _))
    | Some (Token("GRANT", _))
    | Some (Token("INSERT", _))
    | Some (Token("LOCK", _))
    | Some (Token("MERGE", _))
    | Some (Token("NOAUDIT", _))
    | Some (Token("PURGE", _))
    | Some (Token("RENAME", _))
    | Some (Token("REVOKE", _))
    | Some (Token("ROLLBACK", _))
    | Some (Token("SAVEPOINT", _))
    | Some (Token("SET", _))
    | Some (Token("TRUNCATE", _))
    | Some (Token("UPDATE", _)) ->
        ignore_statement ()
    | _ -> parse_assignment_or_call ()

and parse_fetch () =
  let parse_fetch_inner cursor_name bulk_collect =
    consume_or_fake "INTO"
    <+> sep_by "," parse_dotted_identifier >>= fun vars ->
      let fetch_statement =
        if bulk_collect
        then StmtFetchBulkCollect(cursor_name, vars)
        else StmtFetch(cursor_name, vars)
      in
        parse_semicolon
        <+> (result fetch_statement)
  in
    wrap_pos (consume "FETCH" <+> parse_dotted_identifier >>= fun cursor_name ->
                lookahead_many 2 >>= function
                  | Some [Token("BULK", _); Token("COLLECT", _)] ->
                      consume_many 2 <+> parse_fetch_inner cursor_name true
                  | _ ->
                      parse_fetch_inner cursor_name false)

and parse_open () =
  wrap_pos (consume "OPEN" <+> parse_dotted_identifier >>= fun cursor_name ->
              lookahead >>= function
                | Some(Token("(", _)) ->
                    (consume "("
                     <+> sep_by "," parse_expression >>= fun exprs ->
                       consume ")"
                       <+> parse_semicolon
                       <+> (result <| StmtOpenParameterized(cursor_name, exprs)))
                | _ ->
                    parse_semicolon <+> (result <| StmtOpen(cursor_name)))

and parse_close () =
  wrap_pos (consume "CLOSE" <+> parse_dotted_identifier >>= fun cursor_name ->
              parse_semicolon <+> (result <| StmtClose(cursor_name)))

and ignore_statement () =
  let rec ignore_statement_iter nesting_level =
    item <+> lookahead >>= function
      | Some (Token(";", _)) when nesting_level = 0 ->
          item <+> (result ())
      | None ->
          result ()
      | Some (Token("BEGIN", _))
      | Some (Token("DECLARE", _))
      | Some (Token("SELECT", _))
      | Some (Token("IF", _))
      | Some (Token("LOOP", _)) ->
          if nesting_level = 0
          then result ()
          else ignore_statement_iter (nesting_level + 1)
      | Some (Token("FOR", _))
      | Some (Token("WHILE", _))
      | Some (Token("GOTO", _))
      | Some (Token("NULL", _))
      | Some (Token("<", _)) ->
          if nesting_level = 0
          then result ()
          else ignore_statement_iter nesting_level
      | Some (Token("END", _)) ->
          item <+> ignore_statement_iter (nesting_level - 1)
      | _ ->
          ignore_statement_iter nesting_level
  in
    wrap_pos (warning_not_implemented "Start of statement ignored by STAN."
              <+> (ignore_statement_iter 0)
              <+> warning_not_implemented "End of statement ignored by STAN."
              <+> (result StmtNull))

and parse_return () =
  wrap_pos (consume "RETURN" <+> lookahead >>= function
              | Some(Token(";", _)) ->
                  parse_semicolon <+> (result <| StmtReturn(None))
              | _ ->
                  parse_expression >>= function return_value ->
                    parse_semicolon <+> (result <| StmtReturn(Some return_value)))

and parse_create () =
  wrap_pos (consume "CREATE"
            <+> parse_creation_type >>= fun creation_type ->
              lookahead >>= function
                | Some(Token("PROCEDURE", _)) ->
                    parse_create_procedure creation_type
                | Some(Token("FUNCTION", _)) ->
                    parse_create_function creation_type
                | Some(Token("TABLE", _)) ->
                    parse_create_table ()
                | _ ->
                    fail)
  </>
    ignore_statement ()

and parse_field () =
  wrap_pos (string_item >>= fun column_name ->
              parse_type >>= fun column_type ->
                result <| FieldDecl(column_name, column_type))

and parse_create_table () =
  consume "TABLE"
  <+> parse_dotted_identifier >>= fun table_name ->
    consume_or_fake "(" <+> sep_by "," (parse_field ()) >>= fun columns ->
      consume_or_fake ")" <+> parse_semicolon
      <+> (result <| StmtCreateTable(table_name, columns))

and parse_create_function creation_type =
  consume "FUNCTION"
  <+> parse_dotted_identifier >>= fun name ->
    parse_subprogram_arguments >>= fun arguments ->
      consume_or_fake "RETURN" <+> parse_type >>= fun return_type ->
        parse_is_as >>= fun isas ->
          parse_block () >>= fun body ->
            result <| StmtCreateFunction(creation_type,
                                         name,
                                         arguments,
                                         return_type,
                                         isas,
                                         body)

and parse_create_procedure creation_type =
  consume "PROCEDURE"
  <+> parse_dotted_identifier >>= fun name ->
    parse_subprogram_arguments >>= fun arguments ->
      parse_is_as >>= fun isas ->
        parse_block () >>= fun body ->
          result <| StmtCreateProcedure(creation_type,
                                        name,
                                        arguments,
                                        isas,
                                        body)

and parse_null_statement () =
  wrap_pos (consume "NULL" <+> parse_semicolon <+> (result StmtNull))

and parse_while_statement () =
  wrap_pos (consume "WHILE" <+> parse_expression >>= fun expr ->
              parse_loop_statement () >>= fun loop ->
                result <| StmtWhile(expr, loop))

and parse_goto_statement () =
  wrap_pos (consume "GOTO" <+> parse_dotted_identifier >>= fun label ->
              parse_semicolon <+> (result <| StmtGoto(label)))

and parse_for_statement () =
  let parse_for index_variable reversed =
    parse_expression >>= fun index_start ->
      lookahead >>= function
        | Some(Token(".", _)) ->
            (consume "." <+> consume "." <+> parse_expression >>= fun index_end ->
               lookahead >>= function
                 | Some(Token("LOOP", _)) ->
                     (parse_loop_statement () >>= fun loop_statement ->
                        result <| StmtFor(index_variable,
                                          reversed,
                                          index_start,
                                          index_end,
                                          loop_statement))
                 | _ -> error "Expected 'LOOP'.")
        | Some(Token("LOOP", _)) ->
            (parse_loop_statement () >>= fun loop_statement ->
               match index_start with
                 | Call(called, params), _ ->
                     result <| StmtForParameterizedCursor(index_variable,
                                                          reversed,
                                                          called,
                                                          params,
                                                          loop_statement)
                 | _ ->
                     result <| StmtForCursor(index_variable,
                                             reversed,
                                             index_start,
                                             loop_statement))
        | _ ->
            error "Expected 'LOOP' or '..'."
  in
    wrap_pos (consume "FOR" <+> parse_dotted_identifier >>= fun index_variable ->
                consume "IN" <+> lookahead >>= function
                  | Some(Token("REVERSE", _)) ->
                      consume "REVERSE" <+> parse_for index_variable true
                  | _ ->
                      parse_for index_variable false)

and parse_exit_or_continue exit_continue simple with_when  =
  wrap_pos (consume exit_continue <+> lookahead >>= function
              | Some (Token("WHEN", _)) ->
                  (consume "WHEN" <+> parse_expression >>= fun cond ->
                     parse_semicolon <+> (result <| with_when cond None))
              | Some (Token(label, _)) when label <> ";" ->
                  (consume label <+> lookahead >>= function
                     | Some (Token("WHEN", _)) ->
                         (consume "WHEN" <+> parse_expression >>= fun cond ->
                            parse_semicolon <+> (result <| with_when cond (Some label)))
                     | _ ->
                         (parse_semicolon <+> (result <| simple (Some label))))
              | _ ->
                  (parse_semicolon <+> (result <| simple None)))

and parse_label () =
  wrap_pos (consume "<" <+> consume "<" <+> string_item >>= fun label ->
              consume ">" <+> consume ">" <+> (parse_statement ()) >>= fun statement ->
                result <| StmtLabeled(label, statement))

and parse_loop_statement () =
  wrap_pos (consume "LOOP" <+> until ["END"] (parse_statement ()) >>= fun loop_statements ->
              consume "END" <+> consume_or_fake "LOOP" <+> lookahead >>= function
                | Some (Token(content, _)) when content <> ";" ->
                    (consume content <+> parse_semicolon
                     <+> (result <| StmtLoop(loop_statements, Some content)))
                | _ ->
                    (parse_semicolon <+> (result <| StmtLoop(loop_statements, None))))

and parse_if_statement () =
  let rec parse_after_if () =
    parse_expression >>= fun condition ->
      consume_or_fake "THEN"
      <+> until ["END"; "ELSIF"; "ELSE" ] (parse_statement ()) >>= fun then_statements ->
        lookahead >>= function
          | Some (Token("END", _)) ->
              (consume "END"
               <+> consume_or_fake "IF"
               <+> parse_semicolon
               <+> (result (condition, then_statements, NoElse)))
          | Some (Token("ELSE", _)) ->
              (consume "ELSE"
               <+> until ["END"] (parse_statement ()) >>= fun else_statements ->
                 consume "END"
                 <+> consume_or_fake "IF"
                 <+> parse_semicolon
                 <+> (result (condition, then_statements, Else else_statements)))
          | Some (Token("ELSIF", _)) ->
              (consume "ELSIF"
               <+> parse_after_if () >>= fun if_args ->
                 result (condition, then_statements, ElsIf if_args))
          | _ ->
              warning "'END' or 'ELSE' or 'ELSIF' expected."
              <+> (result (condition, then_statements, NoElse))
  in
    wrap_pos (consume "IF" <+> parse_after_if () >>= fun if_args ->
                result <| StmtIf if_args)

and parse_select_statement () =
  wrap_pos (parse_select >>= fun select ->
              parse_semicolon <+> (result <| StmtSelect (select)))

and parse_block () =
  let parse_cursor_decl =
    consume "CURSOR"
    <+> parse_dotted_identifier >>= fun cursor_name ->
      lookahead >>= function
        | Some(Token("IS", _)) ->
            (consume_or_fake "IS"
             <+> parse_select >>= fun select_expression ->
               parse_semicolon
               <+> (result <| CursorDecl(cursor_name, select_expression)))
        | Some(Token("(", _)) ->
            (consume_or_fake "("
             <+> sep_by "," parse_arg_decl >>= fun cursor_arguments ->
               consume_or_fake ")"
               <+> consume_or_fake "IS"
               <+> parse_select >>= fun select_expression ->
                 parse_semicolon
                 <+> (result <| ParameterizedCursorDecl(cursor_name,
                                                        cursor_arguments,
                                                        select_expression)))
        | _ ->
            error "Expected 'IS' or '('."
  in
  let parse_type_decl =
    let parse_record_decl type_name =
      let parse_record_field =
        parse_field () >>= fun field ->
          parse_semicolon <+> (result field)
      in
        consume "RECORD"
        <+> consume_or_fake "("
        <+> until [")"] parse_record_field >>= fun members ->
          consume_or_fake ")"
          <+> parse_semicolon
          <+> (result <| RecordDecl(type_name, members))
    in
    let parse_table_decl type_name =
      consume "TABLE"
      <+> consume_or_fake "OF"
      <+> parse_type >>= fun table_type ->
        parse_semicolon
        <+> (result <| TableDecl(type_name, table_type))
    in
      consume "TYPE" <+> parse_dotted_identifier >>= fun type_name ->
        consume "IS" <+> lookahead >>= function
          | Some(Token("RECORD", _)) ->
              parse_record_decl type_name
          | Some(Token("TABLE", _)) ->
              parse_table_decl type_name
          | _ -> error "Unknown kind of type."
  in
  let parse_vardecl =
    wrap_pos (lookahead >>= function
                | Some(Token("TYPE", _)) ->
                    parse_type_decl
                | Some(Token("CURSOR", _)) ->
                    parse_cursor_decl
                | _ ->
                    string_item >>= fun variable ->
                      parse_type >>= fun var_type ->
                        parse_semicolon <+>
                          (result (VarDecl(variable, var_type))))
  in
  let parse_begin_end declarations =
    consume "BEGIN" >>= fun start_token2 ->
      until ["END"] (parse_statement ()) >>= fun statements ->
        consume "END" <+> parse_semicolon <+>
          (result (Block(declarations, statements)))
  in
    wrap_pos (lookahead >>= fun content ->
                match content with
                  | Some(Token("DECLARE", _)) ->
                      (consume "DECLARE" >>= fun start_token ->
                         until ["BEGIN"] parse_vardecl >>= fun declarations ->
                           parse_begin_end declarations)
                  | _ ->
                      parse_begin_end [])

and parse_assignment_or_call () =
  let parse_assignment var_name =
    consume ":" <+> consume "=" <+> parse_expression >>= fun expression ->
      parse_semicolon <+>
        result (StmtAssignment(var_name, expression))
  in
  let parse_call subprogram_name =
    consume "(" <+> sep_by "," parse_expression >>= fun args ->
      consume_or_fake ")"
      <+> parse_semicolon
      <+> (result <| StmtCall(subprogram_name, args))
  in
    wrap_pos (parse_dotted_identifier >>= fun name ->
                lookahead >>= function
                  | Some (Token(":", _)) ->
                      parse_assignment name
                  | _ ->
                      parse_call name)

let plsql_parser =
  until_eoi <| parse_statement () >>= fun statements ->
    result <| (Program(statements), (extract_limits statements));;

let run_parser_helper parser tokens =
  run_parser parser (Stream(None, tokens), []);;

(* The PLSQL parser. Should be the only public function in this
   module. *)
let parse tokens =
  run_parser_helper plsql_parser tokens;;

(* Convertor of positions reported in errors. Error list must be
   sorted in ascending order by position. *)
let convert_error_positions text errors =
  let positions = List.map (fun (Warning(_, _, pos)) -> pos) errors in
  let line_cols = convert_pos_to_line_col text positions in
    List.map2
      (fun (Warning(kind, msg, _)) pos -> (kind, msg, pos))
      errors
      line_cols;;

(* Helper functions for testing/debugging. *)
let parse2 str =
  let tokens, _ = tokenize str 0 in
  let rev_errors, ast = parse tokens in
  let proper_errors = convert_error_positions str (List.rev rev_errors) in
    proper_errors, ast;;

let parse_expr tokens =
  run_parser_helper parse_expression tokens;;

let parse_select_helper tokens =
  run_parser_helper parse_select tokens;;

let parse_select2 str =
  let tokens, _ = tokenize str 0 in
    parse_select_helper tokens;;

let parse_expr2 str =
  let tokens, _ = tokenize str 0 in
    parse_expr tokens

let time_it n p =
  let rec repeat n p =
    if n = 0
    then 0
    else
      let _ = p () in
        repeat (n - 1) p
  in
  let t1 = Sys.time () in
  let _ = repeat n p in
  let t2 = Sys.time () in
    t2 -. t1;;

let time_parse n p str =
  time_it n (fun _ -> p str);;
