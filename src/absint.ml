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

open Utils;;
open Printf;;
open ParserTypes;;
open PlsqlParser;;
open PlsqlParser.Ast;;

module Ir =
struct

  type ir =
    | AddFrame
    | DeleteFrame
    | Goto of string * plsql_ast_with_pos option
    | GotoIf of expression_ast_with_pos * string * string
    | Label of string
    | Assignment of expression_ast_with_pos * expression_ast_with_pos
    | Declare of string * typ_with_pos
    | Call of expression_ast_with_pos * expression_ast_with_pos list

end;;
