
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
