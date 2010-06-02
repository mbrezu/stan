
open ParserTypes;;
open Lexer;;
open PlsqlParser;;
open Pwm;;
open Ast;;

let time_spent =
  time_parse 20000 parse2 "BEGIN EXIT WHEN a < b AND b < c; a := a + 1; END;";;

print_float time_spent;;

print_newline ();;
