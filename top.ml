#load "utils.cmo";;
#load "pwm.cmo";;
#load "parserTypes.cmo";;
#load "lexer.cmo";;
#load "plsqlParser.cmo";;
#load "acm.cmo";;
#load "absint.cmo";;
#load "absintCompiler.cmo";;

open PlsqlParser;;
open Lexer;;
open ParserTypes;;
open Pwm;;
open Absint.Ir;;
open AbsintCompiler;;

#use "topfind";;
#require "oUnit";;

open OUnit;;

#print_depth 10000;;
#print_length 10000;;

