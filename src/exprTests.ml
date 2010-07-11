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

open OUnit;;
open ParserTypes;;
open Lexer;;
open PlsqlParser;;
open Pwm;;
open TestsCommon;;
open Ast;;

let test_parse_expr_helper = parse_helper parse_expr;;

let test_parse_expression_numeric_literal_1 () =
  let expected = (NumericLiteral "100", Pos (0, 2)) in
    test_parse_expr_helper "100" [] expected

let test_parse_expression_numeric_literal_2 () =
  let expected = (UnaryOp ("-",
                           (NumericLiteral "100", Pos (1, 3))),
                  Pos (0, 3))
  in
    test_parse_expr_helper "-100" [] expected

let test_parse_expression_numeric_literal_3 () =
  let expected = (UnaryOp ("+",
                           (NumericLiteral "100", Pos (1, 3))),
                  Pos (0, 3))
  in
    test_parse_expr_helper "+100" [] expected

let test_parse_expression_numeric_literal_4 () =
  let expected = (UnaryOp ("-",
                           (UnaryOp ("+",
                                     (NumericLiteral "100", Pos (2, 4))),
                            Pos (1, 4))),
                  Pos (0, 4))
  in
    test_parse_expr_helper "-+100" [] expected

let test_parse_expression_string_literal_1 () =
  let expected = (StringLiteral "'ABC'", Pos (0, 4))
  in
    test_parse_expr_helper "'ABC'" [] expected

let test_parse_expression_simple_1 () =
  let expected = (BinaryOp("+",
                           (NumericLiteral "1", Pos(0, 0)),
                           (NumericLiteral "2", Pos(4, 4))),
                  Pos(0, 4)) in
    test_parse_expr_helper "1 + 2" [] expected

let test_parse_expression_simple_2 () =
  let mul_tree = (BinaryOp ("*",
                            (NumericLiteral "2", Pos(4, 4)),
                            (NumericLiteral "3", Pos(8, 8))),
                  Pos(4, 8)) in
  let expected = (BinaryOp("+",
                           (NumericLiteral "1", Pos(0, 0)),
                           mul_tree),
                  Pos(0, 8)) in
    test_parse_expr_helper "1 + 2 * 3" []expected

let test_parse_expression_simple_3 () =
  let second_sum = (BinaryOp ("+", (NumericLiteral "1", Pos (0, 0)),
                              (NumericLiteral "2", Pos (4, 4))),
                    Pos (0, 4)) in
  let expected = (BinaryOp ("+",
                            second_sum,
                            (NumericLiteral "3", Pos (8, 8))),
                  Pos (0, 8))
  in
    test_parse_expr_helper "1 + 2 + 3" [] expected

let test_parse_expression_simple_4 () =
  let expected = (BinaryOp ("-",
                            (BinaryOp ("+", (NumericLiteral "1", Pos (0, 0)),
                                       (NumericLiteral "2", Pos (4, 4))),
                             Pos (0, 4)),
                            (NumericLiteral "3", Pos (8, 8))),
                  Pos (0, 8))
  in
    test_parse_expr_helper "1 + 2 - 3" [] expected

let test_parse_expression_simple_5 () =
  let expected = (BinaryOp ("+",
                            (BinaryOp ("/", (NumericLiteral "1", Pos (0, 0)),
                                       (NumericLiteral "2", Pos (4, 4))),
                             Pos (0, 4)),
                            (BinaryOp ("*", (NumericLiteral "2", Pos (8, 8)),
                                       (NumericLiteral "3", Pos (12, 12))),
                             Pos (8, 12))),
                  Pos (0, 12))
  in
    test_parse_expr_helper "1 / 2 + 2 * 3" [] expected

let test_parse_expression_logical_1 () =
  let expected = (BinaryOp ("OR",
                            (BinaryOp ("<",
                                       (NumericLiteral "1", Pos (0, 0)),
                                       (NumericLiteral "2", Pos (4, 4))),
                             Pos (0, 4)),
                            (BinaryOp ("AND",
                                       (BinaryOp (">",
                                                  (BinaryOp ("+",
                                                             (NumericLiteral "2",
                                                              Pos (9, 9)),
                                                             (NumericLiteral "2",
                                                              Pos (13, 13))),
                                                   Pos (9, 13)),
                                                  (NumericLiteral "3", Pos (17, 17))),
                                        Pos (9, 17)),
                                       (BinaryOp ("<",
                                                  (NumericLiteral "2", Pos (23, 23)),
                                                  (NumericLiteral "3", Pos (27, 27))),
                                        Pos (23, 27))),
                             Pos (9, 27))),
                  Pos (0, 27))
  in
    test_parse_expr_helper "1 < 2 OR 2 + 2 > 3 AND 2 < 3" [] expected

let test_parse_expression_logical_2 () =
  let expected = (BinaryOp ("OR",
                            (BinaryOp ("<",
                                       (NumericLiteral "1", Pos (0, 0)),
                                       (NumericLiteral "2", Pos (4, 4))),
                             Pos (0, 4)),
                            (BinaryOp ("AND",
                                       (UnaryOp ("NOT",
                                                 (BinaryOp ("<",
                                                            (BinaryOp ("+",
                                                                       (NumericLiteral "2",
                                                                        Pos (13, 13)),
                                                                       (NumericLiteral "2",
                                                                        Pos (17, 17))),
                                                             Pos (13, 17)),
                                                            (NumericLiteral "3",
                                                             Pos (21, 21))),
                                                  Pos (13, 21))),
                                        Pos (9, 21)),
                                       (BinaryOp ("<",
                                                  (NumericLiteral "2", Pos (27, 27)),
                                                  (NumericLiteral "3", Pos (31, 31))),
                                        Pos (27, 31))),
                             Pos (9, 31))),
                  Pos (0, 31))
  in
    test_parse_expr_helper "1 < 2 OR NOT 2 + 2 < 3 AND 2 < 3" [] expected

let test_parse_expression_logical_3 () =
  let expected = (BinaryOp ("OR",
                            (BinaryOp ("=",
                                       (NumericLiteral "1", Pos (0, 0)),
                                       (NumericLiteral "2", Pos (4, 4))),
                             Pos (0, 4)),
                            (BinaryOp ("AND",
                                       (UnaryOp ("NOT",
                                                 (BinaryOp ("<>",
                                                            (BinaryOp ("+",
                                                                       (NumericLiteral "2",
                                                                        Pos (13, 13)),
                                                                       (NumericLiteral "2",
                                                                        Pos (17, 17))),
                                                             Pos (13, 17)),
                                                            (NumericLiteral "3",
                                                             Pos (22, 22))),
                                                  Pos (13, 22))),
                                        Pos (9, 22)),
                                       (BinaryOp ("<",
                                                  (NumericLiteral "2", Pos (28, 28)),
                                                  (NumericLiteral "3", Pos (32, 32))),
                                        Pos (28, 32))),
                             Pos (9, 32))),
                  Pos (0, 32))
  in
    test_parse_expr_helper "1 = 2 OR NOT 2 + 2 <> 3 AND 2 < 3" [] expected

let test_parse_expression_parens_1 () =
  let expected = (BinaryOp ("*",
                            (BinaryOp ("+",
                                       (NumericLiteral "1", Pos (1, 1)),
                                       (NumericLiteral "2", Pos (5, 5))),
                             Pos (0, 6)),
                            (NumericLiteral "3", Pos (10, 10))),
                  Pos (0, 10))
  in
    test_parse_expr_helper "(1 + 2) * 3" [] expected

let test_parse_expression_parens_2 () =
  let expected = (BinaryOp ("*",
                            (BinaryOp ("+",
                                       (NumericLiteral "1", Pos (1, 1)),
                                       (NumericLiteral "2", Pos (5, 5))),
                             Pos (0, 6)),
                            (BinaryOp ("-",
                                       (NumericLiteral "3", Pos (11, 11)),
                                       (NumericLiteral "4", Pos (15, 15))),
                             Pos (10, 16))),
                  Pos (0, 16))
  in
    test_parse_expr_helper "(1 + 2) * (3 - 4)" [] expected

let test_parse_expression_function_1 () =
  let expected = (BinaryOp ("+",
                            (Call
                               ((Identifier "NVL", Pos (0, 2)),
                                [(Identifier "A", Pos (4, 4));
                                 (BinaryOp ("+",
                                            (NumericLiteral "1", Pos (7, 7)),
                                            (NumericLiteral "2", Pos (11, 11))),
                                  Pos (7, 11))]),
                             Pos (0, 12)),
                            (Call
                               ((Identifier "MAX", Pos (16, 18)),
                                [(NumericLiteral "3", Pos (20, 20));
                                 (NumericLiteral "4", Pos (23, 23));
                                 (Identifier "B", Pos (26, 26))]),
                             Pos (16, 27))),
                  Pos (0, 27))
  in
    test_parse_expr_helper "NVL(a, 1 + 2) + MAX(3, 4, b)" [] expected

let test_parse_expression_is_null () =
  let expected_1 = (BinaryOp ("AND",
                              (IsNull
                                 (Identifier "A", Pos (0, 0)),
                               Pos (0, 8)),
                              (BinaryOp ("=",
                                         (Identifier "A", Pos (14, 14)),
                                         (Identifier "C", Pos (18, 18))),
                               Pos (14, 18))),
                    Pos (0, 18))
  in
  let expected_2 = (BinaryOp ("AND",
                              (IsNotNull
                                 (Identifier "B", Pos (0, 0)),
                               Pos (0, 12)),
                              (BinaryOp ("=",
                                         (Identifier "A", Pos (18, 18)),
                                         (Identifier "C", Pos (22, 22))),
                               Pos (18, 22))),
                    Pos (0, 22))
  in
    test_parse_expr_helper "a IS NULL AND a = c" [] expected_1;
    test_parse_expr_helper "b IS NOT NULL AND a = c" [] expected_2;;

let test_parse_expression_like () =
  let expected = (BinaryOp ("AND",
                            (Like
                               ((Identifier "A", Pos (0, 0)),
                                (StringLiteral "'Test%'", Pos (7, 13))),
                             Pos (0, 13)),
                            (Like
                               ((Identifier "B", Pos (19, 19)),
                                (BinaryOp ("+",
                                           (Identifier "A", Pos (26, 26)),
                                           (NumericLiteral "2", Pos (30, 30))),
                                 Pos (26, 30))),
                             Pos (19, 30))),
                  Pos (0, 30))
  in
    test_parse_expr_helper "A LIKE \'Test%\' AND B LIKE A + 2" [] expected;;

let test_parse_expression_between () =
  let expected_1 = (Between
                    ((BinaryOp ("/",
                                (NumericLiteral "1", Pos (0, 0)),
                                (NumericLiteral "3", Pos (4, 4))),
                      Pos (0, 4)),
                     (BinaryOp ("+",
                                (NumericLiteral "4", Pos (14, 14)),
                                (Identifier "A", Pos (18, 18))),
                      Pos (14, 18)),
                     (BinaryOp ("-",
                                (NumericLiteral "8", Pos (24, 24)),
                                (NumericLiteral "2", Pos (28, 28))),
                      Pos (24, 28))),
                  Pos (0, 28))
  in
  let expected_2 = (NotBetween
                      ((BinaryOp ("/",
                                  (NumericLiteral "1", Pos (0, 0)),
                                  (NumericLiteral "3", Pos (4, 4))),
                        Pos (0, 4)),
                       (BinaryOp ("+",
                                  (NumericLiteral "4", Pos (18, 18)),
                                  (Identifier "A", Pos (22, 22))),
                        Pos (18, 22)),
                       (BinaryOp ("-",
                                  (NumericLiteral "8", Pos (28, 28)),
                                  (NumericLiteral "2", Pos (32, 32))),
                        Pos (28, 32))),
                    Pos (0, 32))
  in
    test_parse_expr_helper "1 / 3 BETWEEN 4 + A AND 8 - 2" [] expected_1;
    test_parse_expr_helper "1 / 3 NOT BETWEEN 4 + A AND 8 - 2" [] expected_2;;

let test_parse_expression_subquery () =
  let expected = (Subquery
                    (Select
                       {fields =
                           [(Column
                               (Identifier "A", Pos (8, 8)),
                             Pos (8, 8))];
                        clauses =
                           [(FromClause
                               [(TableName "TABLE", Pos (15, 19))],
                             Pos (10, 19))]},
                     Pos (1, 19)),
                  Pos (0, 20))
  in
    test_parse_expr_helper "(SELECT a FROM table)" [] expected;;

let test_parse_expression_exists () =
  let expected = (Exists
                    (Subquery
                       (Select
                          {fields =
                              [(Column (Identifier "A", Pos (15, 15)), Pos (15, 15))];
                           clauses =
                              [(FromClause
                                  [(TableName "TABLE", Pos (22, 26))], Pos (17, 26))]},
                        Pos (8, 26)),
                     Pos (7, 27)),
                  Pos (0, 27))
  in
    test_parse_expr_helper "EXISTS (SELECT a FROM table)" [] expected;;

let test_parse_expression_in () =
  let expected_1 = (In ((Identifier "A", Pos (0, 0)),
                      (Subquery
                         (Select
                            {fields =
                                [(Column (Identifier "A", Pos (13, 13)), Pos (13, 13))];
                             clauses =
                                [(FromClause
                                    [(TableName "TABLE", Pos (20, 24))],
                                  Pos (15, 24))]},
                          Pos (6, 24)),
                       Pos (5, 25))),
                  Pos (0, 25))
  in
  let expected_2 = (NotIn ((Identifier "A", Pos (0, 0)),
                           (Subquery
                              (Select
                                 {fields =
                                     [(Column (Identifier "A", Pos (17, 17)), Pos (17, 17))];
                                  clauses =
                                     [(FromClause
                                         [(TableName "TABLE", Pos (24, 28))],
                                       Pos (19, 28))]},
                               Pos (10, 28)),
                            Pos (9, 29))),
                    Pos (0, 29))
  in
    test_parse_expr_helper "A IN (SELECT a FROM table)" [] expected_1;
    test_parse_expr_helper "A NOT IN (SELECT a FROM table)" [] expected_2;;

let test_parse_expression_any () =
  let expected = (BinaryOp (">=",
                            (Identifier "A", Pos (0, 0)),
                            (Any
                               (Subquery
                                  (Select
                                     {fields =
                                         [(Column
                                             (Identifier "A", Pos (17, 17)),
                                           Pos (17, 17))];
                                      clauses =
                                         [(FromClause
                                             [(TableName "T", Pos (24, 24))],
                                           Pos (19, 24))]},
                                   Pos (10, 24)),
                                Pos (9, 25)),
                             Pos (5, 25))),
                  Pos (0, 25))
  in
    test_parse_expr_helper "A >= ANY (SELECT A FROM T)" [] expected;;

let test_parse_expression_all () =
  let expected = (BinaryOp (">=",
                            (Identifier "A", Pos (0, 0)),
                            (All
                               (Subquery
                                  (Select
                                     {fields =
                                         [(Column
                                             (Identifier "A", Pos (17, 17)),
                                           Pos (17, 17))];
                                      clauses =
                                         [(FromClause
                                             [(TableName "T", Pos (24, 24))],
                                           Pos (19, 24))]},
                                   Pos (10, 24)),
                                Pos (9, 25)),
                             Pos (5, 25))),
                  Pos (0, 25))
  in
    test_parse_expr_helper "A >= ALL (SELECT A FROM T)" [] expected;;

let test_parse_expression_some () =
  let expected = (BinaryOp (">=",
                            (Identifier "A", Pos (0, 0)),
                            (SqlSome
                               (Subquery
                                  (Select
                                     {fields =
                                         [(Column
                                             (Identifier "A", Pos (18, 18)),
                                           Pos (18, 18))];
                                      clauses =
                                         [(FromClause
                                             [(TableName "T", Pos (25, 25))],
                                           Pos (20, 25))]},
                                   Pos (11, 25)),
                                Pos (10, 26)),
                             Pos (5, 26))),
                  Pos (0, 26))
  in
    test_parse_expr_helper "A >= Some (SELECT A FROM T)" [] expected;;

let test_parse_expression_expr_list () =
  let expected = (BinaryOp (">=",
                            (Identifier "A", Pos (0, 0)),
                            (SqlSome
                               (List
                                  [(Identifier "B", Pos (11, 11));
                                   (Identifier "C", Pos (14, 14));
                                   (Identifier "D", Pos (17, 17))],
                                Pos (10, 18)),
                             Pos (5, 18))),
                  Pos (0, 18))
  in
    test_parse_expr_helper "A >= Some (B, C, D)" [] expected;;

let test_parse_expression_case_simple_1 () =
  let expected = (SimpleCase
                    ((Identifier "A", Pos (5, 5)),
                     [CaseWhen
                        ((NumericLiteral "100", Pos (12, 14)),
                         (StringLiteral "'a'", Pos (21, 23)));
                      CaseWhen
                        ((NumericLiteral "200", Pos (30, 32)),
                         (StringLiteral "'b'", Pos (39, 41)))],
                     Some (StringLiteral "'c'", Pos (48, 50))),
                  Pos (0, 54))
  in
    test_parse_expr_helper
      "CASE a WHEN 100 THEN 'a' WHEN 200 THEN 'b' ELSE 'c' END"
      []
      expected;;

let test_parse_expression_case_simple_2 () =
  let expected = (SimpleCase
                    ((Identifier "A", Pos (5, 5)),
                     [CaseWhen
                        ((NumericLiteral "100", Pos (12, 14)),
                         (StringLiteral "'a'", Pos (21, 23)));
                      CaseWhen
                        ((NumericLiteral "200", Pos (30, 32)),
                         (StringLiteral "'b'", Pos (39, 41)))],
                     None),
                  Pos (0, 45))
  in
    test_parse_expr_helper
      "CASE a WHEN 100 THEN 'a' WHEN 200 THEN 'b' END"
      []
      expected;;

let test_parse_expression_searched_case_1 () =
  let expected = (SearchedCase
                    ([CaseWhen
                        ((BinaryOp ("<",
                                    (Identifier "A", Pos (10, 10)),
                                    (NumericLiteral "100", Pos (14, 16))),
                          Pos (10, 16)),
                         (StringLiteral "'a'", Pos (23, 25)));
                      CaseWhen
                        ((BinaryOp ("<",
                                    (Identifier "A", Pos (32, 32)),
                                    (NumericLiteral "200", Pos (36, 38))),
                          Pos (32, 38)),
                         (StringLiteral "'b'", Pos (45, 47)))],
                     Some (StringLiteral "'c'", Pos (54, 56))),
                  Pos (0, 60))
  in
    test_parse_expr_helper
      "CASE WHEN a < 100 THEN 'a' WHEN a < 200 THEN 'b' ELSE 'c' END"
      []
      expected;;

let test_parse_expression_searched_case_2 () =
  let expected = (SearchedCase
                    ([CaseWhen
                        ((BinaryOp ("<",
                                    (Identifier "A", Pos (10, 10)),
                                    (NumericLiteral "100", Pos (14, 16))),
                          Pos (10, 16)),
                         (StringLiteral "'a'", Pos (23, 25)));
                      CaseWhen
                        ((BinaryOp ("<",
                                    (Identifier "A", Pos (32, 32)),
                                    (NumericLiteral "200", Pos (36, 38))),
                          Pos (32, 38)),
                         (StringLiteral "'b'", Pos (45, 47)))],
                     None),
                  Pos (0, 51))
  in
    test_parse_expr_helper
      "CASE WHEN a < 100 THEN 'a' WHEN a < 200 THEN 'b' END"
      []
      expected;;

let test_parse_expression_concat () =
  let expected = (BinaryOp ("||",
                            (StringLiteral "'Prefix_'", Pos (0, 8)),
                            (BinaryOp ("+",
                                       (Identifier "A", Pos (14, 14)),
                                       (NumericLiteral "2", Pos (18, 18))),
                             Pos (13, 19))),
                  Pos (0, 19))
  in
    test_parse_expr_helper
      "'Prefix_' || (a + 2)"
      []
      expected;;

let suite = "Expression tests" >::: ["test_parse_expression_numeric_literal_1" >::
                                       test_parse_expression_numeric_literal_1;
                                     "test_parse_expression_numeric_literal_2" >::
                                       test_parse_expression_numeric_literal_2;
                                     "test_parse_expression_numeric_literal_3" >::
                                       test_parse_expression_numeric_literal_3;
                                     "test_parse_expression_numeric_literal_4" >::
                                       test_parse_expression_numeric_literal_4;
                                     "test_parse_expression_string_literal_1" >::
                                       test_parse_expression_string_literal_1;
                                     "test_parse_expression_simple_1" >::
                                       test_parse_expression_simple_1;
                                     "test_parse_expression_simple_2" >::
                                       test_parse_expression_simple_2;
                                     "test_parse_expression_simple_3" >::
                                       test_parse_expression_simple_3;
                                     "test_parse_expression_simple_4" >::
                                       test_parse_expression_simple_4;
                                     "test_parse_expression_simple_5" >::
                                       test_parse_expression_simple_5;
                                     "test_parse_expression_logical_1" >::
                                       test_parse_expression_logical_1;
                                     "test_parse_expression_logical_2" >::
                                       test_parse_expression_logical_2;
                                     "test_parse_expression_logical_3" >::
                                       test_parse_expression_logical_3;
                                     "test_parse_expression_parens_1" >::
                                       test_parse_expression_parens_1;
                                     "test_parse_expression_parens_2" >::
                                       test_parse_expression_parens_2;
                                     "test_parse_expression_function_1" >::
                                       test_parse_expression_function_1;
                                     "test_parse_expression_is_null" >::
                                       test_parse_expression_is_null;
                                     "test_parse_expression_like" >::
                                       test_parse_expression_like;
                                     "test_parse_expression_between" >::
                                       test_parse_expression_between;
                                     "test_parse_expression_subquery" >::
                                       test_parse_expression_subquery;
                                     "test_parse_expression_exists" >::
                                       test_parse_expression_exists;
                                     "test_parse_expression_in" >::
                                       test_parse_expression_in;
                                     "test_parse_expression_any" >::
                                       test_parse_expression_any;
                                     "test_parse_expression_all" >::
                                       test_parse_expression_all;
                                     "test_parse_expression_some" >::
                                       test_parse_expression_some;
                                     "test_parse_expression_expr_list" >::
                                       test_parse_expression_expr_list;
                                     "test_parse_expression_case_simple_1" >::
                                       test_parse_expression_case_simple_1;
                                     "test_parse_expression_case_simple_2" >::
                                       test_parse_expression_case_simple_2;
                                     "test_parse_expression_searched_case_1" >::
                                       test_parse_expression_searched_case_1;
                                     "test_parse_expression_searched_case_2" >::
                                       test_parse_expression_searched_case_2;
                                     "test_parse_expression_concat" >::
                                       test_parse_expression_concat;
                                    ]
