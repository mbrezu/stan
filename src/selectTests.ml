
open OUnit;;
open ParserTypes;;
open Lexer;;
open PlsqlParser;;
open Pwm;;
open TestsCommon;;
open Ast;;

let test_parse_select_helper = parse_helper parse_select_helper;;

let test_parse_simple_select_1 () =
  let expected = (Select ({fields = [(Column (Identifier "*", Pos (7, 7)), Pos(7, 7))];
                           clauses =
                              [FromClause [(TableName "TABLE", Pos (14, 18))],
                               Pos (9, 18)]}),
                  Pos (0, 18))
  in
    test_parse_select_helper "SELECT * FROM table" [] expected;
    test_parse_select_helper "select * from table" [] expected;;

let test_parse_simple_select_2 () =
  let expected = (Select ({fields =
                              [Column (Identifier "*", Pos (7, 7)), Pos(7, 7);
                               Column (Identifier "ORDERID", Pos (10, 16)), Pos(10, 16)];
                           clauses =
                              [FromClause
                                 [(TableName "TABLE", Pos (23, 27));
                                  (TableName "ORDERS", Pos (30, 35))],
                               Pos (18, 35)]}),
                  Pos (0, 35))
  in
    test_parse_select_helper "SELECT *, OrderId FROM table, orders" [] expected

let test_parse_simple_select_3 () =
  let expected = (Select ({fields =
                              [Column (BinaryOp (".",
                                                 (Identifier "O", Pos (7, 7)),
                                                 (Identifier "ORDERID", Pos (9, 15))),
                                       Pos (7, 15)), Pos(7, 15)];
                           clauses =
                              [FromClause
                                 [(TableAlias ("ORDERS", "O"), Pos (22, 29))],
                               Pos (17, 29)]}),
                  Pos (0, 29))
  in
    test_parse_select_helper "SELECT o.OrderId FROM orders o" [] expected

let test_parse_simple_select_4 () =
  let expected = (Select ({fields =
                              [Column (BinaryOp ("+",
                                                 (NumericLiteral "1", Pos (7, 7)),
                                                 (NumericLiteral "2", Pos (11, 11))),
                                       Pos (7, 11)), Pos(7, 11)];
                           clauses =
                              [FromClause [(TableName "DUAL", Pos (18, 21))],
                               Pos (13, 21)]}),
                  Pos (0, 21))
  in
    test_parse_select_helper "SELECT 1 + 2 FROM dual" [] expected

let test_parse_alias_1 () =
  let expected = (Select
                    {fields =
                        [(ColumnAlias ("AS",
                                       (BinaryOp ("+", (NumericLiteral "1", Pos (7, 7)),
                                                  (NumericLiteral "2", Pos (11, 11))),
                                        Pos (7, 11)),
                                       "SUM"),
                          Pos (7, 18))];
                     clauses =
                        [FromClause
                           [(TableName "DUAL", Pos (25, 28))],
                         Pos (20, 28)]},
                  Pos (0, 28))
  in
    test_parse_select_helper "SELECT 1 + 2 as sum FROM dual" [] expected

let test_parse_alias_2 () =
  let expected = (Select
                    {fields =
                        [(ColumnAlias ("",
                                       (BinaryOp ("+", (NumericLiteral "1", Pos (7, 7)),
                                                  (NumericLiteral "2", Pos (11, 11))),
                                        Pos (7, 11)),
                                       "SUM"),
                          Pos (7, 15))];
                     clauses =
                        [FromClause
                           [(TableName "DUAL", Pos (22, 25))],
                         Pos (17, 25)]},
                  Pos (0, 25))
  in
    test_parse_select_helper "SELECT 1 + 2 sum FROM dual" [] expected

let test_parse_where_1 () =
  let expected = (Select
                    {fields =
                        [(Column (Identifier "AUTHOR", Pos (7, 12)), Pos (7, 12));
                         (Column (Identifier "TITLE", Pos (15, 19)), Pos (15, 19));
                         (Column (Identifier "PRICE", Pos (22, 26)), Pos (22, 26))];
                     clauses =
                        [(FromClause [(TableName "TABLE", Pos (33, 37))], Pos (28, 37));
                         (WhereClause
                            (BinaryOp ("<",
                                       (Identifier "PRICE", Pos (45, 49)),
                                       (NumericLiteral "100", Pos (53, 55))),
                             Pos (45, 55)),
                          Pos (39, 55))]},
                  Pos (0, 55))
  in
    test_parse_select_helper
      "SELECT Author, Title, Price FROM Table WHERE Price < 100"
      []
      expected

let test_parse_where_2 () =
  let expected = (Select
                    {fields =
                        [(Column (Identifier "AUTHOR", Pos (7, 12)), Pos (7, 12));
                         (Column (Identifier "TITLE", Pos (15, 19)), Pos (15, 19));
                         (Column (Identifier "PRICE", Pos (22, 26)), Pos (22, 26))];
                     clauses =
                        [(FromClause
                            [(TableName "TABLE", Pos (33, 37))],
                          Pos (28, 37));
                         (WhereClause
                            (BinaryOp ("AND",
                                       (BinaryOp (">=",
                                                  (Identifier "PRICE", Pos (45, 49)),
                                                  (NumericLiteral "10", Pos (54, 55))),
                                        Pos (45, 55)),
                                       (BinaryOp ("<=",
                                                  (Identifier "PRICE", Pos (61, 65)),
                                                  (NumericLiteral "100", Pos (70, 72))),
                                        Pos (61, 72))),
                             Pos (45, 72)),
                          Pos (39, 72))]},
                  Pos (0, 72))
  in
    test_parse_select_helper
      "SELECT Author, Title, Price FROM Table WHERE Price >= 10 AND Price <= 100"
      []
      expected

let test_parse_order_by_1 () =
  let expected = (Select
                    {fields =
                        [(Column (Identifier "*", Pos (7, 7)), Pos (7, 7))];
                     clauses =
                        [(FromClause [(TableName "TABLE", Pos (14, 18))], Pos (9, 18));
                         (WhereClause
                            (BinaryOp (">=",
                                       (Identifier "PRICE", Pos (26, 30)),
                                       (NumericLiteral "10", Pos (35, 36))),
                             Pos (26, 36)),
                          Pos (20, 36));
                         (OrderByClause
                            ((Identifier "COST", Pos (47, 50)), "ASC"),
                          Pos (38, 54))]},
                  Pos (0, 54))
  in
    test_parse_select_helper
      "SELECT * FROM Table WHERE Price >= 10 ORDER BY Cost ASC"
      []
      expected

let test_parse_order_by_2 () =
  let expected = (Select
                    {fields =
                        [(Column (Identifier "*", Pos (7, 7)), Pos (7, 7))];
                     clauses =
                        [(FromClause [(TableName "TABLE", Pos (14, 18))], Pos (9, 18));
                         (WhereClause
                            (BinaryOp (">=",
                                       (Identifier "PRICE", Pos (26, 30)),
                                       (NumericLiteral "10", Pos (35, 36))),
                             Pos (26, 36)),
                          Pos (20, 36));
                         (OrderByClause
                            ((Identifier "COST", Pos (47, 50)), ""),
                          Pos (38, 50))]},
                  Pos (0, 50))
  in
    test_parse_select_helper
      "SELECT * FROM Table WHERE Price >= 10 ORDER BY Cost"
      []
      expected

let test_parse_order_by_3 () =
  let expected = (Select
                    {fields =
                        [(Column (Identifier "*", Pos (7, 7)), Pos (7, 7))];
                     clauses =
                        [(FromClause [(TableName "TABLE", Pos (14, 18))], Pos (9, 18));
                         (WhereClause
                            (BinaryOp (">=",
                                       (Identifier "PRICE", Pos (26, 30)),
                                       (NumericLiteral "10", Pos (35, 36))),
                             Pos (26, 36)),
                          Pos (20, 36));
                         (OrderByClause
                            ((Identifier "COST", Pos (47, 50)), "DESC"),
                          Pos (38, 55))]},
                  Pos (0, 55))
  in
    test_parse_select_helper
      "SELECT * FROM Table WHERE Price >= 10 ORDER BY Cost DESC"
      []
      expected

let test_parse_group_by_1 () =
  let expected = (Select
                    {fields =
                        [(Column (Identifier "PRODUCT", Pos (7, 13)), Pos (7, 13));
                         (Column (Identifier "PRICE", Pos (16, 20)), Pos (16, 20))];
                     clauses =
                        [(FromClause [(TableName "TABLE", Pos (27, 31))], Pos (22, 31));
                         (WhereClause
                            (BinaryOp (">=",
                                       (Identifier "PRICE", Pos (39, 43)),
                                       (NumericLiteral "10", Pos (48, 49))),
                             Pos (39, 49)),
                          Pos (33, 49));
                         (GroupByClause
                            [(Identifier "PRODUCT", Pos (60, 66));
                             (Identifier "PRICE", Pos (69, 73))],
                          Pos (51, 73))]},
                  Pos (0, 73))
  in
    test_parse_select_helper
      "SELECT Product, Price FROM Table WHERE Price >= 10 GROUP BY Product, Price"
      []
      expected

let test_parse_inner_join_1 () =
  let expected = (Select
                    {fields =
                        [(Column
                            (BinaryOp (".",
                                       (Identifier "T1", Pos (7, 8)),
                                       (Identifier "F1", Pos (10, 11))),
                             Pos (7, 11)),
                          Pos (7, 11));
                         (Column
                            (BinaryOp (".",
                                       (Identifier "T2", Pos (14, 15)),
                                       (Identifier "F2", Pos (17, 18))),
                             Pos (14, 18)),
                          Pos (14, 18))];
                     clauses =
                        [(FromClause
                            [(TableJoin
                                (InnerJoin,
                                 (TableAlias ("TABLE1", "T1"), Pos (25, 33)),
                                 (TableAlias ("TABLE2", "T2"), Pos (46, 54)),
                                 Some (BinaryOp ("=",
                                                 (BinaryOp (".",
                                                            (Identifier "T1", Pos (59, 60)),
                                                            (Identifier "F1", Pos (62, 63))),
                                                  Pos (59, 63)),
                                                 (BinaryOp (".",
                                                            (Identifier "T2", Pos (67, 68)),
                                                            (Identifier "F1", Pos (70, 71))),
                                                  Pos (67, 71))),
                                       Pos (59, 71)),
                                 None),
                              Pos (25, 71))],
                          Pos (20, 71))]},
                  Pos (0, 71))
  in
    test_parse_select_helper
      "SELECT t1.f1, t2.f2 FROM Table1 t1 INNER JOIN Table2 t2 ON t1.f1 = t2.f1"
      []
      expected

let test_parse_inner_join_2 () =
  let expected = (Select
                    {fields =
                        [(Column (Identifier "*", Pos (7, 7)),
                          Pos (7, 7))];
                     clauses =
                        [(FromClause
                            [(TableJoin (InnerJoin,
                                         (TableAlias ("ADDRESSES", "A"), Pos (14, 24)),
                                         (TableAlias ("CLIENTS", "C"), Pos (37, 45)),
                                         None,
                                         Some ["ADDRESSID"; "ADDRESSID"]),
                              Pos (14, 74))],
                          Pos (9, 74))]},
                  Pos (0, 74))
  in
    test_parse_select_helper
      "SELECT * FROM Addresses A INNER JOIN Clients C USING (AddressId, AddressId)"
      []
      expected

let test_parse_inner_join_3 () =
  let expected = (Select
                    {fields =
                        [(Column
                            (BinaryOp (".",
                                       (Identifier "T1", Pos (7, 8)),
                                       (Identifier "F1", Pos (10, 11))),
                             Pos (7, 11)),
                          Pos (7, 11));
                         (Column
                            (BinaryOp (".",
                                       (Identifier "T2", Pos (14, 15)),
                                       (Identifier "F2", Pos (17, 18))),
                             Pos (14, 18)),
                          Pos (14, 18))];
                     clauses =
                        [(FromClause
                            [(TableJoin (Join,
                                         (TableAlias ("TABLE1", "T1"), Pos (25, 33)),
                                         (TableAlias ("TABLE2", "T2"), Pos (40, 48)),
                                         Some
                                           (BinaryOp ("=",
                                                      (BinaryOp (".",
                                                                 (Identifier "T1", Pos (53, 54)),
                                                                 (Identifier "F1", Pos (56, 57))),
                                                       Pos (53, 57)),
                                                      (BinaryOp (".",
                                                                 (Identifier "T2", Pos (61, 62)),
                                                                 (Identifier "F1", Pos (64, 65))),
                                                       Pos (61, 65))),
                                            Pos (53, 65)),
                                         None),
                              Pos (25, 65))],
                          Pos (20, 65))]},
                  Pos (0, 65))
  in
    test_parse_select_helper
      "SELECT t1.f1, t2.f2 FROM Table1 t1 JOIN Table2 t2 ON t1.f1 = t2.f1"
      []
      expected

let test_parse_outer_join_1 () =
  let expected = (Select
                    {fields =
                        [(Column
                            (BinaryOp (".",
                                       (Identifier "T1", Pos (7, 8)),
                                       (Identifier "F1", Pos (10, 11))),
                             Pos (7, 11)),
                          Pos (7, 11));
                         (Column
                            (BinaryOp (".",
                                       (Identifier "T2", Pos (14, 15)),
                                       (Identifier "F2", Pos (17, 18))),
                             Pos (14, 18)),
                          Pos (14, 18))];
                     clauses =
                        [(FromClause
                            [(TableJoin (FullOuterJoin,
                                         (TableAlias ("TABLE1", "T1"), Pos (25, 33)),
                                         (TableAlias ("TABLE2", "T2"), Pos (45, 53)),
                                         Some
                                           (BinaryOp ("=",
                                                      (BinaryOp (".",
                                                                 (Identifier "T1", Pos (58, 59)),
                                                                 (Identifier "F1", Pos (61, 62))),
                                                       Pos (58, 62)),
                                                      (BinaryOp (".",
                                                                 (Identifier "T2", Pos (66, 67)),
                                                                 (Identifier "F1", Pos (69, 70))),
                                                       Pos (66, 70))),
                                            Pos (58, 70)),
                                         None),
                              Pos (25, 70))],
                          Pos (20, 70))]},
                  Pos (0, 70))
  in
    test_parse_select_helper
      "SELECT t1.f1, t2.f2 FROM Table1 t1 FULL JOIN Table2 t2 ON t1.f1 = t2.f1"
      []
      expected

let test_parse_outer_join_2 () =
  let expected = (Select
                    {fields =
                        [(Column
                            (BinaryOp (".",
                                       (Identifier "T1", Pos (7, 8)),
                                       (Identifier "F1", Pos (10, 11))),
                             Pos (7, 11)),
                          Pos (7, 11));
                         (Column
                            (BinaryOp (".",
                                       (Identifier "T2", Pos (14, 15)),
                                       (Identifier "F2", Pos (17, 18))),
                             Pos (14, 18)),
                          Pos (14, 18))];
                     clauses =
                        [(FromClause
                            [(TableJoin (LeftOuterJoin,
                                         (TableAlias ("TABLE1", "T1"), Pos (25, 33)),
                                         (TableAlias ("TABLE2", "T2"), Pos (45, 53)),
                                         Some
                                           (BinaryOp ("=",
                                                      (BinaryOp (".",
                                                                 (Identifier "T1", Pos (58, 59)),
                                                                 (Identifier "F1", Pos (61, 62))),
                                                       Pos (58, 62)),
                                                      (BinaryOp (".",
                                                                 (Identifier "T2", Pos (66, 67)),
                                                                 (Identifier "F1", Pos (69, 70))),
                                                       Pos (66, 70))),
                                            Pos (58, 70)),
                                         None),
                              Pos (25, 70))],
                          Pos (20, 70))]},
                  Pos (0, 70))
  in
    test_parse_select_helper
      "SELECT t1.f1, t2.f2 FROM Table1 t1 LEFT JOIN Table2 t2 ON t1.f1 = t2.f1"
      []
      expected

let test_parse_many_join_1 () =
  let expected = (Select
                    {fields =
                        [(Column
                            (BinaryOp (".",
                                       (Identifier "T1", Pos (8, 9)),
                                       (Identifier "F1", Pos (11, 12))),
                             Pos (8, 12)),
                          Pos (8, 12));
                         (Column
                            (BinaryOp (".",
                                       (Identifier "T2", Pos (15, 16)),
                                       (Identifier "F2", Pos (18, 19))),
                             Pos (15, 19)),
                          Pos (15, 19));
                         (Column
                            (BinaryOp (".",
                                       (Identifier "T3", Pos (22, 23)),
                                       (Identifier "F2", Pos (25, 26))),
                             Pos (22, 26)),
                          Pos (22, 26))];
                     clauses =
                        [(FromClause
                            [(TableJoin (LeftOuterJoin,
                                         (TableJoin (FullOuterJoin,
                                                     (TableAlias ("TABLE1", "T1"), Pos (33, 41)),
                                                     (TableAlias ("TABLE2", "T2"), Pos (53, 61)),
                                                     Some
                                                       (BinaryOp ("=",
                                                                  (BinaryOp (".",
                                                                             (Identifier "T1",
                                                                              Pos (66, 67)),
                                                                             (Identifier "F1",
                                                                              Pos (69, 70))),
                                                                   Pos (66, 70)),
                                                                  (BinaryOp (".",
                                                                             (Identifier "T2",
                                                                              Pos (74, 75)),
                                                                             (Identifier "F1",
                                                                              Pos (77, 78))),
                                                                   Pos (74, 78))),
                                                        Pos (66, 78)),
                                                     None),
                                          Pos (33, 78)),
                                         (TableAlias ("TABLE3", "T3"), Pos (96, 104)),
                                         Some
                                           (BinaryOp ("=",
                                                      (BinaryOp (".",
                                                                 (Identifier "T2", Pos (109, 110)),
                                                                 (Identifier "F2", Pos (112, 113))),
                                                       Pos (109, 113)),
                                                      (BinaryOp (".",
                                                                 (Identifier "T3", Pos (117, 118)),
                                                                 (Identifier "F1", Pos (120, 121))),
                                                       Pos (117, 121))),
                                            Pos (109, 121)),
                                         None),
                              Pos (33, 121))],
                          Pos (28, 121))]},
                  Pos (1, 121))
  in
    test_parse_select_helper
      "
SELECT t1.f1, t2.f2, t3.f2 FROM Table1 t1
FULL JOIN Table2 t2 ON t1.f1 = t2.f1
LEFT OUTER JOIN Table3 t3 ON t2.f2 = t3.f1
"
      []
      expected

let test_parse_group_by_having_1 () =
  let expected = (Select
                    {fields =
                        [(Column
                            (Identifier "PRODUCT", Pos (8, 14)),
                          Pos (8, 14));
                         (Column
                            (Identifier "PRICE", Pos (17, 21)),
                          Pos (17, 21))];
                     clauses =
                        [(FromClause
                            [(TableName "TABLE", Pos (28, 32))],
                          Pos (23, 32));
                         (WhereClause
                            (BinaryOp (">=",
                                       (Identifier "PRICE", Pos (40, 44)),
                                       (NumericLiteral "10", Pos (49, 50))),
                             Pos (40, 50)),
                          Pos (34, 50));
                         (GroupByClause
                            [(Identifier "PRODUCT", Pos (61, 67));
                             (Identifier "PRICE", Pos (70, 74))],
                          Pos (52, 74));
                         (HavingClause
                            (Like
                               ((Identifier "PRODUCT", Pos (83, 89)),
                                (StringLiteral "'Wheel%'", Pos (96, 103))),
                             Pos (83, 103)),
                          Pos (76, 103))]},
                  Pos (1, 103))
  in
    test_parse_select_helper
      "
SELECT Product, Price
FROM Table
WHERE Price >= 10
GROUP BY Product, Price
HAVING Product LIKE 'Wheel%'"
      []
      expected

let test_parse_group_by_having_2 () =
  let expected = (Select
                    {fields =
                        [(Column
                            (Identifier "PRODUCT", Pos (8, 14)),
                          Pos (8, 14));
                         (Column
                            (Identifier "PRICE", Pos (17, 21)),
                          Pos (17, 21))];
                     clauses =
                        [(FromClause
                            [(TableName "TABLE", Pos (28, 32))],
                          Pos (23, 32));
                         (WhereClause
                            (BinaryOp (">=",
                                       (Identifier "PRICE", Pos (40, 44)),
                                       (NumericLiteral "10", Pos (49, 50))),
                             Pos (40, 50)),
                          Pos (34, 50));
                         (HavingClause
                            (Like
                               ((Identifier "PRODUCT", Pos (59, 65)),
                                (StringLiteral "'Wheel%'", Pos (72, 79))),
                             Pos (59, 79)),
                          Pos (52, 79));
                         (GroupByClause
                            [(Identifier "PRODUCT", Pos (90, 96));
                             (Identifier "PRICE", Pos (99, 103))],
                          Pos (81, 103))]},
                  Pos (1, 103))
  in
    test_parse_select_helper
      "
SELECT Product, Price
FROM Table
WHERE Price >= 10
HAVING Product LIKE 'Wheel%'
GROUP BY Product, Price"
      []
      expected

let test_parse_select_into_1 () =
  let expected = (Select
                    {fields =
                        [(Column
                            (Identifier "TOWN", Pos (7, 10)),
                          Pos (7, 10))];
                     clauses =
                        [(IntoClause
                            [(Identifier "V_TOWN", Pos (17, 22))],
                          Pos (12, 22));
                         (FromClause
                            [(TableName "ADDRESSES", Pos (29, 37))],
                          Pos (24, 37));
                         (WhereClause
                            (BinaryOp ("=",
                                       (Identifier "ADDRESSID", Pos (45, 53)),
                                       (NumericLiteral "1", Pos (57, 57))),
                             Pos (45, 57)),
                          Pos (39, 57))]},
                  Pos (0, 57))
  in
    test_parse_select_helper
      "SELECT town INTO v_town FROM addresses WHERE addressid = 1"
      []
      expected

let suite = "Select tests" >::: ["test_parse_simple_select_1" >::
                                   test_parse_simple_select_1;
                                 "test_parse_simple_select_2" >::
                                   test_parse_simple_select_2;
                                 "test_parse_simple_select_3" >::
                                   test_parse_simple_select_3;
                                 "test_parse_simple_select_4" >::
                                   test_parse_simple_select_4;
                                 "test_parse_alias_1" >:: test_parse_alias_1;
                                 "test_parse_alias_2" >:: test_parse_alias_2;
                                 "test_parse_where_1" >:: test_parse_where_1;
                                 "test_parse_where_2" >:: test_parse_where_2;
                                 "test_parse_order_by_1" >:: test_parse_order_by_1;
                                 "test_parse_order_by_2" >:: test_parse_order_by_2;
                                 "test_parse_order_by_3" >:: test_parse_order_by_3;
                                 "test_parse_group_by_1" >:: test_parse_group_by_1;
                                 "test_parse_inner_join_1" >:: test_parse_inner_join_1;
                                 "test_parse_inner_join_2" >:: test_parse_inner_join_2;
                                 "test_parse_inner_join_3" >:: test_parse_inner_join_3;
                                 "test_parse_outer_join_1" >:: test_parse_outer_join_1;
                                 "test_parse_outer_join_2" >:: test_parse_outer_join_2;
                                 "test_parse_many_join_1" >:: test_parse_many_join_1;
                                 "test_parse_group_by_having_1" >::
                                   test_parse_group_by_having_1;
                                 "test_parse_group_by_having_2" >::
                                   test_parse_group_by_having_2;
                                 "test_parse_select_into_1" >:: test_parse_select_into_1;
                                ]
