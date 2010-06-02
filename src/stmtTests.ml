
open OUnit;;
open ParserTypes;;
open Lexer;;
open PlsqlParser;;
open Pwm;;
open Ast;;
open TestsCommon;;

let test_parse_helper = parse_helper parse;;

let test_parse_begin_end () =
  test_parse_helper
    "BEGIN END;"
    []
    (Program([Block([], []), Pos(0, 9)]), Pos(0, 9));;

let test_parse_declare_begin_end () =
  test_parse_helper
    "DECLARE BEGIN END;"
    []
    (Program([Block([], []), Pos(0, 17)]), Pos(0, 17));;

let test_parse_empty_block_with_decl () =
  test_parse_helper
    "DECLARE var NUMBER(3); BEGIN END;"
    []
    (Program
       [(Block
           ([(VarDecl ("VAR",
                       (Number (3, 0), Pos (12, 20))),
              Pos (8, 21))],
            []),
         Pos (0, 32))],
     Pos (0, 32));;

let test_parse_begin_end_no_semicolon () =
  test_parse_helper
    "BEGIN END"
    [(Error, "Expected ';'.", LineColumn(0, 8))]
    (Program([Block([], []), Pos(0, 8)]), Pos(0, 8));
  test_parse_helper
    "DECLARE BEGIN END"
    [(Error, "Expected ';'.", LineColumn(0, 16))]
    (Program([Block([], []), Pos(0, 16)]), Pos(0, 16));
  test_parse_helper
    "DECLARE var NUMBER(3) BEGIN END"
    [(Error, "Expected ';'.", LineColumn(0, 20));
     (Error, "Expected ';'.", LineColumn(0, 30))]
    (Program
       [(Block
           ([(VarDecl ("VAR",
                       (Number (3, 0), Pos (12, 20))),
              Pos (8, 20))],
            []),
         Pos (0, 30))],
     Pos (0, 30));;

let test_parse_simple_complete_block_1 () =
  test_parse_helper
    "DECLARE var NUMBER(3); BEGIN var := 0; END;"
    []
    (Program
       [(Block
           ([(VarDecl ("VAR", (Number (3, 0), Pos (12, 20))), Pos (8, 21))],
            [(StmtAssignment
                ((Identifier "VAR", Pos (29, 31)),
                 (NumericLiteral "0", Pos (36, 36))),
              Pos (29, 37))]),
         Pos (0, 42))],
     Pos (0, 42));;

let test_parse_simple_complete_block_2 () =
  test_parse_helper
    "DECLARE var NUMBER(4, 2); BEGIN var := a; END;"
    []
    (Program
       [(Block
           ([(VarDecl ("VAR",
                       (Number (4, 2), Pos (12, 23))),
              Pos (8, 24))],
            [(StmtAssignment
                ((Identifier "VAR", Pos (32, 34)),
                 (Identifier "A", Pos (39, 39))),
              Pos (32, 40))]),
         Pos (0, 45))],
     Pos (0, 45));;

let test_parse_select_1 () =
  test_parse_helper
    "SELECT a, b FROM c;"
    []
    (Program
       [(StmtSelect
           (Select
              {fields =
                  [(Column (Identifier "A", Pos (7, 7)), Pos (7, 7));
                   (Column (Identifier "B", Pos (10, 10)), Pos (10, 10))];
               clauses =
                  [(FromClause [(TableName "C", Pos (17, 17))], Pos (12, 17))]},
            Pos (0, 17)),
         Pos (0, 18))],
     Pos (0, 18));;

let test_parse_select_1 () =
  test_parse_helper
    "SELECT a, b FROM c;"
    []
    (Program
       [(StmtSelect
           (Select
              {fields =
                  [(Column (Identifier "A", Pos (7, 7)), Pos (7, 7));
                   (Column (Identifier "B", Pos (10, 10)), Pos (10, 10))];
               clauses =
                  [(FromClause [(TableName "C", Pos (17, 17))], Pos (12, 17))]},
            Pos (0, 17)),
         Pos (0, 18))],
     Pos (0, 18));;

let test_parse_select_2 () =
  test_parse_helper
    "BEGIN SELECT a, b FROM c; END;"
    []
    (Program
       [(Block ([],
                [(StmtSelect
                    (Select
                       {fields =
                           [(Column (Identifier "A", Pos (13, 13)), Pos (13, 13));
                            (Column (Identifier "B", Pos (16, 16)), Pos (16, 16))];
                        clauses =
                           [(FromClause [(TableName "C", Pos (23, 23))], Pos (18, 23))]},
                     Pos (6, 23)),
                  Pos (6, 24))]),
         Pos (0, 29))],
     Pos (0, 29));;

let test_parse_if_1 () =
  test_parse_helper
    "IF a > 10 THEN a := a + 1; END IF;"
    []
    (Program
       [(StmtIf
           ((BinaryOp (">",
                       (Identifier "A", Pos (3, 3)),
                       (NumericLiteral "10", Pos (7, 8))),
             Pos (3, 8)),
            [(StmtAssignment ((Identifier "A", Pos(15, 15)),
                              (BinaryOp ("+",
                                         (Identifier "A", Pos (20, 20)),
                                         (NumericLiteral "1", Pos (24, 24))),
                               Pos (20, 24))),
              Pos (15, 25))],
            NoElse),
         Pos (0, 33))],
     Pos (0, 33));;

let test_parse_if_2 () =
  test_parse_helper
    "IF a > 10 THEN a := a + 1; b := a * 2; END IF;"
    []
    (Program
       [(StmtIf
           ((BinaryOp (">",
                       (Identifier "A", Pos (3, 3)),
                       (NumericLiteral "10", Pos (7, 8))),
             Pos (3, 8)),
            [(StmtAssignment ((Identifier "A", Pos(15, 15)),
                              (BinaryOp ("+",
                                         (Identifier "A", Pos (20, 20)),
                                         (NumericLiteral "1", Pos (24, 24))),
                               Pos (20, 24))),
              Pos (15, 25));
             (StmtAssignment ((Identifier "B", Pos(27, 27)),
                              (BinaryOp ("*",
                                         (Identifier "A", Pos (32, 32)),
                                         (NumericLiteral "2", Pos (36, 36))),
                               Pos (32, 36))),
              Pos (27, 37))],
            NoElse),
         Pos (0, 45))],
     Pos (0, 45));;

let test_parse_if_3 () =
  test_parse_helper
    "IF a > 10 THEN a := a + 1; ELSE b := a * 2; END IF;"
    []
    (Program
       [(StmtIf
           ((BinaryOp (">",
                       (Identifier "A", Pos (3, 3)),
                       (NumericLiteral "10", Pos (7, 8))),
             Pos (3, 8)),
            [(StmtAssignment ((Identifier "A", Pos(15, 15)),
                              (BinaryOp ("+",
                                         (Identifier "A", Pos (20, 20)),
                                         (NumericLiteral "1", Pos (24, 24))),
                               Pos (20, 24))),
              Pos (15, 25))],
            Else
              [(StmtAssignment ((Identifier "B", Pos(32, 32)),
                                (BinaryOp ("*",
                                           (Identifier "A", Pos (37, 37)),
                                           (NumericLiteral "2", Pos (41, 41))),
                                 Pos (37, 41))),
                Pos (32, 42))]),
         Pos (0, 50))],
     Pos (0, 50));;

let test_parse_if_4 () =
  test_parse_helper
    "IF a > 10 THEN a := a + 1; ELSE b := a * 2; a := 7 - b; END IF;"
    []
    (Program
       [(StmtIf
           ((BinaryOp (">",
                       (Identifier "A", Pos (3, 3)),
                       (NumericLiteral "10", Pos (7, 8))),
             Pos (3, 8)),
            [(StmtAssignment ((Identifier "A", Pos(15, 15)),
                              (BinaryOp ("+",
                                         (Identifier "A", Pos (20, 20)),
                                         (NumericLiteral "1", Pos (24, 24))),
                               Pos (20, 24))),
              Pos (15, 25))],
            Else
              [(StmtAssignment ((Identifier "B", Pos(32, 32)),
                                (BinaryOp ("*",
                                           (Identifier "A", Pos (37, 37)),
                                           (NumericLiteral "2", Pos (41, 41))),
                                 Pos (37, 41))),
                Pos (32, 42));
               (StmtAssignment ((Identifier "A", Pos(44, 44)),
                                (BinaryOp ("-",
                                           (NumericLiteral "7", Pos (49, 49)),
                                           (Identifier "B", Pos (53, 53))),
                                 Pos (49, 53))),
                Pos (44, 54))]),
         Pos (0, 62))],
     Pos (0, 62));;

let test_parse_if_5 () =
  test_parse_helper
    "IF a > 10 THEN a := a + 1; ELSIF a < 5 THEN b := a * 2; END IF;"
    []
    (Program
       [(StmtIf
           ((BinaryOp (">",
                       (Identifier "A", Pos (3, 3)),
                       (NumericLiteral "10", Pos (7, 8))),
             Pos (3, 8)),
            [(StmtAssignment ((Identifier "A", Pos(15, 15)),
                              (BinaryOp ("+",
                                         (Identifier "A", Pos (20, 20)),
                                         (NumericLiteral "1", Pos (24, 24))),
                               Pos (20, 24))),
              Pos (15, 25))],
            ElsIf
              ((BinaryOp ("<",
                          (Identifier "A", Pos (33, 33)),
                          (NumericLiteral "5", Pos (37, 37))),
                Pos (33, 37)),
               [(StmtAssignment ((Identifier "B", Pos(44, 44)),
                                 (BinaryOp ("*",
                                            (Identifier "A", Pos (49, 49)),
                                            (NumericLiteral "2", Pos (53, 53))),
                                  Pos (49, 53))),
                 Pos (44, 54))],
               NoElse)),
         Pos (0, 62))],
     Pos (0, 62));;

let test_parse_if_6 () =
  test_parse_helper
    "IF a > 10 THEN a := a + 1; ELSIF a < 5 THEN b := a * 2; ELSE b := 2; END IF;"
    []
    (Program
       [(StmtIf
           ((BinaryOp (">",
                       (Identifier "A", Pos (3, 3)),
                       (NumericLiteral "10", Pos (7, 8))),
             Pos (3, 8)),
            [(StmtAssignment ((Identifier "A", Pos(15, 15)),
                              (BinaryOp ("+",
                                         (Identifier "A", Pos (20, 20)),
                                         (NumericLiteral "1", Pos (24, 24))),
                               Pos (20, 24))),
              Pos (15, 25))],
            ElsIf
              ((BinaryOp ("<",
                          (Identifier "A", Pos (33, 33)),
                          (NumericLiteral "5", Pos (37, 37))),
                Pos (33, 37)),
               [(StmtAssignment ((Identifier "B", Pos(44, 44)),
                                 (BinaryOp ("*",
                                            (Identifier "A", Pos (49, 49)),
                                            (NumericLiteral "2", Pos (53, 53))),
                                  Pos (49, 53))),
                 Pos (44, 54))],
               Else
                 [(StmtAssignment ((Identifier "B", Pos(61, 61)),
                                   (NumericLiteral "2", Pos (66, 66))),
                   Pos (61, 67))])),
         Pos (0, 75))],
     Pos (0, 75));;

let test_parse_if_7 () =
  test_parse_helper
    "
IF a > 10 THEN a := a + 1;
ELSIF a < 5 THEN b := a * 2;
ELSIF a = b THEN b := 1;
ELSE b := 2; END IF;"
    []
    (Program
       [(StmtIf
           ((BinaryOp (">",
                       (Identifier "A", Pos (4, 4)),
                       (NumericLiteral "10", Pos (8, 9))),
             Pos (4, 9)),
            [(StmtAssignment
                ((Identifier "A", Pos (16, 16)),
                 (BinaryOp ("+",
                            (Identifier "A", Pos (21, 21)),
                            (NumericLiteral "1", Pos (25, 25))),
                  Pos (21, 25))),
              Pos (16, 26))],
            ElsIf
              ((BinaryOp ("<",
                          (Identifier "A", Pos (34, 34)),
                          (NumericLiteral "5", Pos (38, 38))),
                Pos (34, 38)),
               [(StmtAssignment
                   ((Identifier "B", Pos (45, 45)),
                    (BinaryOp ("*",
                               (Identifier "A", Pos (50, 50)),
                               (NumericLiteral "2", Pos (54, 54))),
                     Pos (50, 54))),
                 Pos (45, 55))],
               ElsIf
                 ((BinaryOp ("=",
                             (Identifier "A", Pos (63, 63)),
                             (Identifier "B", Pos (67, 67))),
                   Pos (63, 67)),
                  [(StmtAssignment
                      ((Identifier "B", Pos (74, 74)),
                       (NumericLiteral "1", Pos (79, 79))),
                    Pos (74, 80))],
                  Else
                    [(StmtAssignment
                        ((Identifier "B", Pos (87, 87)),
                         (NumericLiteral "2", Pos (92, 92))),
                      Pos (87, 93))]))),
         Pos (1, 101))],
     Pos (1, 101));;

let test_parse_basic_loop_1 () =
  test_parse_helper
    "
LOOP
  a := 1;
  b := 'a';
END LOOP;"
    []
    (Program
       [(StmtLoop
           ([(StmtAssignment ((Identifier "A", Pos(8, 8)),
                              (NumericLiteral "1", Pos (13, 13))),
              Pos (8, 14));
             (StmtAssignment ((Identifier "B", Pos(18, 18)),
                              (StringLiteral "'a'", Pos (23, 25))),
              Pos (18, 26))],
            None),
         Pos (1, 36))],
     Pos (1, 36));;

let test_parse_basic_loop_2 () =
  test_parse_helper
    "
<<label>>
LOOP
  a := 1;
  b := 'a';
END LOOP label;"
    []
    (Program
       [(StmtLabeled ("LABEL",
                      (StmtLoop
                         ([(StmtAssignment ((Identifier "A", Pos(18, 18)),
                                            (NumericLiteral "1", Pos (23, 23))),
                            Pos (18, 24));
                           (StmtAssignment ((Identifier "B", Pos(28, 28)),
                                            (StringLiteral "'a'", Pos (33, 35))),
                            Pos (28, 36))],
                          Some "LABEL"),
                       Pos (11, 52))),
         Pos (1, 52))],
     Pos (1, 52));;

let test_parse_basic_loop_3 () =
  test_parse_helper
    "
<<label>>
LOOP
  a := 1;
  b := 'a';
END LOOP;"
    []
    (Program
       [(StmtLabeled ("LABEL",
                      (StmtLoop
                         ([(StmtAssignment ((Identifier "A", Pos(18, 18)),
                                            (NumericLiteral "1", Pos (23, 23))),
                            Pos (18, 24));
                           (StmtAssignment ((Identifier "B", Pos(28, 28)),
                                            (StringLiteral "'a'", Pos (33, 35))),
                            Pos (28, 36))],
                          None),
                       Pos (11, 46))),
         Pos (1, 46))],
     Pos (1, 46));;

let test_parse_exit () =
  test_parse_helper
    "
<<label>>
LOOP
  a := 1;
  EXIT;
  b := 'a';
END LOOP;"
    []
    (Program
       [(StmtLabeled ("LABEL",
                      (StmtLoop
                         ([(StmtAssignment ((Identifier "A", Pos(18, 18)),
                                            (NumericLiteral "1", Pos (23, 23))),
                            Pos (18, 24));
                           (StmtExit None, Pos (28, 32));
                           (StmtAssignment ((Identifier "B", Pos(36, 36)),
                                            (StringLiteral "'a'", Pos (41, 43))),
                            Pos (36, 44))],
                          None),
                       Pos (11, 54))),
         Pos (1, 54))],
     Pos (1, 54));;

let test_parse_exit_when () =
  test_parse_helper
    "
<<label>>
LOOP
  a := a + 1;
  EXIT WHEN a = 5;
  b := 'a';
END LOOP;"
    []
    (Program
       [(StmtLabeled ("LABEL",
                      (StmtLoop
                         ([(StmtAssignment ((Identifier "A", Pos(18, 18)),
                                            (BinaryOp ("+",
                                                       (Identifier "A", Pos (23, 23)),
                                                       (NumericLiteral "1", Pos (27, 27))),
                                             Pos (23, 27))),
                            Pos (18, 28));
                           (StmtExitWhen
                              ((BinaryOp ("=",
                                          (Identifier "A", Pos (42, 42)),
                                          (NumericLiteral "5", Pos (46, 46))),
                                Pos (42, 46)),
                               None),
                            Pos (32, 47));
                           (StmtAssignment ((Identifier "B", Pos(51, 51)),
                                            (StringLiteral "'a'", Pos (56, 58))),
                            Pos (51, 59))],
                          None),
                       Pos (11, 69))),
         Pos (1, 69))],
     Pos (1, 69));;

let test_parse_exit_labeled () =
  test_parse_helper
    "
<<label>>
LOOP
  a := 1;
  EXIT label;
  b := 'a';
END LOOP;"
    []
    (Program
       [(StmtLabeled ("LABEL",
                      (StmtLoop
                         ([(StmtAssignment ((Identifier "A", Pos(18, 18)),
                                            (NumericLiteral "1", Pos (23, 23))),
                            Pos (18, 24));
                           (StmtExit (Some "LABEL"), Pos (28, 38));
                           (StmtAssignment ((Identifier "B", Pos(42, 42)),
                                            (StringLiteral "'a'", Pos (47, 49))),
                            Pos (42, 50))],
                          None),
                       Pos (11, 60))),
         Pos (1, 60))],
     Pos (1, 60));;

let test_parse_exit_when_labeled () =
  test_parse_helper
    "
<<label>>
LOOP
  a := 1;
  EXIT label WHEN a = 5;
  b := 'a';
END LOOP;"
    []
    (Program
       [(StmtLabeled ("LABEL",
                      (StmtLoop
                         ([(StmtAssignment ((Identifier "A", Pos(18, 18)),
                                            (NumericLiteral "1", Pos (23, 23))),
                            Pos (18, 24));
                           (StmtExitWhen
                              ((BinaryOp ("=",
                                          (Identifier "A", Pos (44, 44)),
                                          (NumericLiteral "5", Pos (48, 48))),
                                Pos (44, 48)),
                               Some "LABEL"),
                            Pos (28, 49));
                           (StmtAssignment ((Identifier "B", Pos(53, 53)),
                                            (StringLiteral "'a'", Pos (58, 60))),
                            Pos (53, 61))],
                          None),
                       Pos (11, 71))),
         Pos (1, 71))],
     Pos (1, 71));;

let test_parse_continue () =
  test_parse_helper
    "
<<label>>
LOOP
  a := 1;
  CONTINUE;
  b := 'a';
END LOOP;"
    []
    (Program
       [(StmtLabeled ("LABEL",
                      (StmtLoop
                         ([(StmtAssignment ((Identifier "A", Pos(18, 18)),
                                            (NumericLiteral "1", Pos (23, 23))),
                            Pos (18, 24));
                           (StmtContinue None, Pos (28, 36));
                           (StmtAssignment ((Identifier "B", Pos(40, 40)),
                                            (StringLiteral "'a'", Pos (45, 47))),
                            Pos (40, 48))],
                          None),
                       Pos (11, 58))),
         Pos (1, 58))],
     Pos (1, 58));;

let test_parse_continue_when () =
  test_parse_helper
    "
<<label>>
LOOP
  a := a + 1;
  CONTINUE WHEN a = 5;
  b := 'a';
END LOOP;"
    []
    (Program
       [(StmtLabeled ("LABEL",
                      (StmtLoop
                         ([(StmtAssignment ((Identifier "A", Pos(18, 18)),
                                            (BinaryOp ("+",
                                                       (Identifier "A", Pos (23, 23)),
                                                       (NumericLiteral "1", Pos (27, 27))),
                                             Pos (23, 27))),
                            Pos (18, 28));
                           (StmtContinueWhen
                              ((BinaryOp ("=",
                                          (Identifier "A", Pos (46, 46)),
                                          (NumericLiteral "5", Pos (50, 50))),
                                Pos (46, 50)),
                               None),
                            Pos (32, 51));
                           (StmtAssignment ((Identifier "B", Pos(55, 55)),
                                            (StringLiteral "'a'", Pos (60, 62))),
                            Pos (55, 63))],
                          None),
                       Pos (11, 73))),
         Pos (1, 73))],
     Pos (1, 73));;

let test_parse_continue_labeled () =
  test_parse_helper
    "
<<label>>
LOOP
  a := 1;
  CONTINUE label;
  b := 'a';
END LOOP;"
    []
    (Program
       [(StmtLabeled ("LABEL",
                      (StmtLoop
                         ([(StmtAssignment ((Identifier "A", Pos(18, 18)),
                                            (NumericLiteral "1", Pos (23, 23))),
                            Pos (18, 24));
                           (StmtContinue (Some "LABEL"), Pos (28, 42));
                           (StmtAssignment ((Identifier "B", Pos(46, 46)),
                                            (StringLiteral "'a'", Pos (51, 53))),
                            Pos (46, 54))],
                          None),
                       Pos (11, 64))),
         Pos (1, 64))],
     Pos (1, 64));;

let test_parse_continue_when_labeled () =
  test_parse_helper
    "
<<label>>
LOOP
  a := 1;
  CONTINUE label WHEN a = 5;
  b := 'a';
END LOOP;"
    []
    (Program
       [(StmtLabeled ("LABEL",
                      (StmtLoop
                         ([(StmtAssignment ((Identifier "A", Pos(18, 18)),
                                            (NumericLiteral "1", Pos (23, 23))),
                            Pos (18, 24));
                           (StmtContinueWhen
                              ((BinaryOp ("=",
                                          (Identifier "A", Pos (48, 48)),
                                          (NumericLiteral "5", Pos (52, 52))),
                                Pos (48, 52)),
                               Some "LABEL"),
                            Pos (28, 53));
                           (StmtAssignment ((Identifier "B", Pos(57, 57)),
                                            (StringLiteral "'a'", Pos (62, 64))),
                            Pos (57, 65))],
                          None),
                       Pos (11, 75))),
         Pos (1, 75))],
     Pos (1, 75));;

let test_parse_for_1 () =
  test_parse_helper
    "
FOR i IN 1..10 LOOP
  a := a + i;
END LOOP;"
    []
    (Program
       [(StmtFor ((Identifier "I", Pos (5, 5)),
                  false,
                  (NumericLiteral "1", Pos (10, 10)),
                  (NumericLiteral "10", Pos (13, 14)),
                  (StmtLoop
                     ([(StmtAssignment ((Identifier "A", Pos(23, 23)),
                                        (BinaryOp ("+",
                                                   (Identifier "A", Pos (28, 28)),
                                                   (Identifier "I", Pos (32, 32))),
                                         Pos (28, 32))),
                        Pos (23, 33))],
                      None),
                   Pos (16, 43))),
         Pos (1, 43))],
     Pos (1, 43));;

let test_parse_for_2 () =
  test_parse_helper
    "
FOR i IN REVERSE 1..10 LOOP
  a := a + i;
END LOOP;"
    []
    (Program
       [(StmtFor ((Identifier "I", Pos (5, 5)),
                  true,
                  (NumericLiteral "1", Pos (18, 18)),
                  (NumericLiteral "10", Pos (21, 22)),
                  (StmtLoop
                     ([(StmtAssignment ((Identifier "A", Pos(31, 31)),
                                        (BinaryOp ("+",
                                                   (Identifier "A", Pos (36, 36)),
                                                   (Identifier "I", Pos (40, 40))),
                                         Pos (36, 40))),
                        Pos (31, 41))],
                      None),
                   Pos (24, 51))),
         Pos (1, 51))],
     Pos (1, 51));;

let test_parse_for_3 () =
  test_parse_helper
    "
<<lup>>
FOR i IN REVERSE 1..10 LOOP
  a := a + i;
END LOOP lup;"
    []
    (Program
       [(StmtLabeled ("LUP",
                      (StmtFor ((Identifier "I", Pos (13, 13)),
                                true,
                                (NumericLiteral "1", Pos (26, 26)),
                                (NumericLiteral "10", Pos (29, 30)),
                                (StmtLoop
                                   ([(StmtAssignment ((Identifier "A", Pos(39, 39)),
                                                      (BinaryOp ("+",
                                                                 (Identifier "A", Pos (44, 44)),
                                                                 (Identifier "I", Pos (48, 48))),
                                                       Pos (44, 48))),
                                      Pos (39, 49))],
                                    Some "LUP"),
                                 Pos (32, 63))),
                       Pos (9, 63))),
         Pos (1, 63))],
     Pos (1, 63));;

let test_parse_for_4 () =
  test_parse_helper
    "
<<lup>>
FOR lup.i IN REVERSE 1..10 LOOP
  a := a + i;
END LOOP lup;"
    []
    (Program
       [(StmtLabeled ("LUP",
                      (StmtFor
                         ((BinaryOp (".",
                                     (Identifier "LUP", Pos (13, 15)),
                                     (Identifier "I", Pos (17, 17))),
                           Pos (13, 17)),
                          true, (NumericLiteral "1", Pos (30, 30)),
                          (NumericLiteral "10", Pos (33, 34)),
                          (StmtLoop
                             ([(StmtAssignment ((Identifier "A", Pos(43, 43)),
                                                (BinaryOp ("+",
                                                           (Identifier "A", Pos (48, 48)),
                                                           (Identifier "I", Pos (52, 52))),
                                                 Pos (48, 52))),
                                Pos (43, 53))],
                              Some "LUP"),
                           Pos (36, 67))),
                       Pos (9, 67))),
         Pos (1, 67))],
     Pos (1, 67));;

let test_parse_while_1 () =
  test_parse_helper
    "
WHILE a < 5 LOOP
  a := a + i;
END LOOP;"
    []
    (Program
       [(StmtWhile
           ((BinaryOp ("<",
                       (Identifier "A", Pos (7, 7)),
                       (NumericLiteral "5", Pos (11, 11))),
             Pos (7, 11)),
            (StmtLoop
               ([(StmtAssignment ((Identifier "A", Pos(20, 20)),
                                  (BinaryOp ("+",
                                             (Identifier "A", Pos (25, 25)),
                                             (Identifier "I", Pos (29, 29))),
                                   Pos (25, 29))),
                  Pos (20, 30))],
                None),
             Pos (13, 40))),
         Pos (1, 40))],
     Pos (1, 40));;

let test_parse_goto_1 () =
  test_parse_helper
    "
WHILE 1 < 5 LOOP
  a := a + 1;
  if a = 5 THEN GOTO test; END IF;
END LOOP;
<<test>>
b := a;"
    []
    (Program
       [(StmtWhile
           ((BinaryOp ("<",
                       (NumericLiteral "1", Pos (7, 7)),
                       (NumericLiteral "5", Pos (11, 11))),
             Pos (7, 11)),
            (StmtLoop
               ([(StmtAssignment ((Identifier "A", Pos(20, 20)),
                                  (BinaryOp ("+",
                                             (Identifier "A", Pos (25, 25)),
                                             (NumericLiteral "1", Pos (29, 29))),
                                   Pos (25, 29))),
                  Pos (20, 30));
                 (StmtIf
                    ((BinaryOp ("=",
                                (Identifier "A", Pos (37, 37)),
                                (NumericLiteral "5", Pos (41, 41))),
                      Pos (37, 41)),
                     [(StmtGoto
                         (Identifier "TEST", Pos (53, 56)),
                       Pos (48, 57))],
                     NoElse),
                  Pos (34, 65))],
                None),
             Pos (13, 75))),
         Pos (1, 75));
        (StmtLabeled ("TEST",
                      (StmtAssignment ((Identifier "B", Pos(86, 86)),
                                       (Identifier "A", Pos (91, 91))),
                       Pos (86, 92))),
         Pos (77, 92))],
     Pos (1, 92));;

let test_parse_null_1 () =
  test_parse_helper
    "NULL;"
    []
    (Program [(StmtNull, Pos (0, 4))],
     Pos (0, 4));;

let test_parse_call_1 () =
  test_parse_helper
    "DBMS_OUTPUT.PUT_LINE('Hello');"
    []
    (Program
       [(StmtCall
           ((BinaryOp (".",
                       (Identifier "DBMS_OUTPUT", Pos (0, 10)),
                       (Identifier "PUT_LINE", Pos (12, 19))),
             Pos (0, 19)),
            [(StringLiteral "'Hello'", Pos (21, 27))]),
         Pos (0, 29))],
     Pos (0, 29));;

let test_parse_create_procedure_1 () =
  test_parse_helper
    "
CREATE OR REPLACE PROCEDURE p (
    n1 NUMBER,
    n2 NUMBER
)
IS
BEGIN
    DBMS_OUTPUT.PUT_LINE('Sum is ' || (n1 + n2));
END;
"
    []
    (Program
       [(StmtCreateProcedure
           (CreateOrReplace,
            (Identifier "P", Pos (29, 29)),
            [(ArgDecl ("N1",
                       (Number (38, 127), Pos (40, 45))),
              Pos (37, 45));
             (ArgDecl ("N2",
                       (Number (38, 127), Pos (55, 60))),
              Pos (52, 60))],
            "IS",
            (Block ([],
                    [(StmtCall
                        ((BinaryOp (".",
                                    (Identifier "DBMS_OUTPUT", Pos (77, 87)),
                                    (Identifier "PUT_LINE", Pos (89, 96))),
                          Pos (77, 96)),
                         [(BinaryOp ("||",
                                     (StringLiteral "'Sum is '", Pos (98, 106)),
                                     (BinaryOp ("+",
                                                (Identifier "N1", Pos (112, 113)),
                                                (Identifier "N2", Pos (117, 118))),
                                      Pos (111, 119))),
                           Pos (98, 119))]),
                      Pos (77, 121))]),
             Pos (67, 126))),
         Pos (1, 126))],
     Pos (1, 126));;

let test_parse_create_procedure_2 () =
  test_parse_helper
    "
CREATE OR REPLACE PROCEDURE p
AS
BEGIN
    DBMS_OUTPUT.PUT_LINE('Hello, world!');
END;
"
    []
    (Program
       [(StmtCreateProcedure (CreateOrReplace,
                              (Identifier "P", Pos (29, 29)), [], "AS",
                              (Block ([],
                                      [(StmtCall
                                          ((BinaryOp (".",
                                                      (Identifier "DBMS_OUTPUT", Pos (44, 54)),
                                                      (Identifier "PUT_LINE", Pos (56, 63))),
                                            Pos (44, 63)),
                                           [(StringLiteral "'Hello, world!'", Pos (65, 79))]),
                                        Pos (44, 81))]),
                               Pos (34, 86))),
         Pos (1, 86))],
     Pos (1, 86));;

let test_parse_create_procedure_3 () =
  test_parse_helper
    "
CREATE PROCEDURE p
AS
BEGIN
    DBMS_OUTPUT.PUT_LINE('Hello, world!');
END;
"
    []
    (Program
       [(StmtCreateProcedure (Create,
                              (Identifier "P", Pos (18, 18)), [], "AS",
                              (Block ([],
                                      [(StmtCall
                                          ((BinaryOp (".",
                                                      (Identifier "DBMS_OUTPUT", Pos (33, 43)),
                                                      (Identifier "PUT_LINE", Pos (45, 52))),
                                            Pos (33, 52)),
                                           [(StringLiteral "'Hello, world!'", Pos (54, 68))]),
                                        Pos (33, 70))]),
                               Pos (23, 75))),
         Pos (1, 75))],
     Pos (1, 75));;

let test_parse_typ_number_1 () =
  test_parse_helper
    "
DECLARE
  a NUMBER;
  b NUMBER(3);
  c NUMBER(3,4);
BEGIN
END;
"
    []
    (Program
       [(Block
           ([(VarDecl ("A",
                       (Number (38, 127), Pos (13, 18))),
              Pos (11, 19));
             (VarDecl ("B",
                       (Number (3, 0), Pos (25, 33))),
              Pos (23, 34));
             (VarDecl ("C",
                       (Number (3, 4), Pos (40, 50))),
              Pos (38, 51))],
            []),
         Pos (1, 62))],
     Pos (1, 62));;

let test_parse_typ_varchar_1 () =
  test_parse_helper
    "
DECLARE
  a VARCHAR(10);
BEGIN
END;
"
    []
    (Program
       [(Block
           ([(VarDecl ("A", (Varchar 10, Pos (13, 23))), Pos (11, 24))],
            []),
         Pos (1, 35))],
     Pos (1, 35));;

let test_parse_typ_varchar_2 () =
  test_parse_helper
    "
DECLARE
  a VARCHAR2(10);
BEGIN
END;
"
    []
    (Program
       [(Block
           ([(VarDecl ("A",
                       (Varchar2 10, Pos (13, 24))),
              Pos (11, 25))],
            []),
         Pos (1, 36))],
     Pos (1, 36));;

let test_parse_create_function_1 () =
  test_parse_helper
    "
CREATE OR REPLACE FUNCTION factorial (n NUMBER(2)) RETURN NUMBER(20)
IS
BEGIN
  IF n = 1 OR n = 0 THEN
    RETURN n;
  ELSE
    RETURN n * factorial(n - 1);
  END IF;
END;
"
    []
    (Program
       [(StmtCreateFunction (CreateOrReplace,
                             (Identifier "FACTORIAL", Pos (28, 36)),
                             [(ArgDecl ("N",
                                        (Number (2, 0), Pos (41, 49))),
                               Pos (39, 49))],
                             (Number (20, 0), Pos (59, 68)), "IS",
                             (Block ([],
                                     [(StmtIf
                                         ((BinaryOp ("OR",
                                                     (BinaryOp ("=",
                                                                (Identifier "N", Pos (84, 84)),
                                                                (NumericLiteral "1", Pos (88, 88))),
                                                      Pos (84, 88)),
                                                     (BinaryOp ("=",
                                                                (Identifier "N", Pos (93, 93)),
                                                                (NumericLiteral "0", Pos (97, 97))),
                                                      Pos (93, 97))),
                                           Pos (84, 97)),
                                          [(StmtReturn
                                              (Some (Identifier "N", Pos (115, 115))),
                                            Pos (108, 116))],
                                          Else
                                            [(StmtReturn
                                                (Some
                                                   (BinaryOp ("*",
                                                              (Identifier "N", Pos (136, 136)),
                                                              (Call
                                                                 ((Identifier "FACTORIAL",
                                                                   Pos (140, 148)),
                                                                  [(BinaryOp ("-",
                                                                              (Identifier "N",
                                                                               Pos (150, 150)),
                                                                              (NumericLiteral "1",
                                                                               Pos (154, 154))),
                                                                    Pos (150, 154))]),
                                                               Pos (140, 155))),
                                                    Pos (136, 155))),
                                              Pos (129, 156))]),
                                       Pos (81, 166))]),
                              Pos (73, 171))),
         Pos (1, 171))],
     Pos (1, 171));;

let test_parse_create_function_2 () =
  test_parse_helper "
CREATE OR REPLACE FUNCTION F RETURN NUMBER
AS
BEGIN
  RETURN 3;
END;
"
    []
    (Program
       [(StmtCreateFunction (CreateOrReplace,
                             (Identifier "F", Pos (28, 28)), [],
                             (Number (38, 127), Pos (37, 42)), "AS",
                             (Block ([],
                                     [(StmtReturn
                                         (Some (NumericLiteral "3", Pos (62, 62))),
                                       Pos (55, 63))]),
                              Pos (47, 68))),
         Pos (1, 68))],
     Pos (1, 68));;

let test_parse_create_function_3 () =
  test_parse_helper "
CREATE FUNCTION F RETURN NUMBER
IS
BEGIN
  RETURN 3;
END;
"
    []
    (Program
       [(StmtCreateFunction (Create,
                             (Identifier "F", Pos (17, 17)), [],
                             (Number (38, 127), Pos (26, 31)), "IS",
                             (Block ([],
                                     [(StmtReturn
                                         (Some (NumericLiteral "3", Pos (51, 51))),
                                       Pos (44, 52))]),
                              Pos (36, 57))),
         Pos (1, 57))],
     Pos (1, 57));;

let test_parse_create_table_1 () =
  test_parse_helper "
CREATE TABLE employees (
  employee_id NUMBER(6),
  first_name VARCHAR2(50),
  last_name VARCHAR2(50),
  hire_date DATE,
  salary NUMBER(8, 2)
);
"
    []
    (Program
       [(StmtCreateTable
           ((Identifier "EMPLOYEES", Pos (14, 22)),
            [(FieldDecl ("EMPLOYEE_ID",
                         (Number (6, 0), Pos (40, 48))),
              Pos (28, 48));
             (FieldDecl ("FIRST_NAME",
                         (Varchar2 50, Pos (64, 75))),
              Pos (53, 75));
             (FieldDecl ("LAST_NAME",
                         (Varchar2 50, Pos (90, 101))),
              Pos (80, 101));
             (FieldDecl ("HIRE_DATE",
                         (Date, Pos (116, 119))),
              Pos (106, 119));
             (FieldDecl ("SALARY",
                         (Number (8, 2), Pos (131, 142))),
              Pos (124, 142))]),
         Pos (1, 145))],
     Pos (1, 145));;

let test_parse_rowtype_anchor_1 () =
  test_parse_helper "
DECLARE
  emp employees%ROWTYPE;
BEGIN
  IF emp.first_name = 'JOHN' THEN
    DBMS_OUTPUT.PUT_LINE('Hello, John!');
  END IF;
END;
"
    []
    (Program
       [(Block
           ([(VarDecl ("EMP",
                       (RowTypeAnchor
                          (Identifier "EMPLOYEES", Pos (15, 23)),
                        Pos (15, 31))),
              Pos (11, 32))],
            [(StmtIf
                ((BinaryOp ("=",
                            (BinaryOp (".",
                                       (Identifier "EMP", Pos (45, 47)),
                                       (Identifier "FIRST_NAME", Pos (49, 58))),
                             Pos (45, 58)),
                            (StringLiteral "'JOHN'", Pos (62, 67))),
                  Pos (45, 67)),
                 [(StmtCall
                     ((BinaryOp (".",
                                 (Identifier "DBMS_OUTPUT", Pos (78, 88)),
                                 (Identifier "PUT_LINE", Pos (90, 97))),
                       Pos (78, 97)),
                      [(StringLiteral "'Hello, John!'",
                        Pos (99, 112))]),
                   Pos (78, 114))],
                 NoElse),
              Pos (42, 124))]),
         Pos (1, 129))],
     Pos (1, 129));;

let test_parse_type_anchor_1 () =
  test_parse_helper "
DECLARE
  emp_name employees.first_name%TYPE;
BEGIN
  IF emp_name = 'JOHN' THEN
    DBMS_OUTPUT.PUT_LINE('Hello, John!');
  END IF;
END;
"
    []
    (Program
       [(Block
           ([(VarDecl ("EMP_NAME",
                       (TypeAnchor
                          (BinaryOp (".",
                                     (Identifier "EMPLOYEES", Pos (20, 28)),
                                     (Identifier "FIRST_NAME", Pos (30, 39))),
                           Pos (20, 39)),
                        Pos (20, 44))),
              Pos (11, 45))],
            [(StmtIf
                ((BinaryOp ("=",
                            (Identifier "EMP_NAME", Pos (58, 65)),
                            (StringLiteral "'JOHN'", Pos (69, 74))),
                  Pos (58, 74)),
                 [(StmtCall
                     ((BinaryOp (".",
                                 (Identifier "DBMS_OUTPUT", Pos (85, 95)),
                                 (Identifier "PUT_LINE", Pos (97, 104))),
                       Pos (85, 104)),
                      [(StringLiteral "'Hello, John!'",
                        Pos (106, 119))]),
                   Pos (85, 121))],
                 NoElse),
              Pos (55, 131))]),
         Pos (1, 136))],
     Pos (1, 136));;

let test_parse_record_1 () =
  test_parse_helper "
DECLARE
  TYPE point IS RECORD (
    x NUMBER;
    y NUMBER;
  );
  pt point;
BEGIN
  pt.x := 10;
  pt.y := pt.x * 2;
END;
"
    []
    (Program
       [(Block
           ([(RecordDecl
                ((Identifier "POINT", Pos (16, 20)),
                 [(FieldDecl ("X",
                              (Number (38, 127), Pos (40, 45))),
                   Pos (38, 45));
                  (FieldDecl ("Y",
                              (Number (38, 127), Pos (54, 59))),
                   Pos (52, 59))]),
              Pos (11, 65));
             (VarDecl ("PT",
                       (TypeName
                          (Identifier "POINT", Pos (72, 76)),
                        Pos (72, 76))),
              Pos (69, 77))],
            [(StmtAssignment
                ((BinaryOp (".",
                            (Identifier "PT", Pos (87, 88)),
                            (Identifier "X", Pos (90, 90))),
                  Pos (87, 90)),
                 (NumericLiteral "10", Pos (95, 96))),
              Pos (87, 97));
             (StmtAssignment
                ((BinaryOp (".",
                            (Identifier "PT", Pos (101, 102)),
                            (Identifier "Y", Pos (104, 104))),
                  Pos (101, 104)),
                 (BinaryOp ("*",
                            (BinaryOp (".",
                                       (Identifier "PT", Pos (109, 110)),
                                       (Identifier "X", Pos (112, 112))),
                             Pos (109, 112)),
                            (NumericLiteral "2", Pos (116, 116))),
                  Pos (109, 116))),
              Pos (101, 117))]),
         Pos (1, 122))],
     Pos (1, 122));;

let test_parse_table_type_1 () =
  test_parse_helper "
DECLARE
  TYPE f1_table IS TABLE OF table.f1%TYPE;
  TYPE f2_table IS TABLE OF table.f2%TYPE;
BEGIN
  NULL;
END;
"
    []
    (Program
       [(Block
           ([(TableDecl
                ((Identifier "F1_TABLE", Pos (16, 23)),
                 (TypeAnchor
                    (BinaryOp (".",
                               (Identifier "TABLE", Pos (37, 41)),
                               (Identifier "F1", Pos (43, 44))),
                     Pos (37, 44)),
                  Pos (37, 49))),
              Pos (11, 50));
             (TableDecl
                ((Identifier "F2_TABLE", Pos (59, 66)),
                 (TypeAnchor
                    (BinaryOp (".",
                               (Identifier "TABLE", Pos (80, 84)),
                               (Identifier "F2", Pos (86, 87))),
                     Pos (80, 87)),
                  Pos (80, 92))),
              Pos (54, 93))],
            [(StmtNull, Pos (103, 107))]),
         Pos (1, 112))],
     Pos (1, 112));;

let test_parse_commit_1 () =
  test_parse_helper "
COMMIT WORK NOWAIT IMMEDIATE;
"
    [(SkippedNotImplemented, "Start of statement ignored by STAN.", LineColumn(1, 0));
     (SkippedNotImplemented, "End of statement ignored by STAN.", LineColumn(1, 29))]
    (Program [(StmtNull, Pos (1, 29))],
     Pos (1, 29));;

let test_parse_ignore_create_1 () =
  test_parse_helper "
CREATE CLUSTER;
"
    [(SkippedNotImplemented, "Start of statement ignored by STAN.", LineColumn(1, 0));
     (SkippedNotImplemented, "End of statement ignored by STAN.", LineColumn(1, 15))]
    (Program [(StmtNull, Pos (1, 15))],
     Pos (1, 15));;

let test_parse_ignore_create_2 () =
  test_parse_helper "
CREATE CONTEXT
"
    [(SkippedNotImplemented, "Start of statement ignored by STAN.", LineColumn(1, 0));
     (SkippedNotImplemented, "End of statement ignored by STAN.", LineColumn(1, 14))]
    (Program [(StmtNull, Pos (1, 14))],
     Pos (1, 14));;

let test_parse_cursor_1 () =
  test_parse_helper "
DECLARE
  CURSOR c IS SELECT * FROM table;
BEGIN
  NULL;
END;
"
    []
    (Program
       [(Block
           ([(CursorDecl
                ((Identifier "C", Pos (18, 18)),
                 (Select
                    {fields =
                        [(Column
                            (Identifier "*", Pos (30, 30)),
                          Pos (30, 30))];
                     clauses =
                        [(FromClause
                            [(TableName "TABLE", Pos (37, 41))],
                          Pos (32, 41))]},
                  Pos (23, 41))),
              Pos (11, 42))],
            [(StmtNull, Pos (52, 56))]),
         Pos (1, 61))],
     Pos (1, 61));;

let test_parse_cursor_2 () =
  test_parse_helper "
DECLARE
  CURSOR c1 IS SELECT * FROM table;
BEGIN
  OPEN c1;
END;
"
    []
    (Program
       [(Block
           ([(CursorDecl
                ((Identifier "C1", Pos (18, 19)),
                 (Select
                    {fields =
                        [(Column
                            (Identifier "*", Pos (31, 31)),
                          Pos (31, 31))];
                     clauses =
                        [(FromClause
                            [(TableName "TABLE", Pos (38, 42))],
                          Pos (33, 42))]},
                  Pos (24, 42))),
              Pos (11, 43))],
            [(StmtOpen
                (Identifier "C1", Pos (58, 59)),
              Pos (53, 60))]),
         Pos (1, 65))],
     Pos (1, 65));;

let test_parse_cursor_3 () =
  test_parse_helper "
DECLARE
  CURSOR c1 IS SELECT field1, field2 FROM table;
  v1 table.field1%TYPE;
  v2 table.field2%TYPE;
BEGIN
  OPEN c1;
  LOOP
    FETCH c1 INTO v1, v2;
    EXIT WHEN c1%NOTFOUND;
    DBMS_OUTPUT.PUTLINE(v1 || ' ' || v2);
  END LOOP;
  CLOSE c1;
END;
"
    []
    (Program
       [(Block
           ([(CursorDecl
                ((Identifier "C1", Pos (18, 19)),
                 (Select
                    {fields =
                        [(Column
                            (Identifier "FIELD1", Pos (31, 36)),
                          Pos (31, 36));
                         (Column
                            (Identifier "FIELD2", Pos (39, 44)),
                          Pos (39, 44))];
                     clauses =
                        [(FromClause
                            [(TableName "TABLE", Pos (51, 55))],
                          Pos (46, 55))]},
                  Pos (24, 55))),
              Pos (11, 56));
             (VarDecl ("V1",
                       (TypeAnchor
                          (BinaryOp (".",
                                     (Identifier "TABLE", Pos (63, 67)),
                                     (Identifier "FIELD1", Pos (69, 74))),
                           Pos (63, 74)),
                        Pos (63, 79))),
              Pos (60, 80));
             (VarDecl ("V2",
                       (TypeAnchor
                          (BinaryOp (".",
                                     (Identifier "TABLE", Pos (87, 91)),
                                     (Identifier "FIELD2", Pos (93, 98))),
                           Pos (87, 98)),
                        Pos (87, 103))),
              Pos (84, 104))],
            [(StmtOpen
                (Identifier "C1", Pos (119, 120)),
              Pos (114, 121));
             (StmtLoop
                ([(StmtFetch
                     ((Identifier "C1", Pos (140, 141)),
                      [(Identifier "V1", Pos (148, 149));
                       (Identifier "V2", Pos (152, 153))]),
                   Pos (134, 154));
                  (StmtExitWhen
                     ((CursorExpr
                         ((Identifier "C1", Pos (170, 171)),
                          CursorNotFound),
                       Pos (170, 180)),
                      None),
                   Pos (160, 181));
                  (StmtCall
                     ((BinaryOp (".",
                                 (Identifier "DBMS_OUTPUT", Pos (187, 197)),
                                 (Identifier "PUTLINE", Pos (199, 205))),
                       Pos (187, 205)),
                      [(BinaryOp ("||",
                                  (BinaryOp ("||",
                                             (Identifier "V1", Pos (207, 208)),
                                             (StringLiteral "' '", Pos (213, 215))),
                                   Pos (207, 215)),
                                  (Identifier "V2", Pos (220, 221))),
                        Pos (207, 221))]),
                   Pos (187, 223))],
                 None),
              Pos (125, 235));
             (StmtClose
                (Identifier "C1", Pos (245, 246)),
              Pos (239, 247))]),
         Pos (1, 252))],
     Pos (1, 252));;

let test_parse_cursor_bulk_collect_1 () =
  test_parse_helper "
DECLARE
  TYPE f1_table IS TABLE OF table.f1%TYPE;
  TYPE f2_table IS TABLE OF table.f2%TYPE;
  CURSOR c1 IS SELECT f1, f2 FROM table;
BEGIN
  OPEN c1;
  FETCH c1 BULK COLLECT INTO f1_table, f2_table;
  CLOSE c1;
END;
"
    []
    (Program
       [(Block
           ([(TableDecl
                ((Identifier "F1_TABLE", Pos (16, 23)),
                 (TypeAnchor
                    (BinaryOp (".",
                               (Identifier "TABLE", Pos (37, 41)),
                               (Identifier "F1", Pos (43, 44))),
                     Pos (37, 44)),
                  Pos (37, 49))),
              Pos (11, 50));
             (TableDecl
                ((Identifier "F2_TABLE", Pos (59, 66)),
                 (TypeAnchor
                    (BinaryOp (".",
                               (Identifier "TABLE", Pos (80, 84)),
                               (Identifier "F2", Pos (86, 87))),
                     Pos (80, 87)),
                  Pos (80, 92))),
              Pos (54, 93));
             (CursorDecl
                ((Identifier "C1", Pos (104, 105)),
                 (Select
                    {fields =
                        [(Column
                            (Identifier "F1", Pos (117, 118)),
                          Pos (117, 118));
                         (Column
                            (Identifier "F2", Pos (121, 122)),
                          Pos (121, 122))];
                     clauses =
                        [(FromClause
                            [(TableName "TABLE", Pos (129, 133))],
                          Pos (124, 133))]},
                  Pos (110, 133))),
              Pos (97, 134))],
            [(StmtOpen
                (Identifier "C1", Pos (149, 150)),
              Pos (144, 151));
             (StmtFetchBulkCollect
                ((Identifier "C1", Pos (161, 162)),
                 [(Identifier "F1_TABLE", Pos (182, 189));
                  (Identifier "F2_TABLE", Pos (192, 199))]),
              Pos (155, 200));
             (StmtClose
                (Identifier "C1", Pos (210, 211)),
              Pos (204, 212))]),
         Pos (1, 217))],
     Pos (1, 217));;

let test_parse_cursor_4 () =
  test_parse_helper "
DECLARE
  CURSOR c1 (p1 NUMBER, p2 NUMBER) IS
  SELECT * FROM table WHERE f1 >= p1 AND f1 <= p2;
BEGIN
  NULL;
END;
"
    []
    (Program
       [(Block
           ([(ParameterizedCursorDecl
                ((Identifier "C1", Pos (18, 19)),
                 [(ArgDecl ("P1",
                            (Number (38, 127), Pos (25, 30))),
                   Pos (22, 30));
                  (ArgDecl ("P2",
                            (Number (38, 127), Pos (36, 41))),
                   Pos (33, 41))],
                 (Select
                    {fields =
                        [(Column
                            (Identifier "*", Pos (56, 56)),
                          Pos (56, 56))];
                     clauses =
                        [(FromClause
                            [(TableName "TABLE", Pos (63, 67))],
                          Pos (58, 67));
                         (WhereClause
                            (BinaryOp ("AND",
                                       (BinaryOp (">=",
                                                  (Identifier "F1", Pos (75, 76)),
                                                  (Identifier "P1", Pos (81, 82))),
                                        Pos (75, 82)),
                                       (BinaryOp ("<=",
                                                  (Identifier "F1", Pos (88, 89)),
                                                  (Identifier "P2", Pos (94, 95))),
                                        Pos (88, 95))),
                             Pos (75, 95)),
                          Pos (69, 95))]},
                  Pos (49, 95))),
              Pos (11, 96))],
            [(StmtNull, Pos (106, 110))]),
         Pos (1, 115))],
     Pos (1, 115));;

let test_parse_cursor_5 () =
  test_parse_helper "
DECLARE
  CURSOR c1 (p1 NUMBER, p2 NUMBER) IS
  SELECT * FROM table WHERE f1 >= p1 AND f1 <= p2;
BEGIN
  OPEN c1(10, 20);
END;
"
    []
    (Program
       [(Block
           ([(ParameterizedCursorDecl
                ((Identifier "C1", Pos (18, 19)),
                 [(ArgDecl ("P1",
                            (Number (38, 127), Pos (25, 30))),
                   Pos (22, 30));
                  (ArgDecl ("P2",
                            (Number (38, 127), Pos (36, 41))),
                   Pos (33, 41))],
                 (Select
                    {fields =
                        [(Column
                            (Identifier "*", Pos (56, 56)),
                          Pos (56, 56))];
                     clauses =
                        [(FromClause
                            [(TableName "TABLE", Pos (63, 67))],
                          Pos (58, 67));
                         (WhereClause
                            (BinaryOp ("AND",
                                       (BinaryOp (">=",
                                                  (Identifier "F1", Pos (75, 76)),
                                                  (Identifier "P1", Pos (81, 82))),
                                        Pos (75, 82)),
                                       (BinaryOp ("<=",
                                                  (Identifier "F1", Pos (88, 89)),
                                                  (Identifier "P2", Pos (94, 95))),
                                        Pos (88, 95))),
                             Pos (75, 95)),
                          Pos (69, 95))]},
                  Pos (49, 95))),
              Pos (11, 96))],
            [(StmtOpenParameterized
                ((Identifier "C1", Pos (111, 112)),
                 [(NumericLiteral "10", Pos (114, 115));
                  (NumericLiteral "20", Pos (118, 119))]),
              Pos (106, 121))]),
         Pos (1, 126))],
     Pos (1, 126));;

let test_parse_cursor_6 () =
  test_parse_helper "
DECLARE
  CURSOR c1 IS SELECT * FROM ADDRESSES;
BEGIN
  FOR rec IN c1
  LOOP
    DBMS_OUTPUT.PUT_LINE(rec.town);
  END LOOP;
END;
"
    []
    (Program
       [(Block
           ([(CursorDecl
                ((Identifier "C1", Pos (18, 19)),
                 (Select
                    {fields =
                        [(Column
                            (Identifier "*", Pos (31, 31)),
                          Pos (31, 31))];
                     clauses =
                        [(FromClause
                            [(TableName "ADDRESSES", Pos (38, 46))],
                          Pos (33, 46))]},
                  Pos (24, 46))),
              Pos (11, 47))],
            [(StmtForCursor
                ((Identifier "REC", Pos (61, 63)), false,
                 (Identifier "C1", Pos (68, 69)),
                 (StmtLoop
                    ([(StmtCall
                         ((BinaryOp (".",
                                     (Identifier "DBMS_OUTPUT", Pos (82, 92)),
                                     (Identifier "PUT_LINE", Pos (94, 101))),
                           Pos (82, 101)),
                          [(BinaryOp (".",
                                      (Identifier "REC", Pos (103, 105)),
                                      (Identifier "TOWN", Pos (107, 110))),
                            Pos (103, 110))]),
                       Pos (82, 112))],
                     None),
                  Pos (73, 124))),
              Pos (57, 124))]),
         Pos (1, 129))],
     Pos (1, 129));;

let test_parse_cursor_7 () =
  test_parse_helper "
BEGIN
  FOR rec IN (SELECT * FROM addresses)
  LOOP
    DBMS_OUTPUT.PUT_LINE(rec.town);
  END LOOP;
END;
"
    []
    (Program
       [(Block ([],
                [(StmtForCursor
                    ((Identifier "REC", Pos (13, 15)), false,
                     (Subquery
                        (Select
                           {fields =
                               [(Column
                                   (Identifier "*", Pos (28, 28)),
                                 Pos (28, 28))];
                            clauses =
                               [(FromClause
                                   [(TableName "ADDRESSES", Pos (35, 43))],
                                 Pos (30, 43))]},
                         Pos (21, 43)),
                      Pos (20, 44)),
                     (StmtLoop
                        ([(StmtCall
                             ((BinaryOp (".",
                                         (Identifier "DBMS_OUTPUT", Pos (57, 67)),
                                         (Identifier "PUT_LINE", Pos (69, 76))),
                               Pos (57, 76)),
                              [(BinaryOp (".",
                                          (Identifier "REC", Pos (78, 80)),
                                          (Identifier "TOWN", Pos (82, 85))),
                                Pos (78, 85))]),
                           Pos (57, 87))],
                         None),
                      Pos (48, 99))),
                  Pos (9, 99))]),
         Pos (1, 104))],
     Pos (1, 104));;

let test_parse_cursor_8 () =
  test_parse_helper "
DECLARE
  CURSOR c(town_prefix VARCHAR2) IS
  SELECT * FROM addresses WHERE town LIKE town_prefix || '%';
BEGIN
  FOR rec IN c('B')
  LOOP
    DBMS_OUTPUT.PUT_LINE(rec.town);
  END LOOP;
END;
"
    []
    (Program
       [(Block
           ([(ParameterizedCursorDecl
                ((Identifier "C", Pos (18, 18)),
                 [(ArgDecl ("TOWN_PREFIX",
                            (Varchar2NoSize, Pos (32, 39))),
                   Pos (20, 39))],
                 (Select
                    {fields =
                        [(Column
                            (Identifier "*", Pos (54, 54)),
                          Pos (54, 54))];
                     clauses =
                        [(FromClause
                            [(TableName "ADDRESSES", Pos (61, 69))],
                          Pos (56, 69));
                         (WhereClause
                            (Like
                               ((Identifier "TOWN", Pos (77, 80)),
                                (BinaryOp ("||",
                                           (Identifier "TOWN_PREFIX",
                                            Pos (87, 97)),
                                           (StringLiteral "'%'", Pos (102, 104))),
                                 Pos (87, 104))),
                             Pos (77, 104)),
                          Pos (71, 104))]},
                  Pos (47, 104))),
              Pos (11, 105))],
            [(StmtForParameterizedCursor
                ((Identifier "REC", Pos (119, 121)), false,
                 (Identifier "C", Pos (126, 126)),
                 [(StringLiteral "'B'", Pos (128, 130))],
                 (StmtLoop
                    ([(StmtCall
                         ((BinaryOp (".",
                                     (Identifier "DBMS_OUTPUT",
                                      Pos (144, 154)),
                                     (Identifier "PUT_LINE", Pos (156, 163))),
                           Pos (144, 163)),
                          [(BinaryOp (".",
                                      (Identifier "REC", Pos (165, 167)),
                                      (Identifier "TOWN", Pos (169, 172))),
                            Pos (165, 172))]),
                       Pos (144, 174))],
                     None),
                  Pos (135, 186))),
              Pos (115, 186))]),
         Pos (1, 191))],
     Pos (1, 191));;

let suite = "Select tests" >::: [ "test_parse_begin_end" >:: test_parse_begin_end;
                                  "test_parse_declare_begin_end" >::
                                    test_parse_declare_begin_end;
                                  "test_parse_empty_block_with_decl" >::
                                    test_parse_empty_block_with_decl;
                                  "test_parse_begin_end_no_semicolon" >::
                                    test_parse_begin_end_no_semicolon;
                                  "test_parse_simple_complete_block_1" >::
                                    test_parse_simple_complete_block_1;
                                  "test_parse_simple_complete_block_2" >::
                                    test_parse_simple_complete_block_2;

                                  "test_parse_select_1" >:: test_parse_select_1;
                                  "test_parse_select_2" >:: test_parse_select_2;

                                  "test_parse_if_1" >:: test_parse_if_1;
                                  "test_parse_if_2" >:: test_parse_if_2;
                                  "test_parse_if_3" >:: test_parse_if_3;
                                  "test_parse_if_4" >:: test_parse_if_4;
                                  "test_parse_if_5" >:: test_parse_if_5;
                                  "test_parse_if_6" >:: test_parse_if_6;
                                  "test_parse_if_7" >:: test_parse_if_7;

                                  "test_parse_basic_loop_1" >:: test_parse_basic_loop_1;
                                  "test_parse_basic_loop_2" >:: test_parse_basic_loop_2;
                                  "test_parse_basic_loop_3" >:: test_parse_basic_loop_3;

                                  "test_parse_exit" >:: test_parse_exit;
                                  "test_parse_exit_when" >:: test_parse_exit_when;
                                  "test_parse_exit_labeled" >:: test_parse_exit_labeled;
                                  "test_parse_exit_when_labeled" >::
                                    test_parse_exit_when_labeled;

                                  "test_parse_continue" >:: test_parse_continue;
                                  "test_parse_continue_when" >:: test_parse_continue_when;
                                  "test_parse_continue_labeled" >::
                                    test_parse_continue_labeled;
                                  "test_parse_continue_when_labeled" >::
                                    test_parse_continue_when_labeled;

                                  "test_parse_for_1" >:: test_parse_for_1;
                                  "test_parse_for_2" >:: test_parse_for_2;
                                  "test_parse_for_3" >:: test_parse_for_3;
                                  "test_parse_for_4" >:: test_parse_for_4;

                                  "test_parse_while_1" >:: test_parse_while_1;

                                  "test_parse_goto_1" >:: test_parse_goto_1;

                                  "test_parse_null_1" >:: test_parse_null_1;

                                  "test_parse_call_1" >:: test_parse_call_1;

                                  "test_parse_create_procedure_1" >::
                                    test_parse_create_procedure_1;
                                  "test_parse_create_procedure_2" >::
                                    test_parse_create_procedure_2;
                                  "test_parse_create_procedure_3" >::
                                    test_parse_create_procedure_3;

                                  "test_parse_typ_number_1" >::
                                    test_parse_typ_number_1;
                                  "test_parse_typ_varchar_1" >::
                                    test_parse_typ_varchar_1;
                                  "test_parse_typ_varchar_2" >::
                                    test_parse_typ_varchar_2;
                                  "test_parse_rowtype_anchor_1" >::
                                    test_parse_rowtype_anchor_1;
                                  "test_parse_type_anchor_1" >::
                                    test_parse_type_anchor_1;
                                  "test_parse_record_1" >::
                                    test_parse_record_1;
                                  "test_parse_table_type_1" >::
                                    test_parse_table_type_1;

                                  "test_parse_create_function_1" >::
                                    test_parse_create_function_1;
                                  "test_parse_create_function_2" >::
                                    test_parse_create_function_2;
                                  "test_parse_create_function_3" >::
                                    test_parse_create_function_3;

                                  "test_parse_create_table_1" >::
                                    test_parse_create_table_1;

                                  "test_parse_commit_1" >:: test_parse_commit_1;
                                  "test_parse_ignore_create_1" >::
                                    test_parse_ignore_create_1;
                                  "test_parse_ignore_create_2" >::
                                    test_parse_ignore_create_2;

                                  "test_parse_cursor_1" >:: test_parse_cursor_1;
                                  "test_parse_cursor_2" >:: test_parse_cursor_2;
                                  "test_parse_cursor_3" >:: test_parse_cursor_3;
                                  "test_parse_cursor_bulk_collect_1" >::
                                    test_parse_cursor_bulk_collect_1;
                                  "test_parse_cursor_4" >:: test_parse_cursor_4;
                                  "test_parse_cursor_5" >:: test_parse_cursor_5;
                                  "test_parse_cursor_6" >:: test_parse_cursor_6;
                                  "test_parse_cursor_7" >:: test_parse_cursor_7;
                                  "test_parse_cursor_8" >:: test_parse_cursor_8;
                                ];;
