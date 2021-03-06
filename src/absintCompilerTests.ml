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
open PlsqlParser.Ast;;
open Utils;;
open AbsintCompiler;;
open Absint.Ir;;
open PlsqlParser;;
open ParserTypes;;

let compile_test_helper str expected_ir =
  let actual_ir = compile_helper str in
    assert_equal expected_ir actual_ir

let test_compile_simple_program () =
  compile_test_helper
    "DECLARE N NUMBER(3); BEGIN NULL; END;"
    [AddFrame;
     Declare ("N", (Number (3, 0), Pos (10, 18)));
     DeleteFrame];;

let test_compile_assignment () =
  compile_test_helper
    "DECLARE N NUMBER(3); BEGIN N := 3 + 5; END;"
    [AddFrame;
     Declare ("N", (Number (3, 0), Pos (10, 18)));
     Assignment ((Identifier "N", Pos (27, 27)),
                 (BinaryOp ("+",
                            (NumericLiteral "3", Pos (32, 32)),
                            (NumericLiteral "5", Pos (36, 36))),
                  Pos (32, 36)));
     DeleteFrame];;

let test_compile_call () =
  compile_test_helper
    "BEGIN DBMS_OUTPUT.PUT_LINE('Hello, world'); END;"
    [AddFrame;
     Call
       ((BinaryOp (".",
                   (Identifier "DBMS_OUTPUT", Pos (6, 16)),
                   (Identifier "PUT_LINE", Pos (18, 25))),
         Pos (6, 25)),
        [(StringLiteral "'Hello, world'", Pos (27, 40))]);
     DeleteFrame];;

let test_compile_if () =
  compile_test_helper
    "
BEGIN
  IF N = 2 THEN
    DBMS_OUTPUT.PUT_LINE('2');
  ELSE
    DBMS_OUTPUT.PUT_LINE('Not 2');
  END IF;
END;"
    [AddFrame;
     GotoIf
       ((BinaryOp ("=",
                   (Identifier "N", Pos (12, 12)),
                   (NumericLiteral "2", Pos (16, 16))),
         Pos (12, 16)),
        "Then_1", "Else_2");
     Label "Then_1";
     Call
       ((BinaryOp (".",
                   (Identifier "DBMS_OUTPUT", Pos (27, 37)),
                   (Identifier "PUT_LINE", Pos (39, 46))),
         Pos (27, 46)),
        [(StringLiteral "'2'", Pos (48, 50))]);
     Goto ("AfterIf_3", None);
     Label "Else_2";
     Call
       ((BinaryOp (".",
                   (Identifier "DBMS_OUTPUT", Pos (65, 75)),
                   (Identifier "PUT_LINE", Pos (77, 84))),
         Pos (65, 84)),
        [(StringLiteral "'Not 2'", Pos (86, 92))]);
     Label "AfterIf_3";
     DeleteFrame];;

let test_compile_if_no_else () =
  compile_test_helper
    "
BEGIN
  IF N = 2 THEN
    DBMS_OUTPUT.PUT_LINE('2');
  END IF;
END;"
    [AddFrame;
     GotoIf
       ((BinaryOp ("=",
                   (Identifier "N", Pos (12, 12)),
                   (NumericLiteral "2", Pos (16, 16))),
         Pos (12, 16)),
        "Then_1", "Else_2");
     Label "Then_1";
     Call
       ((BinaryOp (".",
                   (Identifier "DBMS_OUTPUT", Pos (27, 37)),
                   (Identifier "PUT_LINE", Pos (39, 46))),
         Pos (27, 46)),
        [(StringLiteral "'2'", Pos (48, 50))]);
     Goto ("AfterIf_3", None);
     Label "Else_2";
     Label "AfterIf_3";
     DeleteFrame];;

let test_compile_if_elsif () =
  compile_test_helper
    "
BEGIN
  IF N = 2 THEN
    DBMS_OUTPUT.PUT_LINE('2');
  ELSIF N = 3 THEN
    DBMS_OUTPUT.PUT_LINE('3');
  ELSE
    DBMS_OUTPUT.PUT_LINE('Not 2 or 3');
  END IF;
END;"
    [AddFrame;
     GotoIf
       ((BinaryOp ("=",
                   (Identifier "N", Pos (12, 12)),
                   (NumericLiteral "2", Pos (16, 16))),
         Pos (12, 16)),
        "Then_1", "Else_2");
     Label "Then_1";
     Call
       ((BinaryOp (".",
                   (Identifier "DBMS_OUTPUT", Pos (27, 37)),
                   (Identifier "PUT_LINE", Pos (39, 46))),
         Pos (27, 46)),
        [(StringLiteral "'2'", Pos (48, 50))]);
     Goto ("AfterIf_3", None);
     Label "Else_2";
     GotoIf
       ((BinaryOp ("=",
                   (Identifier "N", Pos (62, 62)),
                   (NumericLiteral "3", Pos (66, 66))),
         Pos (62, 66)),
        "Then_4", "Else_5");
     Label "Then_4";
     Call
       ((BinaryOp (".",
                   (Identifier "DBMS_OUTPUT", Pos (77, 87)),
                   (Identifier "PUT_LINE", Pos (89, 96))),
         Pos (77, 96)),
        [(StringLiteral "'3'", Pos (98, 100))]);
     Goto ("AfterIf_6", None);
     Label "Else_5";
     Call
       ((BinaryOp (".",
                   (Identifier "DBMS_OUTPUT", Pos (115, 125)),
                   (Identifier "PUT_LINE", Pos (127, 134))),
         Pos (115, 134)),
        [(StringLiteral "'Not 2 or 3'", Pos (136, 147))]);
     Label "AfterIf_6";
     Label "AfterIf_3";
     DeleteFrame];;

let test_compile_loop_exit () =
  compile_test_helper
    "
DECLARE
  N NUMBER(2);
BEGIN
  N := 1;
  LOOP
    DBMS_OUTPUT.PUT_LINE(N);
    N := N + 1;
    EXIT WHEN N > 10;
  END LOOP;
END;"
    [AddFrame;
     Declare ("N", (Number (2, 0), Pos (13, 21)));
     Assignment ((Identifier "N", Pos (32, 32)),
                 (NumericLiteral "1", Pos (37, 37)));
     Label "BeforeLoop_1";
     Call
       ((BinaryOp (".",
                   (Identifier "DBMS_OUTPUT", Pos (51, 61)),
                   (Identifier "PUT_LINE", Pos (63, 70))),
         Pos (51, 70)),
        [(Identifier "N", Pos (72, 72))]);
     Assignment ((Identifier "N", Pos (80, 80)),
                 (BinaryOp ("+",
                            (Identifier "N", Pos (85, 85)),
                            (NumericLiteral "1", Pos (89, 89))),
                  Pos (85, 89)));
     GotoIf
       ((BinaryOp (">",
                   (Identifier "N", Pos (106, 106)),
                   (NumericLiteral "10", Pos (110, 111))),
         Pos (106, 111)),
        "AfterLoop_2", "Next_3");
     Label "Next_3";
     Goto ("BeforeLoop_1", None);
     Label "AfterLoop_2";
     DeleteFrame];;

let test_compile_labeled_loop_exit () =
  compile_test_helper
    "
DECLARE
  N NUMBER(2);
BEGIN
  N := 1;
  <<outer>>
  LOOP
    DBMS_OUTPUT.PUT_LINE(N);
    N := N + 1;
    EXIT outer WHEN N > 10;
  END LOOP;
END;"
    [AddFrame;
     Declare ("N", (Number (2, 0), Pos (13, 21)));
     Assignment ((Identifier "N", Pos (32, 32)),
                 (NumericLiteral "1", Pos (37, 37)));
     Label "UserLabel_OUTER";
     Label "UserLabel_OUTER_BeforeLoop";
     Call
       ((BinaryOp (".",
                   (Identifier "DBMS_OUTPUT", Pos (63, 73)),
                   (Identifier "PUT_LINE", Pos (75, 82))),
         Pos (63, 82)),
        [(Identifier "N", Pos (84, 84))]);
     Assignment ((Identifier "N", Pos (92, 92)),
                 (BinaryOp ("+",
                            (Identifier "N", Pos (97, 97)),
                            (NumericLiteral "1", Pos (101, 101))),
                  Pos (97, 101)));
     GotoIf
       ((BinaryOp (">",
                   (Identifier "N", Pos (124, 124)),
                   (NumericLiteral "10", Pos (128, 129))),
         Pos (124, 129)),
        "UserLabel_OUTER_AfterLoop", "Next_1");
     Label "Next_1";
     Goto ("UserLabel_OUTER_BeforeLoop", None);
     Label "UserLabel_OUTER_AfterLoop";
     DeleteFrame];;

let test_compile_labeled_nested_loop_exit () =
  compile_test_helper
    "
DECLARE
  N NUMBER(2);
BEGIN
  N := 1;
  <<outer>>
  LOOP
    DBMS_OUTPUT.PUT_LINE(N);
    LOOP
      N := N + 1;
      EXIT outer WHEN N > 10;
    END LOOP;
  END LOOP;
END;"
    [AddFrame;
     Declare ("N", (Number (2, 0), Pos (13, 21)));
     Assignment ((Identifier "N", Pos (32, 32)),
                 (NumericLiteral "1", Pos (37, 37)));
     Label "UserLabel_OUTER";
     Label "UserLabel_OUTER_BeforeLoop";
     Call
       ((BinaryOp (".",
                   (Identifier "DBMS_OUTPUT", Pos (63, 73)),
                   (Identifier "PUT_LINE", Pos (75, 82))),
         Pos (63, 82)),
        [(Identifier "N", Pos (84, 84))]);
     Label "BeforeLoop_1";
     Assignment ((Identifier "N", Pos (103, 103)),
                 (BinaryOp ("+",
                            (Identifier "N", Pos (108, 108)),
                            (NumericLiteral "1", Pos (112, 112))),
                  Pos (108, 112)));
     GotoIf
       ((BinaryOp (">",
                   (Identifier "N", Pos (137, 137)),
                   (NumericLiteral "10", Pos (141, 142))),
         Pos (137, 142)),
        "UserLabel_OUTER_AfterLoop", "Next_3");
     Label "Next_3";
     Goto ("BeforeLoop_1", None);
     Label "AfterLoop_2";
     Goto ("UserLabel_OUTER_BeforeLoop", None);
     Label "UserLabel_OUTER_AfterLoop";
     DeleteFrame];;

let test_compile_while () =
  compile_test_helper
    "
DECLARE
  N NUMBER(2);
BEGIN
  N := 1;
  WHILE N <= 10
  LOOP
    DBMS_OUTPUT.PUT_LINE(N);
    N := N + 1;
  END LOOP;
END;"
    [AddFrame;
     Declare ("N", (Number (2, 0), Pos (13, 21)));
     Assignment ((Identifier "N", Pos (32, 32)),
                 (NumericLiteral "1", Pos (37, 37)));
     Label "BeforeLoop_1";
     GotoIf
       ((BinaryOp ("<=",
                   (Identifier "N", Pos (48, 48)),
                   (NumericLiteral "10", Pos (53, 54))),
         Pos (48, 54)),
        "WhileBodyStart_3", "AfterLoop_2");
     Label "WhileBodyStart_3";
     Call
       ((BinaryOp (".",
                   (Identifier "DBMS_OUTPUT", Pos (67, 77)),
                   (Identifier "PUT_LINE", Pos (79, 86))),
         Pos (67, 86)),
        [(Identifier "N", Pos (88, 88))]);
     Assignment ((Identifier "N", Pos (96, 96)),
                 (BinaryOp ("+",
                            (Identifier "N", Pos (101, 101)),
                            (NumericLiteral "1", Pos (105, 105))),
                  Pos (101, 105)));
     Goto ("BeforeLoop_1", None);
     Label "AfterLoop_2";
     DeleteFrame];;

let test_compile_for () =
  compile_test_helper
    "
BEGIN
  FOR N IN 1..10
  LOOP
    DBMS_OUTPUT.PUT_LINE(N);
  END LOOP;
END;"
    [AddFrame;
     AddFrame;
     Declare ("N", (Number (38, 127), Pos (0, 0)));
     Assignment ((Identifier "N", Pos (13, 13)),
                 (NumericLiteral "1", Pos (18, 18)));
     Label "BeforeLoop_1";
     GotoIf
       ((BinaryOp ("<=",
                   (Identifier "N", Pos (13, 13)),
                   (NumericLiteral "10", Pos (21, 22))),
         Pos (0, 0)),
        "ForBodyStart_3", "AfterLoop_2");
     Label "ForBodyStart_3";
     Call
       ((BinaryOp (".",
                   (Identifier "DBMS_OUTPUT", Pos (35, 45)),
                   (Identifier "PUT_LINE", Pos (47, 54))),
         Pos (35, 54)),
        [(Identifier "N", Pos (56, 56))]);
     Assignment ((Identifier "N", Pos (13, 13)),
                 (BinaryOp ("+",
                            (Identifier "N", Pos (13, 13)),
                            (NumericLiteral "1", Pos (0, 0))),
                  Pos (0, 0)));
     Goto ("BeforeLoop_1", None);
     Label "AfterLoop_2";
     DeleteFrame;
     DeleteFrame];;

let test_compile_for_nested_block () =
  compile_test_helper
    "
BEGIN
  FOR N IN 1..10
  LOOP
    DECLARE
      M NUMBER(3);
    BEGIN
      EXIT WHEN M IS NULL;
      DBMS_OUTPUT.PUT_LINE(N + M);
    END;
  END LOOP;
END;"
    [AddFrame;
     AddFrame;
     Declare ("N", (Number (38, 127), Pos (0, 0)));
     Assignment ((Identifier "N", Pos (13, 13)),
                 (NumericLiteral "1", Pos (18, 18)));
     Label "BeforeLoop_1";
     GotoIf
       ((BinaryOp ("<=",
                   (Identifier "N", Pos (13, 13)),
                   (NumericLiteral "10", Pos (21, 22))),
         Pos (0, 0)),
        "ForBodyStart_3", "AfterLoop_2");
     Label "ForBodyStart_3";
     AddFrame;
     Declare ("M", (Number (3, 0), Pos (51, 59)));
     GotoIf
       ((IsNull (Identifier "M", Pos (88, 88)),
         Pos (88, 96)),
        "DischargeEnv_5", "Next_4");
     Label "DischargeEnv_5";
     DeleteFrame;
     Goto ("AfterLoop_2", None);
     Label "Next_4";
     Call
       ((BinaryOp (".",
                   (Identifier "DBMS_OUTPUT", Pos (105, 115)),
                   (Identifier "PUT_LINE", Pos (117, 124))),
         Pos (105, 124)),
        [(BinaryOp ("+",
                    (Identifier "N", Pos (126, 126)),
                    (Identifier "M", Pos (130, 130))),
          Pos (126, 130))]);
     DeleteFrame;
     Assignment ((Identifier "N", Pos (13, 13)),
                 (BinaryOp ("+",
                            (Identifier "N", Pos (13, 13)),
                            (NumericLiteral "1", Pos (0, 0))),
                  Pos (0, 0)));
     Goto ("BeforeLoop_1", None);
     Label "AfterLoop_2";
     DeleteFrame;
     DeleteFrame];;

let test_compile_nested_for () =
  compile_test_helper
    "
BEGIN
  <<outer>>
  FOR M IN 1..10
  LOOP
    FOR N IN 1..10
    LOOP
      EXIT outer WHEN M = 2;
      DBMS_OUTPUT.PUT_LINE(N + M);
    END LOOP;
  END LOOP;
END;"
    [AddFrame;
     Label "UserLabel_OUTER";
     AddFrame;
     Declare ("M", (Number (38, 127), Pos (0, 0)));
     Assignment ((Identifier "M", Pos (25, 25)),
                 (NumericLiteral "1", Pos (30, 30)));
     Label "UserLabel_OUTER_BeforeLoop";
     GotoIf
       ((BinaryOp ("<=",
                   (Identifier "M", Pos (25, 25)),
                   (NumericLiteral "10", Pos (33, 34))),
         Pos (0, 0)),
        "ForBodyStart_1", "UserLabel_OUTER_AfterLoop");
     Label "ForBodyStart_1";
     AddFrame;
     Declare ("N", (Number (38, 127), Pos (0, 0)));
     Assignment ((Identifier "N", Pos (51, 51)),
                 (NumericLiteral "1", Pos (56, 56)));
     Label "BeforeLoop_2";
     GotoIf
       ((BinaryOp ("<=",
                   (Identifier "N", Pos (51, 51)),
                   (NumericLiteral "10", Pos (59, 60))),
         Pos (0, 0)),
        "ForBodyStart_4", "AfterLoop_3");
     Label "ForBodyStart_4";
     GotoIf
       ((BinaryOp ("=",
                   (Identifier "M", Pos (93, 93)),
                   (NumericLiteral "2", Pos (97, 97))),
         Pos (93, 97)),
        "DischargeEnv_6", "Next_5");
     Label "DischargeEnv_6";
     DeleteFrame;
     Goto ("UserLabel_OUTER_AfterLoop", None);
     Label "Next_5";
     Call
       ((BinaryOp (".",
                   (Identifier "DBMS_OUTPUT", Pos (106, 116)),
                   (Identifier "PUT_LINE", Pos (118, 125))),
         Pos (106, 125)),
        [(BinaryOp ("+",
                    (Identifier "N", Pos (127, 127)),
                    (Identifier "M", Pos (131, 131))),
          Pos (127, 131))]);
     Assignment ((Identifier "N", Pos (51, 51)),
                 (BinaryOp ("+",
                            (Identifier "N", Pos (51, 51)),
                            (NumericLiteral "1", Pos (0, 0))),
                  Pos (0, 0)));
     Goto ("BeforeLoop_2", None);
     Label "AfterLoop_3";
     DeleteFrame;
     Assignment ((Identifier "M", Pos (25, 25)),
                 (BinaryOp ("+",
                            (Identifier "M", Pos (25, 25)),
                            (NumericLiteral "1", Pos (0, 0))),
                  Pos (0, 0)));
     Goto ("UserLabel_OUTER_BeforeLoop", None);
     Label "UserLabel_OUTER_AfterLoop";
     DeleteFrame;
     DeleteFrame];;

let test_label_depth_helper str expected =
  parse2_cont str (fun ast ->
                     let label_depths = compute_label_depth ast |> list_of_map in
                       assert_equal expected label_depths);;

let test_label_depth_1 () =
  test_label_depth_helper
    "
BEGIN
  <<label1>>
  N := 1;
END;"
    [("LABEL1", 1)];;

let suite = "Absint tests" >::: [
  "test_compile_simple_program" >:: test_compile_simple_program;
  "test_compile_assignment" >:: test_compile_assignment;
  "test_compile_call" >:: test_compile_call;
  "test_compile_if" >:: test_compile_if;
  "test_compile_if_no_else" >:: test_compile_if_no_else;
  "test_compile_if_elsif" >:: test_compile_if_elsif;
  "test_compile_loop_exit" >:: test_compile_loop_exit;
  "test_compile_labeled_loop_exit" >:: test_compile_labeled_loop_exit;
  "test_compile_labeled_nested_loop_exit" >:: test_compile_labeled_nested_loop_exit;
  "test_compile_while" >:: test_compile_while;
  "test_compile_for" >:: test_compile_for;
  "test_compile_for_nested_block" >:: test_compile_for_nested_block;
  "test_compile_nested_for" >:: test_compile_nested_for;

  (* Label depth tests - will expand if label depths prove useful. *)
  "test_label_depth_1" >:: test_label_depth_1;
];;
