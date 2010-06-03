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
     Label "BeforeWhile_1";
     GotoIf
       ((BinaryOp ("<=",
                   (Identifier "N", Pos (48, 48)),
                   (NumericLiteral "10", Pos (53, 54))),
         Pos (48, 54)),
        "WhileBodyStart_3", "AfterWhile_2");
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
     Goto ("BeforeWhile_1", None);
     Label "AfterWhile_2";
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

  (* Label depth tests - will expand if label depths prove useful. *)
  "test_label_depth_1" >:: test_label_depth_1;
];;
