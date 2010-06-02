
open OUnit;;
open ParserTypes;;
open Lexer;;

let test_lex_helper str expected =
  let tokens, _ = tokenize str 0 in
    assert_equal expected tokens;;

let test_lex_begin_end () =
  test_lex_helper
    "BEGIN END;"
    [Token ("BEGIN", Pos (0, 4));
     Token ("END", Pos (6, 8));
     Token (";", Pos (9, 9))];;

let test_lex_simple_select () =
  test_lex_helper
    "SELECT Field1 FROM TestTable;"
    [Token ("SELECT", Pos (0, 5));
     Token ("FIELD1", Pos (7, 12));
     Token ("FROM", Pos (14, 17));
     Token ("TESTTABLE", Pos (19, 27));
     Token (";", Pos (28, 28))];;

let test_lex_string_literal () =
  test_lex_helper
    "\'Test\' 1 \'\'\'Test2\'\'\' \'Test3"
    [Token ("'Test'", Pos (0, 5));
     Token ("1", Pos (7, 7));
     Token ("'''Test2'''", Pos (9, 19));
     Token ("'Test3", Pos (21, 26))];;

let test_convert_pos_1 () =
  let positions = [0; 10; 20;] in
  let text = "Some line,\nAnother line,\nLast line." in
  let expected = [LineColumn(0, 0);
                  LineColumn(0, 10);
                  LineColumn(1, 9)] in
  let actual = convert_pos_to_line_col text positions in
    assert_equal expected actual;;

let test_convert_pos_2 () =
  let positions = [0; 10; 20; 100] in
  let text = "Some line,\nAnother line,\nLast line." in
  let expected = [LineColumn(0, 0);
                  LineColumn(0, 10);
                  LineColumn(1, 9);
                  LineColumn(2, 10)] in
  let actual = convert_pos_to_line_col text positions in
    assert_equal expected actual;;

let test_convert_pos_3 () =
  let positions = [0; 10; 20; 35; 100; 200] in
  let text = "Some line,\n\nAnother line,\nLast line." in
  let expected = [LineColumn (0, 0);
                  LineColumn (0, 10);
                  LineColumn (2, 8);
                  LineColumn (3, 9);
                  LineColumn (3, 10);
                  LineColumn (3, 10)] in
  let actual = convert_pos_to_line_col text positions in
    assert_equal expected actual;;

let suite = "Lexer tests" >::: ["test_lex_begin_end" >:: test_lex_begin_end;
                                "test_lex_simple_select" >:: test_lex_simple_select;
                                "test_lex_string_literal" >:: test_lex_string_literal;
                                "test_convert_pos_1" >:: test_convert_pos_1;
                                "test_convert_pos_2" >:: test_convert_pos_2;
                                "test_convert_pos_3" >:: test_convert_pos_3;
                               ]

