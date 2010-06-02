
open OUnit;;
open ParserTypes;;
open Lexer;;
open PlsqlParser;;
open Pwm;;

let test_pwm_lookahead () =
  let tokens, _ = tokenize "" 0 in
  let result = run_parser_helper lookahead tokens in
    assert_equal ([], Some (Stream (None, []), None)) result;;

let test_pwm_eoi () =
  let result_1 = run_parser eoi (Stream(None, []), []) in
  let tokens, _ = tokenize "BEGIN" 0 in
  let result_2 = run_parser eoi (Stream(None, tokens), []) in
    assert_equal ([], Some (Stream (None, []), true)) result_1;
    assert_equal ([], Some (Stream (None, tokens), false)) result_2;;

let test_pwm_consume_1 () =
  let tokens, _ = tokenize "" 0 in
  let result = run_parser (consume "BEGIN") (Stream(None, tokens), []) in
    match result with
      | [warning], None ->
          assert_equal
            (Warning(Error, "Expected 'BEGIN' but reached end of input.", 0))
            warning
      | _ ->
          assert_failure "Expected a warning.";;

let test_pwm_consume_2 () =
  let tokens, _ = tokenize "BEGIN END" 0 in
  let result = run_parser
    (consume "BEGIN" <+> consume "BEGIN")
    (Stream(None, tokens), [])
  in
    match result with
      | [warning], None ->
          assert_equal (Warning (Error, "Expected 'BEGIN' but got 'END'.", 6)) warning
      | _ ->
          assert_failure "Expected a warning.";;

let test_pwm_until_eoi () =
  let tokens, _ = tokenize "BEGIN END BEGIN" 0 in
  let result =
    run_parser (until_eoi (consume "BEGIN" <+> consume "END")) (Stream(None, tokens), [])
  in
    match result with
      | [warning], None ->
          assert_equal
            (Warning (Error, "Expected 'END' but reached end of input.", 15))
            warning
      | _ ->
          assert_failure "Expected a warning.";;

let test_pwm_consume_or_fake () =
  let tokens, _ = tokenize "BEGIN END" 0 in
  let parser = consume "BEGIN" <+> consume "END" <+> consume_or_fake ";" in
  let result = run_parser parser (Stream(None, tokens), []) in
    match result with
      | [warning], _ ->
          assert_equal (Warning (Error, "Expected ';'.", 8)) warning
      | _ ->
          assert_failure "Expected a warning.";;

let suite = "Pwm tests" >::: ["test_pwm_lookahead" >:: test_pwm_lookahead;
                              "test_pwm_eoi" >:: test_pwm_eoi;
                              "test_pwm_consume_1" >:: test_pwm_consume_1;
                              "test_pwm_consume_2" >:: test_pwm_consume_2;
                              "test_pwm_until_eoi" >:: test_pwm_until_eoi;
                              "test_pwm_consume_or_fake" >:: test_pwm_consume_or_fake;
                             ]

