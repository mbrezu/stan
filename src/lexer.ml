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

open ParserTypes;;
open Utils;;

let is_digit ch = ch >= '0' && ch <= '9';;
let is_letter ch = ch >= 'A' && ch <= 'Z' || ch >= 'a' && ch <= 'z';;
let is_alnum ch = is_letter ch || is_digit ch;;

let tokenize str start_pos =
  let is_ws ch = ch == ' ' || ch == '\n' || ch == '\r' || ch == '\t' in
  let lex_while str start_pos pred =
    let rec lex_while' pos =
      if String.length str <= pos then pos - 1
      else (if pred str.[pos] then lex_while' (pos + 1)
            else pos - 1)
    in
    let final_pos = lex_while' start_pos in
      if final_pos < start_pos
      then failwith "Lexer internal error."
      else
        Token (String.sub str start_pos (final_pos - start_pos + 1) |> String.uppercase,
               Pos (start_pos, final_pos)), final_pos + 1
  in
  let parse_string_literal str str_pos =
    let rec psl_iter current_pos =
      if current_pos >= String.length str
      then (Token(String.sub str (str_pos - 1) (current_pos - str_pos + 1),
                  Pos(str_pos - 1, current_pos - 1)),
            current_pos)
      else if str.[current_pos] = '\''
        && current_pos + 1 < String.length str
        && str.[current_pos + 1] = '\''
      then psl_iter (current_pos + 2)
      else if str.[current_pos] = '\''
      then (Token(String.sub str (str_pos - 1) (current_pos - str_pos + 2),
                  Pos(str_pos - 1, current_pos)),
            current_pos + 1)
      else psl_iter (current_pos + 1)
    in
      psl_iter str_pos
  in
  let rec tokenize' str start_pos acc =
    if String.length str <= start_pos
    then List.rev acc, start_pos
    else
      let cont new_pos parsed = tokenize' str new_pos (parsed :: acc) in
      let firstChar = str.[start_pos] in
        if is_letter firstChar then
          let ident_pred ch = is_alnum ch || ch = '_' in
          let parsed, new_pos = lex_while str start_pos ident_pred in
            cont new_pos parsed
        else if is_digit firstChar then
          let parsed, new_pos = lex_while str start_pos is_digit in
            cont new_pos parsed
        else if is_ws firstChar then
          tokenize' str (start_pos + 1) acc
        else if firstChar = '\'' then
          let parsed, new_pos = parse_string_literal str (start_pos + 1) in
            cont new_pos parsed
        else
          let token = Token (String.sub str start_pos 1,
                             Pos (start_pos, start_pos)) in
            cont (start_pos + 1) token
  in
    tokenize' str start_pos [];;

let convert_pos_to_line_col text sorted_positions =
  let text_len = String.length text in
  let rec convert_pos_iter text_pos line column sorted_positions line_cols =
    if text_pos >= text_len || sorted_positions = []
    then
      let rest_of_positions =
        List.map (fun _ -> LineColumn(line, column)) sorted_positions
      in
        (List.rev line_cols) @ rest_of_positions
    else
      let is_newline = text.[text_pos] = '\n' in
      let new_line = if is_newline then line + 1 else line in
      let new_column = if is_newline then 0 else column + 1 in
        if List.hd sorted_positions = text_pos
        then
            convert_pos_iter
              (text_pos + 1)
              new_line
              new_column
              (List.tl sorted_positions)
              (LineColumn(line, column) :: line_cols)
        else
          convert_pos_iter
            (text_pos + 1)
            new_line
            new_column
            sorted_positions
            line_cols
  in
    convert_pos_iter 0 0 0 sorted_positions [];;
