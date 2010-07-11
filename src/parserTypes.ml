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

type pos = Pos of int * int;;

type line_column = LineColumn of int * int;;

type warning_kind = Error | SkippedError | SkippedNotImplemented

type warning = Warning of (warning_kind * string * int);;

type token = Token of (string * pos);;

let token_content (Token(content, _)) = content;;

let token_pos (Token(_, pos)) = pos;;
