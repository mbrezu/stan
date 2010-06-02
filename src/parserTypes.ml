type pos = Pos of int * int;;

type line_column = LineColumn of int * int;;

type warning_kind = Error | SkippedError | SkippedNotImplemented

type warning = Warning of (warning_kind * string * int);;

type token = Token of (string * pos);;

type 'a stream = Stream of ('a option * 'a list);;

type 'a input = 'a stream * warning list;;

type ('a, 'b) parser = ParserM of ('a input -> (warning list) * ('a stream * 'b) option);;

let token_content (Token(content, _)) = content;;

let token_pos (Token(_, pos)) = pos;;
