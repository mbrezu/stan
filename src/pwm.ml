
open ParserTypes;;
open Printf;;
open Utils;;

type 'a stream = Stream of ('a option * 'a list);;

type 'a input = 'a stream * warning list;;

type ('a, 'b) parser = ParserM of ('a input -> (warning list) * ('a stream * 'b) option);;

let run_parser (ParserM fn) inp = fn inp;;

let bind m f =
  ParserM (fun (stream, warnings) ->
             let apply_next (new_stream, result) warnings =
               let new_parser = f result in
                 run_parser new_parser (new_stream, warnings)
             in
               match run_parser m (stream, warnings) with
                 | warnings, Some hd -> apply_next hd warnings
                 | warnings, None -> warnings, None);;

(* Syntactic sugar for bind. *)
let (>>=) = bind;;

(* Parse with `p`, ignore, then parse with `q`. *)
let (<+>) p q =
  p >>= fun _ -> q;;

(* Parse with either p and q. Only the first valid alternative is
   kept. We use </> instead of <|> to help the emacs mode
   formatter. *)
let (</>) p q = ParserM (fun inp ->
                           match run_parser p inp with
                             | (_, Some _) as p_results ->
                                 p_results
                             | (_, None) ->
                                 run_parser q inp);;

(* A parser that does not consume any input, but `return`s v. *)
let result v = ParserM (fun (stream, warnings) ->
                          warnings, Some (stream, v));;

(* A parser that returns the next position in the stream. *)
let get_next_pos = ParserM (fun (stream, warnings) ->
                              let Stream(last_token, tokens) = stream in
                                match tokens with
                                  | Token(_, Pos(start_pos, _)) :: _ ->
                                      warnings, Some (stream, start_pos)
                                  | [] ->
                                      (match last_token with
                                         | Some(Token(_, Pos(_, end_pos))) ->
                                             warnings, Some (stream, end_pos + 1)
                                         | None ->
                                             warnings, Some (stream, 0)));;

(* A parser that returns the previous position in the stream. *)
let get_previous_pos = ParserM (fun (stream, warnings) ->
                                  let Stream(last_token, tokens) = stream in
                                    match last_token with
                                      | Some(Token(_, Pos(_, end_pos))) ->
                                          warnings, Some (stream, end_pos)
                                      | None ->
                                          warnings, Some (stream, 0));;

(* Common code to add warnings. *)
let add_warning warning_type warning_message =
  let warning_impl pos =
    ParserM (fun (stream, warnings) ->
               let warning = Warning(warning_type, warning_message, pos) in
               let new_warnings = warning :: warnings in
                 new_warnings, Some (stream, ()))
  in
    get_next_pos >>= fun pos ->
      warning_impl pos;;

(* Add a warning, but don't terminate the parser. *)
let warning warning_message =
  add_warning Error warning_message;;

(* Add a 'not implemented' warning. *)
let warning_not_implemented warning_message =
  add_warning SkippedNotImplemented warning_message;;

(* Terminate this parser. *)
let fail =
  ParserM (fun (stream, warnings) -> warnings, None);;

(* Terminate the parser with a message. *)
let error message =
  warning message <+> fail ;;

(* Parse one item. *)
let item =
  ParserM (fun (Stream(_, tokens), warnings) ->
             match tokens with
               | hd :: tl -> warnings, Some (Stream(Some hd, tl), hd)
               | [] -> warnings, None);;

(* Lookahead one item. Like `item`, but doesn't consume the input. *)
let lookahead =
  ParserM (fun (stream, warnings) ->
             let Stream(_, tokens) = stream in
               match tokens with
                 | hd :: tl -> warnings, Some (stream, Some(hd))
                 | [] -> warnings, Some (stream, None));;

(* Consume `n` items, returns a list of them. Fails if there are not
   enough items in the steram. *)
let consume_many n =
  let rec cm_iter n acc =
    if n = 0 then result <| List.rev acc
    else (item >>= fun tok -> cm_iter (n - 1) (tok :: acc))
  in
    cm_iter n [];;

(* Lookahead many items. Like consume_many, but doesn't consume the input. *)
let lookahead_many n =
  let rec take n seq =
    if n = 0 || seq = []
    then []
    else (List.hd seq) :: (take (n - 1) (List.tl seq))
  in
    ParserM (fun (stream, warnings) ->
               let Stream(_, tokens) = stream in
                 if List.length tokens >= n
                 then warnings, Some (stream, Some(take n tokens))
                 else warnings, Some (stream, None));;

(* Parse using `p` until next token has value `next`. *)
let until nexts p =
  let rec until_impl acc =
    lookahead >>= function
      | Some(Token(token, _)) when (not <| List.mem token nexts) ->
          (p >>= fun res -> until_impl (res :: acc))
      | _ ->
          result (List.rev acc)
  in
    until_impl [];;

(* Will consume `str` like `consume`, or will add a warning and create
   a fake `token` with content `str`. *)
let consume_or_fake str =
  ParserM (fun (stream, warnings) ->
             let Stream(last_token, tokens) = stream in
               match tokens with
                 | Token(content, pos) as hd :: tl when content = str ->
                     warnings, Some (Stream(Some hd, tl), hd)
                 | _ ->
                     let str_len = String.length str in
                     let pos = match last_token with
                       | Some(Token(_, Pos(_, pos))) -> Pos(pos, pos + str_len - 1)
                       | None -> Pos(0, str_len - 1) in
                     let token = Token(str, pos) in
                     let (Pos(pos_start, _)) = pos in
                     let warning_message = sprintf "Expected '%s'." str in
                     let new_warning = Warning(Error, warning_message, pos_start) in
                       (new_warning :: warnings), Some (stream, token));;

(* Parser that returns `true` if there aren't any symbols left in the
   input, `false` otherwise. *)
let eoi  =
  lookahead >>= function
    | Some(Token _) -> result false
    | None -> result true;;

(* Applies parser `p` until end of input and collects results in a list. *)
let until_eoi p =
  let rec until_eoi_impl acc =
    eoi >>= fun finished ->
      if finished
      then result (List.rev acc)
      else p >>= fun result ->
        until_eoi_impl (result :: acc)
  in
    until_eoi_impl [];;

(* Parse one token with content `content`. *)
let consume content =
  lookahead >>= function
    | Some(Token(token_content, _)) when token_content = content ->
        item
    | Some(Token(token_content, _)) ->
        let error_message =
          sprintf "Expected '%s' but got '%s'." content token_content
        in
          error error_message
    | None ->
        let error_message = sprintf "Expected '%s' but reached end of input." content in
          error error_message;;

(* A parser that accumulates results of parsing elements with `p`, as
long as they are followed by `sep_token`. *)
let sep_by sep_token p =
  let rec sep_by_iter acc =
    p >>= fun elm ->
      lookahead >>= function
        | Some(Token(tok, _)) when tok = sep_token ->
            consume sep_token <+> sep_by_iter (elm :: acc)
        | _ ->
            result <| List.rev (elm :: acc)
  in
    sep_by_iter [];;
