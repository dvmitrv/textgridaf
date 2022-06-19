open Angstrom

module type S = sig
  type textgrid

  val parse : string -> (textgrid, string) result
end

module Make (TG : Textgrid.S) : S with type textgrid = TG.t = struct
  type textgrid = TG.t

  let comment =
    let is_newline c = Char.equal c '\n' || Char.equal c '\r' in
    let line_feed = char '\n' >>| Fun.const () in
    char '!' *> take_till is_newline *> option () line_feed

  let rec garbage p =
    let* _ = skip_while (fun c -> not (p c || Char.equal c '!')) in
    let* c = peek_char_fail in
    match c with '!' -> comment *> garbage p | _ -> return ()

  let text =
    let garbage = garbage (Char.equal '"') in
    let quoted_string = char '"' *> take_till (Char.equal '"') <* char '"' in
    let multi_string = String.concat "\"" <$> many1 quoted_string in
    garbage *> multi_string

  let is_digit = function '0' .. '9' -> true | _ -> false

  let number =
    let garbage = garbage is_digit in
    let number = take_while1 (fun c -> Char.equal c '.' || is_digit c) in
    garbage *> number

  let index =
    let garbage = garbage (Char.equal '[') in
    let sqrbrk p = char '[' *> p <* char ']' in
    let index = sqrbrk (take_while1 is_digit) in
    garbage *> index >>| int_of_string

  let index_opt =
    let* () = skip_while (function '0' .. '9' | '[' -> false | _ -> true) in
    let* c = peek_char_fail in
    match c with
    | '0' .. '9' -> return None
    | '[' -> index >>| Option.some
    | _ -> assert false

  let flag_value =
    let garbage = garbage (Char.equal '<') in
    let anglbrk = char '<' *> take_till (Char.equal '>') <* char '>' in
    garbage *> anglbrk >>= function
    | "exists" -> return true
    | "absent" -> return false
    | other -> fail (Format.sprintf "unknown flag <%s>" other)

  let to_pos s =
    match TG.Position.of_string s with
    | Some pos -> return pos
    | None -> fail "malformed position"

  let interval =
    let* _ = index_opt in
    let* xmin = number <?> "interval xmin" >>= to_pos in
    let* xmax = number <?> "interval xmax" >>= to_pos in
    let+ text = text <?> "interval text" in
    TG.Interval.v ~xmin ~xmax text

  let point =
    let* _ = index_opt in
    let* number = number <?> "point number" >>= to_pos in
    let+ mark = text <?> "point mark" in
    TG.Point.v ~number mark

  let interval_tier =
    let* name = text <?> "interval tier name" in
    let* xmin = number <?> "interval tier xmin" >>= to_pos in
    let* xmax = number <?> "interval tier xmax" >>= to_pos in
    let* size = number <?> "interval tier size" >>| int_of_string in
    let+ intervals = count size interval in
    TG.Tier.of_intervals ~name ~xmin ~xmax intervals

  let point_tier =
    let* name = text <?> "text tier name" in
    let* xmin = number <?> "text tier xmin" >>= to_pos in
    let* xmax = number <?> "text tier xmax" >>= to_pos in
    let* size = number <?> "text tier size" >>| int_of_string in
    let+ points = count size point in
    TG.Tier.of_points ~name ~xmin ~xmax points

  let tier =
    let* _ = option None (index >>| fun i -> Some i) in
    let* cls = text <?> "tier class" in
    match cls with
    | "IntervalTier" -> interval_tier <?> "interval tier"
    | "TextTier" -> point_tier <?> "text tier"
    | other -> fail ("unknown tier class " ^ other)

  let textgrid =
    let open Angstrom in
    let* xmin = number <?> "textgrid xmin" >>= to_pos in
    let* xmax = number <?> "textgrid xmax" >>= to_pos in
    let* flag = flag_value <?> "textgrid tier exists?" in
    if flag then
      let* size = number <?> "tiers size" >>| int_of_string in
      count size tier >>| fun tiers -> TG.v ~xmin ~xmax tiers
    else return (TG.v ~xmin ~xmax [])

  let to_utf8 ?encoding (src : string) =
    let rec loop d buf acc =
      match Uutf.decode d with
      | `Uchar u ->
          Uutf.Buffer.add_utf_8 buf u;
          loop d buf acc
      | `End -> Buffer.contents buf
      | `Malformed _ ->
          Uutf.Buffer.add_utf_8 buf Uutf.u_rep;
          loop d buf acc
      | `Await -> assert false
    in
    loop (Uutf.decoder ?encoding (`String src)) (Buffer.create 512) []

  let parse s =
    let as_utf8 = to_utf8 s in
    print_endline as_utf8;
    parse_string ~consume:Prefix textgrid as_utf8
end
