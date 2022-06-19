open Faraday

type format = [ `Full | `Short ]

let escape_quotes = Str.global_replace (Str.regexp "\"") "\"\""

module type S = sig
  type pos
  type tier
  type textgrid
  type format = [ `Full | `Short ]

  val serialize : ?format:format -> textgrid -> string
end

module Make (TG : Textgrid.S) : S with type textgrid = TG.t = struct
  type pos = TG.pos
  type tier = TG.tier
  type textgrid = TG.t
  type format = [ `Full | `Short ]

  let string_of_pos = TG.Position.to_string

  module Short_format = struct
    let interval s i =
      let xmin, xmax, text = TG.Interval.(xmin i, xmax i, text i) in
      write_string s (string_of_pos xmin);
      write_char s ' ';
      write_string s (string_of_pos xmax);
      write_char s ' ';
      write_string s ("\"" ^ escape_quotes text ^ "\"\n")

    let point s p =
      let number, mark = TG.Point.(number p, text p) in
      write_string s (string_of_pos number);
      write_char s ' ';
      write_string s ("\"" ^ escape_quotes mark ^ "\"\n")

    let tier s t =
      let name, xmin, xmax, data = TG.Tier.(name t, xmin t, xmax t, data t) in
      let xmin, xmax = (string_of_pos xmin, string_of_pos xmax) in
      match data with
      | `Intervals is ->
          write_string s ("\"IntervalTier\" \"" ^ name ^ "\" ");
          write_string s (xmin ^ " " ^ xmax ^ " ");
          write_string s (Int.to_string (List.length is) ^ "\n");
          List.iter (interval s) is
      | `Points ps ->
          write_string s ("\"TextTier\" \"" ^ name ^ "\" ");
          write_string s (xmin ^ " " ^ xmax ^ " ");
          write_string s (Int.to_string (List.length ps) ^ "\n");
          List.iter (point s) ps

    let textgrid s ~xmin ~xmax tiers =
      let xmin, xmax = (string_of_pos xmin, string_of_pos xmax) in
      write_string s "\"ooTextFile\" \"TextGrid\"\n";
      write_string s (xmin ^ " " ^ xmax ^ " ");
      write_string s "<exists> ";
      write_string s (Int.to_string (List.length tiers) ^ "\n");
      List.iter (tier s) tiers
  end

  module Full_format = struct
    let rec indent ?(times = 1) s =
      if times > 0 then (
        write_string s "    ";
        indent ~times:(times - 1) s)

    let header s ?(level = 0) name idx =
      indent ~times:level s;
      write_string s (Format.sprintf "%s [%d]:\n" name idx)

    let prop s ?(level = 0) ?(quoted = false) key data =
      let data = if quoted then "\"" ^ escape_quotes data ^ "\"" else data in
      indent ~times:level s;
      write_string s (Format.sprintf "%s = %s\n" key data)

    let interval s idx i =
      let xmin, xmax, text = TG.Interval.(xmin i, xmax i, text i) in
      header s ~level:2 "intervals" (idx + 1);
      prop s ~level:3 "xmin" (string_of_pos xmin);
      prop s ~level:3 "xmax" (string_of_pos xmax);
      prop s ~level:3 ~quoted:true "text" text

    let point s idx p =
      let number, mark = TG.Point.(number p, text p) in
      header s ~level:2 "points" (idx + 1);
      prop s ~level:3 "number" (string_of_pos number);
      prop s ~level:3 ~quoted:true "mark" mark

    let tier s idx t =
      let name, xmin, xmax, data = TG.Tier.(name t, xmin t, xmax t, data t) in
      let xmin, xmax = (string_of_pos xmin, string_of_pos xmax) in
      match data with
      | `Intervals is ->
          header s ~level:1 "item" (idx + 1);
          prop s ~level:2 ~quoted:true "class" "IntervalTier";
          prop s ~level:2 ~quoted:true "name" name;
          prop s ~level:2 "xmin" xmin;
          prop s ~level:2 "xmax" xmax;
          prop s ~level:2 "intervals: size" (TG.Tier.count t |> string_of_int);
          List.iteri (interval s) is
      | `Points ps ->
          header s ~level:1 "item" (idx + 1);
          prop s ~level:2 ~quoted:true "class" "TextTier";
          prop s ~level:2 ~quoted:true "name" name;
          prop s ~level:2 "xmin" xmin;
          prop s ~level:2 "xmax" xmax;
          prop s ~level:2 "points: size" (TG.Tier.count t |> string_of_int);
          List.iteri (point s) ps

    let textgrid s ~xmin ~xmax tiers =
      let serialize_tiers tiers =
        match tiers with
        | [] -> write_string s "tiers? <absent>\n"
        | tiers ->
            write_string s "tiers? <exists>\n";
            prop s "size" (string_of_int (List.length tiers));
            write_string s "item []:\n";
            List.iteri (tier s) tiers
      in
      prop s ~quoted:true "File type" "ooTextFile";
      prop s ~quoted:true "Object class" "TextGrid";
      write_char s '\n';
      prop s "xmin" (string_of_pos xmin);
      prop s "xmax" (string_of_pos xmax);
      serialize_tiers tiers
  end

  let serialize ?format t =
    let xmin, xmax, tiers = TG.(xmin t, xmax t, tiers t) in
    let s = create 1024 in
    (match format with
    | None | Some `Short -> Short_format.textgrid s ~xmin ~xmax tiers
    | Some `Full -> Full_format.textgrid s ~xmin ~xmax tiers);
    serialize_to_string s
end
