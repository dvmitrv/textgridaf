module Textgrid = Textgrid (* export *)
module Element = Element (* export *)

module Float_position : Element.POSITION with type t = float = struct
  type t = float

  let compare = Float.compare
  let equal = Float.equal

  let to_string t =
    let s = Float.to_string t in
    let trim_last_char s = String.sub s 0 (String.length s - 1) in
    if String.ends_with ~suffix:"." s then trim_last_char s else s

  let of_string = Float.of_string_opt
  let to_json t = `Number (to_string t)
  let decoder = Decoders_jsonaf.Decode.float

  let of_json value =
    Decoders_jsonaf.Decode.decode_value decoder value
    |> Result.map_error Decoders_jsonaf.Decode.string_of_error
end

module Integer_microseconds_position : Element.POSITION with type t = int =
struct
  type t = int

  let compare = Int.compare
  let equal = Int.equal

  let to_string t =
    let seconds t = Float.of_int t /. 1_000_000. in
    Float.to_string (seconds t)

  let of_string s =
    let to_microseconds x = Float.to_int (x *. 1_000_000.) in
    Option.map to_microseconds (float_of_string_opt s)

  let to_json t = `Number (Int.to_string t)
  let decoder = Decoders_jsonaf.Decode.int

  let of_json value =
    Decoders_jsonaf.Decode.decode_value decoder value
    |> Result.map_error Decoders_jsonaf.Decode.string_of_error
end

module Float_textgrid : sig
  include Textgrid.S with module Position = Float_position
  include Parser.S with type textgrid := t
  include Serializer.S with type textgrid := t
end = struct
  module TG = Textgrid.Make (Float_position)
  include TG
  include Parser.Make (TG)
  include Serializer.Make (TG)
end

module Integer_microseconds_textgrid : sig
  include Textgrid.S with module Position = Integer_microseconds_position
  include Parser.S with type textgrid := t
  include Serializer.S with type textgrid := t
end = struct
  module TG = Textgrid.Make (Integer_microseconds_position)
  include TG
  include Parser.Make (TG)
  include Serializer.Make (TG)
end
