include Element_intf

module Make_point (P : POSITION) = struct
  type pos = P.t
  type t = { number : pos; mark : string }

  let v ~number mark = { number; mark }
  let xmin p = p.number
  let xmax p = p.number
  let number p = p.number
  let text p = p.mark
  let equal x y = P.equal x.number y.number && String.equal x.mark y.mark

  let to_json ?format { number = pos; mark } =
    let open Decoders_jsonaf.Encode in
    let number = number (P.to_string pos) in
    match format with
    | Some `A -> list Fun.id [ number; string mark ]
    | None | Some `O -> obj [ ("number", number); ("mark", string mark) ]

  let decoder ~format =
    let open Decoders_jsonaf.Decode in
    let array_decoder =
      let* pos = index 0 P.decoder in
      let+ mark = index 1 string in
      { number = pos; mark }
    in
    let object_decoder =
      let* pos = field "number" P.decoder in
      let+ mark = field "mark" string in
      { number = pos; mark }
    in
    match format with `A -> array_decoder | `O -> object_decoder

  let of_json ~format value =
    Decoders_jsonaf.Decode.decode_value (decoder ~format) value
    |> Result.map_error Decoders_jsonaf.Decode.string_of_error

  let to_csv { number; mark } = [ P.to_string number; mark ]
end

module Make_interval (P : POSITION) = struct
  type pos = P.t
  type t = { xmin : pos; xmax : pos; text : string }

  let v ~xmin ~xmax text = { xmin; xmax; text }
  let xmin i = i.xmin
  let xmax i = i.xmax
  let text i = i.text

  let equal x y =
    P.equal x.xmin y.xmin && P.equal x.xmax y.xmax && String.equal x.text y.text

  let to_json ?format { xmin; xmax; text } =
    let open Decoders_jsonaf.Encode in
    let xmin = P.to_json xmin in
    let xmax = P.to_json xmax in
    match format with
    | Some `A -> list Fun.id [ xmin; xmax; string text ]
    | None | Some `O ->
        obj [ ("xmin", xmin); ("xmax", xmax); ("text", string text) ]

  let decoder ~format =
    let open Decoders_jsonaf.Decode in
    let array_decoder =
      let* xmin = index 0 P.decoder in
      let* xmax = index 1 P.decoder in
      let+ text = index 2 string in
      { xmin; xmax; text }
    in
    let object_decoder =
      let* xmin = field "xmin" P.decoder in
      let* xmax = field "xmax" P.decoder in
      let+ text = field "text" string in
      { xmin; xmax; text }
    in
    match format with `A -> array_decoder | `O -> object_decoder

  let of_json ~format value =
    Decoders_jsonaf.Decode.decode_value (decoder ~format) value
    |> Result.map_error Decoders_jsonaf.Decode.string_of_error

  let to_csv { xmin; xmax; text } = [ P.to_string xmin; P.to_string xmax; text ]
end
