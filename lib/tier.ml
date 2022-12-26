let map ~f =
  let rec loop acc = function
    | [] -> List.rev acc
    | x :: xs -> loop (f x :: acc) xs
  in
  loop []

module type S = sig
  type pos

  module Point : Element.POINT with type pos = pos
  module Interval : Element.INTERVAL with type pos = pos

  type point = Point.t
  type interval = Interval.t
  type data = [ `Points of point list | `Intervals of interval list ]
  type t

  val name : t -> string
  val xmin : t -> pos
  val xmax : t -> pos
  val data : t -> data
  val count : t -> int
  val of_points : name:string -> xmin:pos -> xmax:pos -> point list -> t
  val of_intervals : name:string -> xmin:pos -> xmax:pos -> interval list -> t
  val to_json : ?format:[< `A | `O ] -> t -> Jsonaf.t
  val of_json : format:[< `A | `O ] -> Jsonaf.t -> (t, string) result
  val to_csv : t -> string list list
end

module Make (P : Element.POSITION) : S with type pos = P.t = struct
  type pos = P.t

  module Point = Element.Make_point (P)
  module Interval = Element.Make_interval (P)

  type point = Point.t
  type interval = Interval.t
  type data = [ `Points of point list | `Intervals of interval list ]
  type t = { name : string; xmin : pos; xmax : pos; data : data }

  let name t = t.name
  let xmin t = t.xmin
  let xmax t = t.xmax
  let data t = t.data

  let count t =
    match t.data with
    | `Points ps -> List.length ps
    | `Intervals is -> List.length is

  let of_points ~name ~xmin ~xmax elts =
    { name; xmin; xmax; data = `Points elts }

  let of_intervals ~name ~xmin ~xmax elts =
    { name; xmin; xmax; data = `Intervals elts }

  let to_json ?format t : Jsonaf.t =
    let open Decoders_jsonaf.Encode in
    match t with
    | { name; xmin; xmax; data = `Points points } ->
        obj
          [
            ("name", string name);
            ("xmin", number (P.to_string xmin));
            ("xmax", number (P.to_string xmax));
            ("points", list Fun.id (map points ~f:(Point.to_json ?format)));
          ]
    | { name; xmin; xmax; data = `Intervals intervals } ->
        obj
          [
            ("name", string name);
            ("xmin", number (P.to_string xmin));
            ("xmax", number (P.to_string xmax));
            ("intervals", list Fun.id (map intervals ~f:(Interval.to_json ?format)));
          ]

  let decoder ~format =
    let open Decoders_jsonaf.Decode in
    let* name = field "name" string in
    let* xmin = field "xmin" P.decoder in
    let* xmax = field "xmax" P.decoder in
    let points = list (Point.decoder ~format) in
    let intervals = list (Interval.decoder ~format) in
    let+ data =
      one_of [
        "points", field "points" (map (fun ps -> `Points ps) points);
        "intervals", field "intervals" (map (fun is -> `Intervals is) intervals);
      ]
    in
    { name; xmin; xmax; data }

  let of_json ~format value =
    Decoders_jsonaf.Decode.decode_value (decoder ~format) value
    |> Result.map_error Decoders_jsonaf.Decode.string_of_error

  let to_csv = function
    | { data = `Points points; _ } ->
        [ "number"; "mark" ] :: map points ~f:Point.to_csv
    | { data = `Intervals intervals; _ } ->
        [ "xmin"; "xmax"; "text" ] :: map intervals ~f:Interval.to_csv
end
