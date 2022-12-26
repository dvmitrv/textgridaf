module type S = sig
  module Position : Element.POSITION

  type pos = Position.t

  module Tier : Tier.S with type pos = pos

  type tier = Tier.t

  module Point = Tier.Point
  module Interval = Tier.Interval

  type point = Point.t
  type interval = Interval.t
  type t

  val v : xmin:pos -> xmax:pos -> tier list -> t
  val xmin : t -> pos
  val xmax : t -> pos
  val tiers : t -> tier list
  val to_json : ?format:[< `A | `O ] -> t -> Jsonaf.t
end

module Make (P : Element.POSITION) : S with module Position = P = struct
  module Position = P
  module Tier = Tier.Make (P)
  module Point = Tier.Point
  module Interval = Tier.Interval

  type pos = Position.t
  type tier = Tier.t
  type point = Point.t
  type interval = Interval.t
  type t = { xmin : pos; xmax : pos; tiers : Tier.t list }

  let v ~xmin ~xmax tiers = { xmin; xmax; tiers }
  let xmin t = t.xmin
  let xmax t = t.xmax
  let tiers t = t.tiers

  let to_json ?format t : Jsonaf.t =
    `Object
      [
        ("xmin", `Number (P.to_string t.xmin));
        ("xmax", `Number (P.to_string t.xmax));
        ("tiers", `Array (List.map (Tier.to_json ?format) t.tiers));
      ]
end
