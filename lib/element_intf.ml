module type POSITION = sig
  type t

  val compare : t -> t -> int
  val equal : t -> t -> bool
  val to_string : t -> string
  val of_string : string -> t option
  val to_json : t -> Jsonaf.t
  val decoder : t Decoders_jsonaf.Decode.decoder
  val of_json : Jsonaf.t -> (t, string) Result.t
end

module type ELEMENT = sig
  type pos
  (** The type of a position in time. *)

  type t
  (** The type of an element. *)

  val xmin : t -> pos
  (** [xmin t] is the left bound of [t]. *)

  val xmax : t -> pos
  (** [xmax t] is the right bound of [t]. *)

  val text : t -> string
  (** [text t] is the payload of [t]. *)

  val equal : t -> t -> bool
  (** [equal x y] is [true] iff [x] is the same as [y]. *)

  val to_json : ?format:[ `A | `O ] -> t -> Jsonaf.t
  val of_json : format:[ `A | `O ] -> Jsonaf.t -> (t, string) Result.t
end

module type POINT = sig
  type pos

  include ELEMENT with type pos := pos

  val v : number:pos -> string -> t
  val number : t -> pos
end

module type INTERVAL = sig
  type pos

  include ELEMENT with type pos := pos

  val v : xmin:pos -> xmax:pos -> string -> t
end

module type Intf = sig
  module Make_point (P : POSITION) : POINT with type pos = P.t
  module Make_interval (P : POSITION) : INTERVAL with type pos = P.t
end
