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

  let to_json ?format { number; mark } =
    let number = P.to_string number in
    match format with
    | Some `Array -> `Array [ `Number number; `String mark ]
    | None | Some `Object ->
        `Object [ ("number", `Number number); ("mark", `String mark) ]

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
    let xmin = P.to_string xmin in
    let xmax = P.to_string xmax in
    match format with
    | Some `Array -> `Array [ `Number xmin; `Number xmax; `String text ]
    | None | Some `Object ->
        `Object
          [
            ("xmin", `Number xmin);
            ("xmax", `Number xmax);
            ("text", `String text);
          ]

  let to_csv { xmin; xmax; text } = [ P.to_string xmin; P.to_string xmax; text ]
end
