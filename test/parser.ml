module Textgrid = Textgridaf.Float_textgrid

let fixture name =
  let path = "./fixtures/" ^ name in
  let rec loop ic acc =
    try
      let line = input_line ic in
      loop ic (line :: acc)
    with End_of_file -> List.rev acc
  in
  let ic = open_in path in
  let lines = loop ic [] in
  close_in ic;
  String.concat "\n" lines ^ "\n"

let json = Alcotest.testable Jsonaf.pp Jsonaf.exactly_equal

let test input ~ref () =
  let input = fixture input |> Textgrid.parse in
  let ref = fixture ref |> Jsonaf.of_string in
  match input with
  | Ok input ->
      Alcotest.(check json) "same json value" ref (Textgrid.to_json input)
  | Error e -> Alcotest.fail e

let () =
  let open Alcotest in
  run "Parsing"
    [
      ( "formats",
        [
          test_case "ASCII-encoded, long format" `Quick
            (test "0-full-ascii" ~ref:"0-reference.json");
          test_case "ASCII-encoded, short format" `Quick
            (test "0-short-ascii" ~ref:"0-reference.json");
          test_case "ASCII-encoded, short format, with comments" `Quick
            (test "0-short-commented-ascii" ~ref:"0-reference.json");
        ] );
      ( "encodings",
        [
          test_case "UTF-8" `Quick (test "1-full-utf8" ~ref:"1-reference.json");
          test_case "UTF-16BE" `Quick
            (test "1-full-utf16be" ~ref:"1-reference.json");
          test_case "UTF-16LE" `Quick
            (test "1-full-utf16le" ~ref:"1-reference.json");
        ] );
    ]
