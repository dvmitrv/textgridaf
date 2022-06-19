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

let test input ~ref ~format () =
  let input = fixture input |> Textgrid.parse in
  let ref = fixture ref in
  match input with
  | Ok input ->
      Alcotest.(check string) "equal" ref (Textgrid.serialize ~format input)
  | Error e -> Alcotest.fail e

let () =
  let open Alcotest in
  run "Serializing"
    [
      ( "formats",
        [
          test_case "ASCII-encoded, long format to long format" `Quick
            (test "0-full-ascii" ~ref:"0-full-ascii" ~format:`Full);
          test_case "ASCII-encoded, short format to full format" `Quick
            (test "0-short-ascii" ~ref:"0-full-ascii" ~format:`Full);
          test_case "ASCII-encoded, full format to short format" `Quick
            (test "0-full-ascii" ~ref:"0-short-ascii" ~format:`Short);
          test_case
            "ASCII-encoded, short format, with comments, to short format" `Quick
            (test "0-short-commented-ascii" ~ref:"0-short-ascii" ~format:`Short);
        ] );
    ]
