open Lib.Editor.SplitableString
open Alcotest


let test_insert_at f chr pos input expected =
  fun (_ : unit) ->
  (check string) "same string" expected (f input chr pos)

let test_delete_at f pos input expected =
  fun (_ : unit) ->
  (check string) "same string" expected (f input pos)

(* let%test _ = insert_at "foo" 'x' 3 = "foox" *)
(* let%test _ = insert_at "foo" 'x' 2 = "foxo" *)
(* let%test _ = insert_at "foo" 'x' 1 = "fxoo" *)
(* let%test _ = insert_at "foo" 'x' 0 = "xfoo" *)
(* let%test _ = insert_at "foo" 'x' (-1) = "xfoo" *)

(* let%test _ = replace_at "foo" 'x' 3 = "fox" *)
(* let%test _ = replace_at "foo" 'x' 2 = "fox" *)
(* let%test _ = replace_at "foo" 'x' 1 = "fxo" *)
(* let%test _ = replace_at "foo" 'x' 0 = "xoo" *)
(* let%test _ = replace_at "foo" 'x' (-1) = "xfoo" *)

(* let%test _ = delete_at "abc" 3 = "abc" *)
(* let%test _ = delete_at "abc" 2 = "ab" *)
(* let%test _ = delete_at "abc" 1 = "ac" *)
(* let%test _ = delete_at "abc" 0 = "bc" *)
(* let%test _ = delete_at "abc" (-1) = "abc" *)

let () =
  let open Alcotest in
  run "Lib.Editor" [
    "SplitableString", [
      test_case "insert_at" `Quick (test_insert_at insert_at 'x' 4 "foo" "foo");
      test_case "insert_at" `Quick (test_insert_at insert_at 'x' 3 "foo" "foox");
      test_case "insert_at" `Quick (test_insert_at insert_at 'x' 2 "foo" "foxo");
      test_case "insert_at" `Quick (test_insert_at insert_at 'x' 1 "foo" "fxoo");
      test_case "insert_at" `Quick (test_insert_at insert_at 'x' 0 "foo" "xfoo");
      test_case "insert_at" `Quick (test_insert_at insert_at 'x' (-1) "foo" "foo");

      test_case "replace_at 3" `Quick (test_insert_at replace_at 'x' 3 "foo" "foo");
      test_case "replace_at 2" `Quick (test_insert_at replace_at 'x' 2 "foo" "fox");
      test_case "replace_at 1" `Quick (test_insert_at replace_at 'x' 1 "foo" "fxo");
      test_case "replace_at 0" `Quick (test_insert_at replace_at 'x' 0 "foo" "xoo");
      test_case "replace_at -1" `Quick (test_insert_at replace_at 'x' (-1) "foo" "foo");

      test_case "delete_at" `Quick (test_delete_at delete_at 3 "foo" "foo");
      test_case "delete_at" `Quick (test_delete_at delete_at 2 "foo" "fo");
      test_case "delete_at" `Quick (test_delete_at delete_at 1 "fao" "fo");
      test_case "delete_at" `Quick (test_delete_at delete_at 0 "foo" "oo");
      test_case "delete_at" `Quick (test_delete_at delete_at (-1) "foo" "foo");
    ]
  ]
