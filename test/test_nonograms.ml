open Nonograms (* Import your main module *)
open OUnit2

(* Helper functions for testing *)

(*--------------------------------------------------------------------------------------------*)
(* testing *)

(* check row equality *)
let assert_row_equal expected actual =
  let expected_str = cell_list_to_string expected in
  let actual_str = cell_list_to_string actual in
  assert_equal expected_str actual_str

(* check puzzle equality *)
let assert_puzzle_equal expected actual =
  let expected_str = puzzle_to_string expected in
  let actual_str = puzzle_to_string actual in
  assert_equal expected_str actual_str

(* test get_line *)
let test_get_line _ =
  let puzzle = [
    [Filled; Empty; Unknown];
    [Empty; Filled; Filled];
    [Unknown; Empty; Filled]
  ] in
  let expected_row = [Empty; Filled; Filled] in
  let actual_row = get_line puzzle Row 1 in
  assert_row_equal expected_row actual_row;

  let expected_column = [Filled; Empty; Unknown] in
  let actual_column = get_line puzzle Column 0 in
  assert_row_equal expected_column actual_column

(* test initialize_puzzle *)
let test_initialize_puzzle _ =
  let rows, cols = 2, 3 in
  let expected = [
    [Unknown; Unknown; Unknown];
    [Unknown; Unknown; Unknown]
  ] in
  let actual = initialize_puzzle rows cols in
  assert_puzzle_equal expected actual

(* test validate_line *)
let test_validate_line _ =
  assert_equal true (validate_line [3] [Filled; Filled; Filled; Empty; Empty]);
  assert_equal true (validate_line [3] [Filled; Empty; Filled; Empty; Empty]);
  assert_equal true (validate_line [1; 1] [Filled; Empty; Filled; Empty; Empty])

(* more validate_line tests *)
let test_validate_line_additional _ =
  
  (* valid *)
  assert_equal true (validate_line [3] [Filled; Filled; Filled; Empty; Empty]);
  assert_equal true (validate_line [3] [Empty; Filled; Filled; Filled; Empty]);
  assert_equal true (validate_line [3] [Empty; Empty; Filled; Filled; Filled]);
  assert_equal true (validate_line [1; 1] [Filled; Empty; Filled; Empty; Empty]);
  assert_equal true (validate_line [1; 1] [Empty; Filled; Empty; Filled; Empty]);
  assert_equal true (validate_line [1; 1] [Empty; Filled; Empty; Filled; Empty]);
  
  (* invalid *)
  assert_equal false (validate_line [1; 1] [Filled; Filled; Filled; Empty; Empty]);
  assert_equal false (validate_line [2] [Filled; Filled; Filled; Empty; Empty]);
  assert_equal false (validate_line [2; 1] [Filled; Filled; Empty; Filled; Filled]);
  assert_equal false (validate_line [5] [Filled; Filled; Filled; Filled]);
  assert_equal false (validate_line [2; 2] [Filled; Filled; Empty; Filled; Empty]);
  
  (* edge cases *)
  assert_equal true (validate_line [] [Empty; Empty; Empty]);
  assert_equal false (validate_line [] [Empty; Filled; Empty]);
  
  print_endline "Additional validate_line tests complete."

(* test generate_line_configurations *)
let test_generate_line_configurations _ =
  print_endline "Running test_generate_line_configurations...";
  let hint = [2] in
  let length = 3 in
  let expected = [
    [Filled; Filled; Empty];
    [Empty; Filled; Filled]
  ] in
  let actual = generate_line_configurations hint length in
  assert_puzzle_equal expected (List.map (fun row -> row) actual);
  
  let hint = [1; 1] in
  let length = 3 in
  let expected = [
    [Filled; Empty; Filled]
  ] in
  let actual = generate_line_configurations hint length in
  assert_puzzle_equal expected (List.map (fun row -> row) actual)

(* test update_line_with_guaranteed_cells *)
let test_update_line_with_guaranteed_cells _ =
  print_endline "Running test_update_line_with_guaranteed_cells...";
  let line = [Empty; Filled; Unknown] in
  let hint = [2] in
  let expected = [Empty; Filled; Filled] in
  let configs = get_line_configurations hint 3 in
  let actual = update_line_with_guaranteed_cells hint line configs in
  assert_row_equal expected actual;
  
  let line = [Unknown; Unknown; Unknown] in
  let hint = [3] in
  let expected = [Filled; Filled; Filled] in
  let configs = get_line_configurations hint 3 in
  let actual = update_line_with_guaranteed_cells hint line configs in
  assert_row_equal expected actual

(* test is_same *)
let test_is_same _ =
  let ref_line = [Filled; Empty; Unknown] in
  let cmp_line = [Filled; Empty; Unknown] in
  assert_equal true (is_same ref_line cmp_line);
  assert_equal false (is_same [Filled; Filled; Unknown] cmp_line);
  
  (* empty lists *)
  assert_equal true (is_same [] [])

(* test validate_column *)
let test_validate_column _ =
  assert_equal true (validate_line [3] [Filled; Filled; Filled; Empty; Empty]);
  assert_equal false (validate_line [3] [Filled; Empty; Filled; Empty; Empty])

(* test update_column_with_guaranteed_cells *)
let test_update_column_with_guaranteed_cells _ =
  print_endline "Running test_update_column_with_guaranteed_cells...";
  let column = [Unknown; Filled; Empty] in
  let hint = [2] in
  let expected = [Filled; Filled; Empty] in
  let configs = get_line_configurations hint 3 in
  let actual = update_line_with_guaranteed_cells hint column configs in
  assert_row_equal expected actual;
  
  let column = [Unknown; Unknown; Unknown] in
  let hint = [3] in
  let expected = [Filled; Filled; Filled] in
  let configs = get_line_configurations hint 3 in
  let actual = update_line_with_guaranteed_cells hint column configs in
  assert_row_equal expected actual

let suite = 
  "TestList" >::: [
    "test_get_line" >:: test_get_line;
    "test_initialize_puzzle" >:: test_initialize_puzzle;
    "test_validate_line" >:: test_validate_line;
    "test_validate_line_additional" >:: test_validate_line_additional;
    "test_generate_line_configurations" >:: test_generate_line_configurations;
    "test_update_line_with_guaranteed_cells" >:: test_update_line_with_guaranteed_cells;
    "test_is_same" >:: test_is_same;
    "test_validate_column" >:: test_validate_column;
    "test_update_column_with_guaranteed_cells" >:: test_update_column_with_guaranteed_cells
  ]

let () = 
  run_test_tt_main suite