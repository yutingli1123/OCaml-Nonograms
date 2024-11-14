(* tests.ml *)
open Nonogram (* Import your main module *)

(* Helper functions for testing *)

let assert_equal expected actual =
  if expected = actual then
    print_endline "Test passed"
  else
    print_endline ("Test failed. Expected: " ^ expected ^ " but got: " ^ actual)

let assert_puzzle_equal expected actual =
  if expected = actual then
    print_endline "Puzzle Test passed"
  else
    print_endline "Puzzle Test failed"

(* Test cases for get_column *)
let test_get_column () =
  let puzzle = [
    [Filled; Empty; Unknown];
    [Empty; Filled; Filled];
    [Unknown; Empty; Filled]
  ] in
  let expected = [Empty; Filled; Empty] in
  let actual = get_column puzzle 1 in
  assert_equal expected actual

(* Test cases for initialize_puzzle *)
let test_initialize_puzzle () =
  let rows, cols = 2, 3 in
  let expected = [
    [Unknown; Unknown; Unknown];
    [Unknown; Unknown; Unknown]
  ] in
  let actual = initialize_puzzle rows cols in
  assert_puzzle_equal expected actual

(* Test cases for validate_row *)
let test_validate_row () =
  assert_equal true (validate_row [3] [Filled; Filled; Filled; Empty; Empty]);
  assert_equal false (validate_row [3] [Filled; Empty; Filled; Empty; Empty])

(* Test cases for generate_row_configurations *)
let test_generate_row_configurations () =
  let hint = [2] in
  let length = 3 in
  let expected = [
    [Filled; Filled; Empty];
    [Empty; Filled; Filled]
  ] in
  let actual = generate_row_configurations hint length [] in
  assert_equal expected actual

(* Test cases for update_row_with_guaranteed_cells *)
let test_update_row_with_guaranteed_cells () =
  let row = [Unknown; Filled; Unknown] in
  let hint = [1; 1] in
  let expected = [Empty; Filled; Filled] in
  let actual = update_row_with_guaranteed_cells hint row in
  assert_equal expected actual

(* Test cases for is_filled_same *)
let test_is_filled_same () =
  let ref_row = [Filled; Empty; Unknown] in
  let cmp_row = [Filled; Empty; Unknown] in
  assert_equal true (is_filled_same ref_row cmp_row);
  assert_equal false (is_filled_same [Filled; Filled; Unknown] cmp_row)

(* Run all tests *)
let () =
  print_endline "Running tests...";
  test_get_column ();
  test_initialize_puzzle ();
  test_validate_row ();
  test_generate_row_configurations ();
  test_update_row_with_guaranteed_cells ();
  test_is_filled_same ();
  print_endline "All tests complete."