open Nonograms

(* test cases type *)
type test_case = {
  name : string;
  row_hints : hint list;
  col_hints : hint list;
  expected_solution : puzzle;
}

let test_cases : test_case list = [
  (*  
  {
    name = "1x1";
    row_hints = [[1]];
    col_hints = [[1]];
    expected_solution = [
      [Filled]
    ];
  };
  
  {
    name = "2x2";
    row_hints = [[2]; [2]];
    col_hints = [[2]; [2]];
    expected_solution = [
      [Filled; Filled];
      [Filled; Filled]
    ];
  };
  
  {
    name = "2x2 example2";
    row_hints = [[1]; [1]];
    col_hints = [[1]; [1]];
    expected_solution = [
      [Filled; Empty];
      [Empty; Filled]
    ];
  };
  
  {
    name = "3x3";
    row_hints = [[1]; [3]; [1]];
    col_hints = [[1]; [3]; [1]];
    expected_solution = [
      [Empty; Filled; Empty];
      [Filled; Filled; Filled];
      [Empty; Filled; Empty]
    ];
  };
  
  {
    name = "4x4 empty";
    row_hints = [[]; []; []; []];
    col_hints = [[]; []; []; []];
    expected_solution = [
      [Empty; Empty; Empty; Empty];
      [Empty; Empty; Empty; Empty];
      [Empty; Empty; Empty; Empty];
      [Empty; Empty; Empty; Empty]
    ];
  };
  *)
  {
    name = "5x5";
    row_hints = [[5]; [1;1;1]; [5]; [1;1;1]; [5]];
    col_hints = [[5]; [1;1;1]; [5]; [1;1;1]; [5]];
    expected_solution = [
      [Filled; Filled; Filled; Filled; Filled];
      [Filled; Empty; Filled; Empty; Filled];
      [Filled; Filled; Filled; Filled; Filled];
      [Filled; Empty; Filled; Empty; Filled];
      [Filled; Filled; Filled; Filled; Filled]
    ];
  };
  
  (*
  {
    name = "5x5 example 2";
    row_hints = [[1]; [1]; [5]; [1]; [1]];
    col_hints = [[1]; [5]; [1]; [1]; [1]];
    expected_solution = [
      [Empty; Filled; Empty; Empty; Empty];
      [Empty; Filled; Empty; Empty; Empty];
      [Filled; Filled; Filled; Filled; Filled];
      [Empty; Filled; Empty; Empty; Empty];
      [Empty; Filled; Empty; Empty; Empty]
    ];
  };
  

  {
    name = "4x4";
    row_hints = [[1;1]; [1;1]; [1;1]; [1;1]];
    col_hints = [[1;1]; [1;1]; [1;1]; [1;1]];
    expected_solution = [
      [Filled; Empty; Filled; Empty];
      [Empty; Filled; Empty; Filled];
      [Filled; Empty; Filled; Empty];
      [Empty; Filled; Empty; Filled]
    ];
  };
  

  {
    name = "5x5 example 3";
    row_hints = [[1;1]; [1;1]; [1]; [1;1]; [1;1]];
    col_hints = [[1;1]; [1;1]; [1]; [1;1]; [1;1]];
    expected_solution = [
      [Filled; Empty; Filled; Empty; Empty];
      [Empty; Filled; Empty; Filled; Empty];
      [Empty; Empty; Empty; Empty; Filled];
      [Filled; Empty; Empty; Filled; Empty];
      [Empty; Filled; Empty; Empty; Filled]
    ];
  };
  

  {
    name = "6x6";
    row_hints = [[2]; [1;1]; [3]; [1;2]; [2]; [1;1]];
    col_hints = [[1;1;1]; [6]; [1]; [1;1;1]; [1]; []];
    expected_solution = [
      [Filled; Filled; Empty; Empty; Empty; Empty];
      [Empty; Filled; Empty; Filled; Empty; Empty];
      [Filled; Filled; Filled; Empty; Empty; Empty];
      [Empty; Filled; Empty; Filled; Filled; Empty];
      [Filled; Filled; Empty; Empty; Empty; Empty];
      [Empty; Filled; Empty; Filled; Empty; Empty]
    ];
  };

  {
    name = "2x5 Example 1";
    row_hints = [[2]; [1;3]];
    col_hints = [[2]; [1]; [1]; [1]; [1]];
    expected_solution = [
      [Filled; Filled; Empty; Empty; Empty];
      [Filled; Empty; Filled; Filled; Filled];
    ];
  };

  {
    name = "2x5 Example 2";
    row_hints = [[3]; [1;3]];
    col_hints = [[2]; [1]; [2]; [1]; [1]];
    expected_solution = [
      [Filled; Filled; Filled; Empty; Empty];
      [Filled; Empty; Filled; Filled; Filled];
    ];
  };
  *)
  {
    name = "Large puzzle 15x30";
    row_hints = [[3]; [5]; [5]; [5; 1]; [5; 3]; [5; 3]; [5; 3]; [1; 5; 3]; [3; 9]; [3; 9]; [3; 8]; [3; 5]; [3; 5]; [3; 5]; [9]; [9]; [8]; [5]; [5]; [5]; [5]; [15]; [2; 2]; [15]; [13]; [11]; [11]; [9]; [9]; [7]];
    col_hints = [[3]; [8; 4]; [10; 1; 4]; [9; 1; 6]; [3; 1; 7]; [21; 7]; [22; 7]; [22; 7]; [22; 7]; [21; 7]; [3; 1; 7]; [7; 1; 6]; [8; 1; 4]; [6; 4]; [3]];
    expected_solution = [
      [Empty; Empty; Empty; Empty; Empty; Empty; Filled; Filled; Filled; Empty; Empty; Empty; Empty; Empty; Empty];
      [Empty; Empty; Empty; Empty; Empty; Filled; Filled; Filled; Filled; Filled; Empty; Empty; Empty; Empty; Empty];
      [Empty; Empty; Empty; Empty; Empty; Filled; Filled; Filled; Filled; Filled; Empty; Empty; Empty; Empty; Empty];
      [Empty; Empty; Empty; Empty; Empty; Filled; Filled; Filled; Filled; Filled; Empty; Empty; Filled; Empty; Empty];
      [Empty; Empty; Empty; Empty; Empty; Filled; Filled; Filled; Filled; Filled; Empty; Filled; Filled; Filled; Empty];
      [Empty; Empty; Empty; Empty; Empty; Filled; Filled; Filled; Filled; Filled; Empty; Filled; Filled; Filled; Empty];
      [Empty; Empty; Empty; Empty; Empty; Filled; Filled; Filled; Filled; Filled; Empty; Filled; Filled; Filled; Empty];
      [Empty; Empty; Filled; Empty; Empty; Filled; Filled; Filled; Filled; Filled; Empty; Filled; Filled; Filled; Empty];
      [Empty; Filled; Filled; Filled; Empty; Filled; Filled; Filled; Filled; Filled; Filled; Filled; Filled; Filled; Empty];
      [Empty; Filled; Filled; Filled; Empty; Filled; Filled; Filled; Filled; Filled; Filled; Filled; Filled; Filled; Empty];
      [Empty; Filled; Filled; Filled; Empty; Filled; Filled; Filled; Filled; Filled; Filled; Filled; Filled; Empty; Empty];
      [Empty; Filled; Filled; Filled; Empty; Filled; Filled; Filled; Filled; Filled; Empty; Empty; Empty; Empty; Empty];
      [Empty; Filled; Filled; Filled; Empty; Filled; Filled; Filled; Filled; Filled; Empty; Empty; Empty; Empty; Empty];
      [Empty; Filled; Filled; Filled; Empty; Filled; Filled; Filled; Filled; Filled; Empty; Empty; Empty; Empty; Empty];
      [Empty; Filled; Filled; Filled; Filled; Filled; Filled; Filled; Filled; Filled; Empty; Empty; Empty; Empty; Empty];
      [Empty; Filled; Filled; Filled; Filled; Filled; Filled; Filled; Filled; Filled; Empty; Empty; Empty; Empty; Empty];
      [Empty; Empty; Filled; Filled; Filled; Filled; Filled; Filled; Filled; Filled; Empty; Empty; Empty; Empty; Empty];
      [Empty; Empty; Empty; Empty; Empty; Filled; Filled; Filled; Filled; Filled; Empty; Empty; Empty; Empty; Empty];
      [Empty; Empty; Empty; Empty; Empty; Filled; Filled; Filled; Filled; Filled; Empty; Empty; Empty; Empty; Empty];
      [Empty; Empty; Empty; Empty; Empty; Filled; Filled; Filled; Filled; Filled; Empty; Empty; Empty; Empty; Empty];
      [Empty; Empty; Empty; Empty; Empty; Filled; Filled; Filled; Filled; Filled; Empty; Empty; Empty; Empty; Empty];
      [Filled; Filled; Filled; Filled; Filled; Filled; Filled; Filled; Filled; Filled; Filled; Filled; Filled; Filled; Filled];
      [Filled; Filled; Empty; Empty; Empty; Empty; Empty; Empty; Empty; Empty; Empty; Empty; Empty; Filled; Filled];
      [Filled; Filled; Filled; Filled; Filled; Filled; Filled; Filled; Filled; Filled; Filled; Filled; Filled; Filled; Filled];
      [Empty; Filled; Filled; Filled; Filled; Filled; Filled; Filled; Filled; Filled; Filled; Filled; Filled; Filled; Empty];
      [Empty; Empty; Filled; Filled; Filled; Filled; Filled; Filled; Filled; Filled; Filled; Filled; Filled; Empty; Empty];
      [Empty; Empty; Filled; Filled; Filled; Filled; Filled; Filled; Filled; Filled; Filled; Filled; Filled; Empty; Empty];
      [Empty; Empty; Empty; Filled; Filled; Filled; Filled; Filled; Filled; Filled; Filled; Filled; Empty; Empty; Empty];
      [Empty; Empty; Empty; Filled; Filled; Filled; Filled; Filled; Filled; Filled; Filled; Filled; Empty; Empty; Empty];
      [Empty; Empty; Empty; Empty; Filled; Filled; Filled; Filled; Filled; Filled; Filled; Empty; Empty; Empty; Empty];
    ];
  };
]

(* run all tests *)
let run_tests () =
  List.iter (fun test ->
    print_endline ("Running Test: " ^ test.name);
    match solve_nonogram test.row_hints test.col_hints with
    | None ->
        print_endline "Solver failed to find a solution.\n";
    | Some solution ->
        if puzzles_equal solution test.expected_solution then begin
          print_endline "Test Passed.\n";
          print_puzzle solution
        end
        else begin
          print_endline "Test Failed.";
          print_endline "Expected Solution:";
          print_puzzle test.expected_solution;
          print_endline "Solver Output:";
          print_puzzle solution;
        end
  ) test_cases

(* start tests *)
let () =
  print_endline "Starting Nonogram Solver Tests...\n";
  run_tests ();
  print_endline "All tests done."