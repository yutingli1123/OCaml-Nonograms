(* types *)
exception Error of string

type cell = Filled | Empty | Unknown
type hint = int list
type puzzle = cell list list

type line_type = Row | Column

(* check if first n cells are Filled *)
let rec all_filled n lst =
  if n <= 0 then true
  else match lst with
       | [] -> false
       | Filled :: ys -> all_filled (n - 1) ys
       | _ :: _ -> false

(* drop first n cells *)
let rec drop n lst =
  if n <= 0 then lst
  else match lst with
       | [] -> []
       | _ :: ys -> drop (n - 1) ys

(* get a specific line *)
let get_line puzzle ltype index =
  if index < 0 then raise (Error "Index cannot be negative") else
    match ltype with
    | Row -> 
        (try List.nth puzzle index
         with Failure _ -> raise (Error "Row index out of bounds"))
    | Column ->
        List.map (fun row -> 
          try List.nth row index
          with Failure _ -> raise (Error "Column index out of bounds")
        ) puzzle

(* update a specific line *)
let update_line puzzle ltype index new_line =
  match ltype with
  | Row ->
      List.mapi (fun row_idx row ->
        if row_idx = index then new_line else row
      ) puzzle
  | Column ->
      List.mapi (fun row_idx row ->
        List.mapi (fun col_idx cell ->
          if col_idx = index then List.nth new_line row_idx else cell
        ) row
      ) puzzle

(* sum of list *)
let get_list_sum lst =
  List.fold_left (+) 0 lst

(* remove first n elements *)
let rec remove_from_list lst amt =
  if amt <= 0 then lst
  else match lst with
       | [] -> []
       | _ :: xs -> remove_from_list xs (amt - 1)

(* init puzzle with Unknown *)
let initialize_puzzle rows cols =
  List.init rows (fun _ -> List.init cols (fun _ -> Unknown))

(* generate line configs *)
let generate_line_configurations hint length =
  let rec place_blocks hints remaining_length acc =
    match hints with
    | [] ->
        if remaining_length >= 0 then
          [acc @ (List.init remaining_length (fun _ -> Empty))]
        else
          []
    | h :: t ->
        let min_length = h + (if t = [] then 0 else 1) in
        let rec try_positions start acc_configs =
          if start + min_length > remaining_length then
            acc_configs
          else
            let new_acc = acc @ (List.init start (fun _ -> Empty)) @ (List.init h (fun _ -> Filled)) in
            let new_acc =
              if t <> [] then new_acc @ [Empty] else new_acc
            in
            let configs = place_blocks t (remaining_length - start - h - (if t <> [] then 1 else 0)) new_acc in
            try_positions (start + 1) (acc_configs @ configs)
        in
        try_positions 0 []
  in
  place_blocks hint length []

(* cache for line configs *)
let line_config_cache = Hashtbl.create 1000

(* get line configs with cache *)
let get_line_configurations hint length =
  try Hashtbl.find line_config_cache (hint, length)
  with Not_found ->
    let configs = generate_line_configurations hint length in
    Hashtbl.add line_config_cache (hint, length) configs;
    configs

(* check if two lines match *)
let is_same ref_ cmp =
  try
    List.for_all2 (fun a b ->
      match a with
      | Filled -> b = Filled
      | Empty -> b = Empty
      | Unknown -> true
    ) ref_ cmp
  with Invalid_argument _ ->
    false

(* validate a line *)
let validate_line hint line =
  let configs = get_line_configurations hint (List.length line) in
  List.exists (fun config -> is_same line config) configs

(* update line with guaranteed cells *)
let update_line_with_guaranteed_cells hint line configs =
  let valid_configs = List.filter (is_same line) configs in
  if valid_configs = [] then
    line
  else
    List.mapi (fun i cell ->
      if cell <> Unknown then cell
      else
        let first = List.nth (List.hd valid_configs) i in
        if List.for_all (fun config -> List.nth config i = first) valid_configs then
          first
        else
          Unknown
    ) line

(* update puzzle rows and columns *)
let update_puzzle puzzle row_hints col_hints =
  (* update rows *)
  let updated_puzzle = 
    List.fold_left2 (fun acc i hint ->
      let row = get_line acc Row i in
      let configs = get_line_configurations hint (List.length row) in
      let updated_row = update_line_with_guaranteed_cells hint row configs in
      if updated_row = row then acc else
        update_line acc Row i updated_row
    ) puzzle (List.init (List.length row_hints) (fun x -> x)) row_hints
  in

  (* update columns *)
  let updated_puzzle = 
    List.fold_left2 (fun acc i hint ->
      let column = get_line acc Column i in
      let configs = get_line_configurations hint (List.length column) in
      let updated_column = update_line_with_guaranteed_cells hint column configs in
      if updated_column = column then acc else
        update_line acc Column i updated_column
    ) updated_puzzle (List.init (List.length col_hints) (fun x -> x)) col_hints
  in

  updated_puzzle

(* check if solved *)
let is_solved puzzle =
  List.for_all (fun row -> List.for_all (fun cell -> cell <> Unknown) row) puzzle

(* check if puzzles are same *)
let is_puzzle_same p1 p2 =
  List.for_all2 (fun row1 row2 ->
    List.for_all2 (fun cell1 cell2 -> cell1 = cell2) row1 row2
  ) p1 p2

(* find next unknown cell *)
let get_next_unknown_cell puzzle =
  let rec aux i j =
    if i >= List.length puzzle then None
    else if j >= List.length (List.nth puzzle i) then aux (i + 1) 0
    else if List.nth (List.nth puzzle i) j = Unknown then Some (i, j)
    else aux i (j + 1)
  in
  aux 0 0

(* update a cell *)
let update_cell p (i, j) value =
  List.mapi (fun row_idx row ->
    if row_idx = i then
      List.mapi (fun col_idx cell -> if col_idx = j then value else cell) row
    else row
  ) p

(* backtrack solver *)
let rec backtrack_solver p row_hints col_hints =
  if is_solved p then Some p
  else
    match get_next_unknown_cell p with
    | None -> None
    | Some (i, j) ->
        (* try filled *)
        let new_p_filled = update_cell p (i, j) Filled in
        if validate_line (List.nth row_hints i) (List.nth new_p_filled i) &&
           validate_line (List.nth col_hints j) (get_line new_p_filled Column j) then
          match backtrack_solver new_p_filled row_hints col_hints with
          | Some sol_p -> Some sol_p
          | None ->
            (* try empty *)
            let new_p_empty = update_cell p (i, j) Empty in
            if validate_line (List.nth row_hints i) (List.nth new_p_empty i) &&
               validate_line (List.nth col_hints j) (get_line new_p_empty Column j) then
              backtrack_solver new_p_empty row_hints col_hints
            else
              None
        else
          (* try empty if filled didn't work *)
          let new_p_empty = update_cell p (i, j) Empty in
          if validate_line (List.nth row_hints i) (List.nth new_p_empty i) &&
             validate_line (List.nth col_hints j) (get_line new_p_empty Column j) then
            backtrack_solver new_p_empty row_hints col_hints
          else
            None

(* solve nonogram *)
let solve_nonogram row_hints col_hints =
  let row_size = List.length row_hints in
  let col_size = List.length col_hints in
  let initial_puzzle = initialize_puzzle row_size col_size in
  let rec solve current_puzzle =
    if is_solved current_puzzle then Some current_puzzle
    else
      let updated_puzzle = update_puzzle current_puzzle row_hints col_hints in
      if is_puzzle_same current_puzzle updated_puzzle then
        backtrack_solver updated_puzzle row_hints col_hints
      else
        solve updated_puzzle
  in
  solve initial_puzzle

(* convert cell to string *)
let cell_to_string cell =
  match cell with
  | Filled -> "█"
  | Empty -> " "
  | Unknown -> "?"

(* convert cell list to string *)
let cell_list_to_string cells =
  List.fold_left (fun acc cell -> acc ^ (cell_to_string cell) ^ " ") "" cells

(* convert puzzle to string *)
let puzzle_to_string puzzle =
  List.fold_left (fun acc row -> acc ^ (cell_list_to_string row) ^ "\n") "" puzzle

(* compare puzzles *)
let puzzles_equal p1 p2 =
  let rec aux l1 l2 =
    match l1, l2 with
    | [], [] -> true
    | row1 :: xs1, row2 :: xs2 ->
        (List.for_all2 (fun a b -> a = b) row1 row2) && aux xs1 xs2
    | _, _ -> false
  in
  aux p1 p2

(* print puzzle *)
let print_puzzle puzzle =
  List.iter (fun row ->
    List.iter (fun cell ->
      print_string (cell_to_string cell)
    ) row;
    print_newline ()
  ) puzzle;
  print_newline ()

(*--------------------------------------------------------------------------------------------*)
(* testing *)

(* check row equality *)
let assert_row_equal expected actual =
  let expected_str = cell_list_to_string expected in
  let actual_str = cell_list_to_string actual in
  if expected_str = actual_str then
    print_endline "Row Test passed"
  else
    print_endline ("Row Test failed. Expected: " ^ expected_str ^ " but got: " ^ actual_str)

(* check puzzle equality *)
let assert_puzzle_equal expected actual =
  let expected_str = puzzle_to_string expected in
  let actual_str = puzzle_to_string actual in
  if expected_str = actual_str then
    print_endline "Puzzle Test passed"
  else
    print_endline ("Puzzle Test failed. Expected:\n" ^ expected_str ^ "but got:\n" ^ actual_str)

(* check boolean equality *)
let assert_bool_equal expected actual =
  if expected = actual then
    print_endline "Boolean Test passed"
  else
    print_endline ("Boolean Test failed. Expected: " ^ string_of_bool expected ^ " but got: " ^ string_of_bool actual)

(* test get_line *)
let test_get_line () =
  print_endline "Running test_get_line...";
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
let test_initialize_puzzle () =
  print_endline "Running test_initialize_puzzle...";
  let rows, cols = 2, 3 in
  let expected = [
    [Unknown; Unknown; Unknown];
    [Unknown; Unknown; Unknown]
  ] in
  let actual = initialize_puzzle rows cols in
  assert_puzzle_equal expected actual

(* test validate_line *)
let test_validate_line () =
  print_endline "Running test_validate_line...";
  assert_bool_equal true (validate_line [3] [Filled; Filled; Filled; Empty; Empty]);
  assert_bool_equal false (validate_line [3] [Filled; Empty; Filled; Empty; Empty]);
  assert_bool_equal true (validate_line [1; 1] [Filled; Empty; Filled; Empty; Empty])

(* more validate_line tests *)
let test_validate_line_additional () =
  print_endline "Running additional validate_line tests...";
  
  (* valid *)
  assert_bool_equal true (validate_line [3] [Filled; Filled; Filled; Empty; Empty]);
  assert_bool_equal true (validate_line [3] [Empty; Filled; Filled; Filled; Empty]);
  assert_bool_equal true (validate_line [3] [Empty; Empty; Filled; Filled; Filled]);
  assert_bool_equal true (validate_line [1; 1] [Filled; Empty; Filled; Empty; Empty]);
  assert_bool_equal true (validate_line [1; 1] [Empty; Filled; Empty; Filled; Empty]);
  assert_bool_equal true (validate_line [1; 1] [Empty; Filled; Empty; Filled; Empty]);
  
  (* invalid *)
  assert_bool_equal false (validate_line [1; 1] [Filled; Filled; Filled; Empty; Empty]);
  assert_bool_equal false (validate_line [2] [Filled; Filled; Filled; Empty; Empty]);
  assert_bool_equal false (validate_line [2; 1] [Filled; Filled; Empty; Filled; Filled]);
  assert_bool_equal false (validate_line [5] [Filled; Filled; Filled; Filled]);
  assert_bool_equal false (validate_line [2; 2] [Filled; Filled; Empty; Filled; Empty]);
  
  (* edge cases *)
  assert_bool_equal true (validate_line [] [Empty; Empty; Empty]);
  assert_bool_equal false (validate_line [] [Empty; Filled; Empty]);
  
  print_endline "Additional validate_line tests complete."

(* test generate_line_configurations *)
let test_generate_line_configurations () =
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
let test_update_line_with_guaranteed_cells () =
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
let test_is_same () =
  print_endline "Running test_is_same...";
  let ref_line = [Filled; Empty; Unknown] in
  let cmp_line = [Filled; Empty; Unknown] in
  assert_bool_equal true (is_same ref_line cmp_line);
  assert_bool_equal false (is_same [Filled; Filled; Unknown] cmp_line);
  
  (* empty lists *)
  assert_bool_equal true (is_same [] [])

(* test validate_column *)
let test_validate_column () =
  print_endline "Running test_validate_column...";
  assert_bool_equal true (validate_line [3] [Filled; Filled; Filled; Empty; Empty]);
  assert_bool_equal false (validate_line [3] [Filled; Empty; Filled; Empty; Empty])

(* test update_column_with_guaranteed_cells *)
let test_update_column_with_guaranteed_cells () =
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

(* test cases type *)
type test_case = {
  name : string;
  row_hints : hint list;
  col_hints : hint list;
  expected_solution : puzzle;
}

let test_cases : test_case list = [
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
]

(* run all tests *)
let run_tests () =
  List.iter (fun test ->
    print_endline ("Running Test: " ^ test.name);
    match solve_nonogram test.row_hints test.col_hints with
    | None ->
        print_endline "Solver failed to find a solution.\n";
    | Some solution ->
        if puzzles_equal solution test.expected_solution then
          print_endline "Test Passed.\n"
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