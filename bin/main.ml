(* Define the types *)
exception Error of string

type cell = Filled | Empty | Unknown
type row_hint = int list
type column_hint = int list
type puzzle = cell list list

(* Function to get a specific column from the puzzle *)
let get_column : puzzle -> int -> cell list = fun puzzle col_index ->
  List.map (fun row -> List.nth row col_index) puzzle

(* Function to update a specific column in the puzzle *)
let update_column : puzzle -> int -> cell list -> puzzle = fun puzzle col_index new_col ->
  List.mapi (fun row_idx row ->
      List.mapi (fun col_idx cell ->
          if col_idx = col_index then List.nth new_col row_idx else cell
        ) row
    ) puzzle

(* Function to get the first element of a list, or raise an error if empty *)
let get_first_elem_of_list : 'a list -> 'a = fun lst ->
  match lst with
  | [] -> raise (Error "Empty list has no first element")
  | x :: _ -> x

(* Function to get the rest of a list after the first element *)
let get_rest_of_list : 'a list -> 'a list = fun lst ->
  match lst with
  | [] -> []
  | x :: xs -> xs

(* Function to get the sum of integers in a list *)
let rec get_list_sum : int list -> int = fun lst ->
  match lst with
  | [] -> 0
  | x :: xs -> x + (get_list_sum xs)

(* Function to remove a specified number of elements from a list *)
let rec remove_from_list : 'a list -> int -> 'a list = fun lst amt ->
  match lst with
  | [] -> []
  | x :: xs -> if amt > 0 then remove_from_list xs (amt - 1) else xs

(* Function to initialize the puzzle grid with Unknown cells *)
let initialize_puzzle : int -> int -> puzzle = fun rows cols ->
  List.init rows (fun _ -> (List.init cols (fun _ -> Unknown)))

(* Function to validate whether a row configuration satisfies its hint *)
let rec validate_row : row_hint -> cell list -> bool = fun hint row ->
  let hint_size = List.length hint in
  match row with
  | [] -> if hint_size > 0 then false else true (* if row ends but hints remain, return false; otherwise, it's valid *)
  | x :: xs -> begin
    if hint_size = 0 then
      if x = Filled then false else validate_row hint xs (* if no hints but cells left, they must all be Empty or Unknown *)
    else
      let this_hint = get_first_elem_of_list hint in 
      if x = Filled then 
        validate_row (get_rest_of_list hint) (remove_from_list row this_hint) (* if Filled, apply hint and recurse *)
      else
        validate_row hint xs (* if Empty or Unknown, recurse without changing hints *)
  end

(* Function to validate whether a column configuration satisfies its hint *)
let rec validate_column : column_hint -> cell list -> bool = fun hint column -> (* same logic as validate_row *)
  let hint_size = List.length hint in
  match column with
  | [] -> if hint_size > 0 then false else true
  | x :: xs -> begin
    if hint_size = 0 then
      if x = Filled then false else validate_column hint xs
    else
      let this_hint = get_first_elem_of_list hint in
      if x = Filled then 
        validate_column (get_rest_of_list hint) (remove_from_list column this_hint)
      else
        validate_column hint xs
  end

(* Updated generate_row_configurations with debugging *)
let rec generate_row_configurations : row_hint -> int -> cell list -> cell list list = fun hint length acc ->
  let total_filled = get_list_sum hint in (* total required filled cells *)
  let min_length = total_filled + List.length hint - 1 in (* minimum row length to fit hints *)

  if min_length > length then
    [] (* not enough space for hints *)
  else if hint = [] then
    [acc @ (List.init (max 0 length) (fun _ -> Empty))] (* fill remaining cells with Empty if no more hints *)
  else
    let this_hint = get_first_elem_of_list hint in
    let rest_hint = get_rest_of_list hint in

    let rec recurse_hints start configs =
      if start + min_length > length then
        configs
      else
        let filled_part = (List.init start (fun _ -> Empty)) @ (List.init this_hint (fun _ -> Filled)) in
        let new_acc = acc @ filled_part in
        let new_acc = if rest_hint = [] then new_acc else new_acc @ [Empty] in
        let remaining_length = max 0 (length - start - this_hint - 1) in
        let new_configs = generate_row_configurations rest_hint remaining_length new_acc in
        recurse_hints (start + 1) (configs @ new_configs)
    in
    recurse_hints 0 []

(* Function to generate possible configurations for a column based on its hint *)
let rec generate_column_configurations : column_hint -> int -> cell list -> cell list list = fun hint length acc ->
  let total_filled = get_list_sum hint in
  let min_length = total_filled + List.length hint - 1 in

  if min_length > length then
    []
  else if hint = [] then
    [acc @ (List.init (max 0 length) (fun _ -> Empty))] (* ensure length is non-negative *)
  else
    let this_hint = get_first_elem_of_list hint in
    let rest_hint = get_rest_of_list hint in

    let rec recurse_hints start configs =
      if start + min_length > length then
        configs
      else
        let filled = (List.init start (fun _ -> Empty)) @ (List.init this_hint (fun _ -> Filled)) in
        let new_acc = acc @ filled in
        let new_acc = if rest_hint = [] then new_acc else new_acc @ [Empty] in
        let remaining_length = max 0 (length - start - this_hint - 1) in (* ensure non-negative *)
        let new_configs = generate_column_configurations rest_hint remaining_length new_acc in
        recurse_hints (start + 1) (configs @ new_configs)
    in
    recurse_hints 0 []

let row_config_cache = Hashtbl.create 1000
let column_config_cache = Hashtbl.create 1000

let get_row_configurations : row_hint -> int -> cell list list = fun hint length ->
  try Hashtbl.find row_config_cache (hint, length)
  with Not_found ->
    let configs = generate_row_configurations hint length [] in
    Hashtbl.add row_config_cache (hint, length) configs;
    configs

let get_column_configurations : column_hint -> int -> cell list list = fun hint length ->
  try Hashtbl.find column_config_cache (hint, length)
  with Not_found ->
    let configs = generate_column_configurations hint length [] in
    Hashtbl.add column_config_cache (hint, length) configs;
    configs

(* Helper functions to check if cells in two lists match based on certain criteria *)
  let is_filled_same : cell list -> cell list -> bool = fun ref cmp ->
    if List.length ref <> List.length cmp then false
    else List.for_all2 (fun x y -> if x = Filled then (x = y) else true) ref cmp
  
  let is_empty_same : cell list -> cell list -> bool = fun ref cmp ->
    if List.length ref <> List.length cmp then false
    else List.for_all2 (fun x y -> if x = Empty then (x = y) else true) ref cmp
  
  let is_same : cell list -> cell list -> bool = fun ref cmp ->
    if List.length ref <> List.length cmp then false
    else (is_filled_same ref cmp) && (is_empty_same ref cmp)

(* Helper function to determine consensus across configurations for a specific position *)
let consensus_at_position configs pos =
  let values = List.map (fun config -> List.nth config pos) configs in (* get values at pos across all configs *)
  match List.fold_left (fun acc v -> if acc = Some v then acc else None) (Some (List.hd values)) (List.tl values) with
  | Some value -> value
  | None -> Unknown

(* Updated update_row_with_guaranteed_cells to avoid raising an error *)
let update_row_with_guaranteed_cells : row_hint -> cell list -> cell list = fun hint row ->
  let configs = get_row_configurations hint (List.length row) in
  let valid_configs = List.filter (fun config -> is_same row config) configs in
  if List.length valid_configs = 0 then
    row  (* If no valid configurations, return the row unchanged *)
  else
    List.mapi (fun i cell -> 
      match cell with
      | Unknown -> consensus_at_position valid_configs i
      | _ -> cell
    ) row

(* Update column based on guaranteed cells across all valid configurations *)
let update_column_with_guaranteed_cells : column_hint -> cell list -> cell list = fun hint column ->
  let configs = get_column_configurations hint (List.length column) in
  let valid_configs = List.filter (fun config -> is_same column config) configs in
  if List.length valid_configs = 0 then
    column
  else
    List.mapi (fun i cell -> 
      match cell with
      | Unknown -> consensus_at_position valid_configs i
      | _ -> cell
    ) column

(* Function to apply both row and column updates to the puzzle grid *)
let update_puzzle : puzzle -> row_hint list -> column_hint list -> puzzle = fun puzzle row_hints col_hints ->
  let updated_rows = List.mapi (fun i row -> update_row_with_guaranteed_cells (List.nth row_hints i) row) puzzle in
  let all_columns = List.init (List.length col_hints) (fun i -> get_column puzzle i) in
  let updated_columns = List.mapi (fun i column -> update_column_with_guaranteed_cells (List.nth col_hints i) column) all_columns in
  List.mapi (fun i row -> 
    List.mapi (fun j cell -> 
      let row_value = List.nth (List.nth updated_rows i) j in
      let col_value = List.nth (List.nth updated_columns j) i in
      match row_value, col_value with
      | r, c when r = c -> r (* if row and column updates agree, keep the value *)
      | v, Unknown | Unknown, v -> v (* if one is Unknown, take the other *)
      | _ -> Unknown (* otherwise, set to Unknown *)
    ) row
  ) puzzle

(* Check if all cells in the puzzle are either Filled or Empty, meaning the puzzle is solved *)
let is_solved : puzzle -> bool = fun puzzle ->
  List.for_all (fun row -> List.for_all (fun cell -> cell <> Unknown) row) puzzle

(* Check if two puzzles are identical by comparing all cells *)
let is_puzzle_same : puzzle -> puzzle -> bool = fun p1 p2 ->
  List.for_all2 (fun row1 row2 ->
    List.for_all2 (fun cell1 cell2 -> cell1 = cell2) row1 row2
  ) p1 p2

(* Find the next cell in the puzzle with an Unknown value *)
let get_next_unknown_cell : puzzle -> (int * int) option = fun puzzle ->
  let rec get_next_unknown_cell' : puzzle -> int -> int -> (int * int) option = fun puzzle i j ->
    if i = List.length puzzle then None
    else if j = List.length (List.nth puzzle i) then get_next_unknown_cell' puzzle (i + 1) 0
    else if List.nth (List.nth puzzle i) j = Unknown then Some (i, j)
    else get_next_unknown_cell' puzzle i (j + 1)
  in
  get_next_unknown_cell' puzzle 0 0

(* Update a specific cell in the puzzle with a new value *)
let update_cell : puzzle -> (int * int) -> cell -> puzzle = fun p (i, j) value ->
  List.mapi (fun row_idx row ->
    if row_idx = i then
      List.mapi (fun col_idx cell -> if col_idx = j then value else cell) row
    else row
  ) p

(* Backtracking function to attempt solving when deduction stalls *)
let rec backtrack_solver : puzzle -> row_hint list -> column_hint list -> puzzle option = fun p row_hints col_hints ->
  if is_solved p then Some p
  else
    let next_unknown_cell = get_next_unknown_cell p in
    match next_unknown_cell with
    | None -> None (* Backtrack if no solution found *)
    | Some (i, j) -> begin
        let new_p = update_cell p (i, j) Filled in
        if validate_row (List.nth row_hints i) (List.nth new_p i) && validate_column (List.nth col_hints j) (get_column new_p j) then
          match backtrack_solver new_p row_hints col_hints with
          | Some sol_p -> Some sol_p
          | None ->
            let new_p = update_cell p (i, j) Empty in
            if validate_row (List.nth row_hints i) (List.nth new_p i) && validate_column (List.nth col_hints j) (get_column new_p j) then
              backtrack_solver new_p row_hints col_hints
            else None
        else
          let new_p = update_cell p (i, j) Empty in
          if validate_row (List.nth row_hints i) (List.nth new_p i) && validate_column (List.nth col_hints j) (get_column new_p j) then
            backtrack_solver new_p row_hints col_hints
          else None
      end

(* Main function to solve the puzzle, combining deduction and backtracking *)
let solve_nonogram : row_hint list -> column_hint list -> puzzle option = fun row_hints col_hints ->
  let row_size = List.length row_hints in
  let col_size = List.length col_hints in
  let p = initialize_puzzle row_size col_size in
  let rec repeat_solve p =
    let prev_p = p in (* Make a copy to track changes *)
    let p_new = update_puzzle p row_hints col_hints in
    if is_solved p_new then Some p_new (* If puzzle is solved, return it *)
    else if is_puzzle_same prev_p p_new then 
      backtrack_solver p_new row_hints col_hints (* Backtrack if no progress *)
    else repeat_solve p_new (* Continue solving if there were updates *)
  in
  repeat_solve p (* Start the solving process *)

(* Helper function to print the puzzle for visualization *)
let print_puzzle puzzle =
  List.iter (fun row ->
    List.iter (fun cell ->
      match cell with
      | Filled -> print_string "█"
      | Empty -> print_string " "
      | Unknown -> print_string "?"
    ) row;
    print_newline ()
  ) puzzle;
  print_newline ()


(*--------------------------------------------------------------------------------------------*)
(* Helper functions for testing *)

(* Helper function to convert a cell to a string *)
let cell_to_string cell =
  match cell with
  | Filled -> "█"
  | Empty -> " "
  | Unknown -> "?"

(* Helper function to convert a cell list to a string *)
let cell_list_to_string cells =
  List.fold_left (fun acc cell -> acc ^ (cell_to_string cell)) "" cells

(* Modified assert_equal to handle cell list inputs *)
let assert_row_equal expected actual =
  let expected_str = cell_list_to_string expected in
  let actual_str = cell_list_to_string actual in
  if expected_str = actual_str then
    print_endline "Row Test passed"
  else
    print_endline ("Row Test failed. Expected: " ^ expected_str ^ " but got: " ^ actual_str)

(* Helper function to convert a puzzle (list of cell lists) to a string *)
let puzzle_to_string puzzle =
  List.fold_left (fun acc row -> acc ^ (cell_list_to_string row) ^ "\n") "" puzzle

(* Modified assert_puzzle_equal to handle puzzle inputs *)
let assert_puzzle_equal expected actual =
  let expected_str = puzzle_to_string expected in
  let actual_str = puzzle_to_string actual in
  if expected_str = actual_str then
    print_endline "Puzzle Test passed"
  else
    print_endline ("Puzzle Test failed. Expected:\n" ^ expected_str ^ "\nbut got:\n" ^ actual_str)

(* Helper function to compare boolean values *)
let assert_bool_equal expected actual =
  if expected = actual then
    print_endline "Boolean Test passed"
  else
    print_endline ("Boolean Test failed. Expected: " ^ string_of_bool expected ^ " but got: " ^ string_of_bool actual)

(* Test cases for get_column *)
let test_get_column () =
  print_endline "Running test_get_column...";
  let puzzle = [
    [Filled; Empty; Unknown];
    [Empty; Filled; Filled];
    [Unknown; Empty; Filled]
  ] in
  let expected = [Empty; Filled; Empty] in
  let actual = get_column puzzle 1 in
  assert_row_equal expected actual

(* Test cases for initialize_puzzle *)
let test_initialize_puzzle () =
  print_endline "Running test_initialize_puzzle...";
  let rows, cols = 2, 3 in
  let expected = [
    [Unknown; Unknown; Unknown];
    [Unknown; Unknown; Unknown]
  ] in
  let actual = initialize_puzzle rows cols in
  assert_puzzle_equal expected actual

(* Test cases for validate_row *)
let test_validate_row () =
  print_endline "Running test_validate_row...";
  assert_bool_equal true (validate_row [3] [Filled; Filled; Filled; Empty; Empty]);
  assert_bool_equal false (validate_row [3] [Filled; Empty; Filled; Empty; Empty]);
  assert_bool_equal true (validate_row [1; 1] [Filled; Empty; Filled; Empty; Empty])

(* Test cases for generate_row_configurations *)
let test_generate_row_configurations () =
  print_endline "Running test_generate_row_configurations...";
  let hint = [2] in
  let length = 3 in
  let expected = [
    [Filled; Filled; Empty];
    [Empty; Filled; Filled]
  ] in
  let actual = generate_row_configurations hint length [] in
  assert_puzzle_equal expected actual;

  let hint = [1; 1] in
  let length = 3 in
  let expected = [
    [Filled; Empty; Filled];
    [Empty; Filled; Filled]
  ] in
  let actual = generate_row_configurations hint length [] in
  assert_puzzle_equal expected actual

(* Test cases for update_row_with_guaranteed_cells *)
let test_update_row_with_guaranteed_cells () =
  print_endline "Running test_update_row_with_guaranteed_cells...";
  let row = [Unknown; Filled; Unknown] in
  let hint = [1; 1] in
  let expected = [Empty; Filled; Filled] in
  let actual = update_row_with_guaranteed_cells hint row in
  assert_row_equal expected actual;

  let row = [Unknown; Unknown; Unknown] in
  let hint = [3] in
  let expected = [Filled; Filled; Filled] in
  let actual = update_row_with_guaranteed_cells hint row in
  assert_row_equal expected actual

(* Test cases for is_filled_same *)
let test_is_filled_same () =
  print_endline "Running test_is_filled_same...";
  let ref_row = [Filled; Empty; Unknown] in
  let cmp_row = [Filled; Empty; Unknown] in
  assert_bool_equal true (is_filled_same ref_row cmp_row);
  assert_bool_equal false (is_filled_same [Filled; Filled; Unknown] cmp_row);

  (* Edge case: empty lists *)
  assert_bool_equal true (is_filled_same [] [])

(* Test cases for validate_column *)
let test_validate_column () =
  print_endline "Running test_validate_column...";
  assert_bool_equal true (validate_column [3] [Filled; Filled; Filled; Empty; Empty]);
  assert_bool_equal false (validate_column [3] [Filled; Empty; Filled; Empty; Empty])

(* Test cases for update_column_with_guaranteed_cells *)
let test_update_column_with_guaranteed_cells () =
  print_endline "Running test_update_column_with_guaranteed_cells...";
  let column = [Unknown; Filled; Unknown] in
  let hint = [1; 1] in
  let expected = [Empty; Filled; Filled] in
  let actual = update_column_with_guaranteed_cells hint column in
  assert_row_equal expected actual;

  let column = [Unknown; Unknown; Unknown] in
  let hint = [3] in
  let expected = [Filled; Filled; Filled] in
  let actual = update_column_with_guaranteed_cells hint column in
  assert_row_equal expected actual

(* Run all tests *)
let () =
  print_endline "Running tests...";
  test_get_column ();
  test_initialize_puzzle ();
  test_validate_row ();
  test_generate_row_configurations ();
  test_update_row_with_guaranteed_cells ();
  test_is_filled_same ();
  test_validate_column ();
  test_update_column_with_guaranteed_cells ();
  print_endline "All tests complete."