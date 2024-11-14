(* Define the types *)
exception Error of string

type cell = Filled | Empty | Unknown
type row_hint = int list
type column_hint = int list
type puzzle = cell list list

let get_column : puzzle -> int -> cell list = fun puzzle col_index ->
  List.map (fun row -> List.nth row col_index) puzzle

let update_column : puzzle -> int -> cell list -> puzzle = fun puzzle col_index new_col ->
  List.mapi (fun row_idx row ->
      List.mapi (fun col_idx cell ->
          if col_idx = col_index then List.nth new_col row_idx else cell
        ) row
    ) puzzle

let rec get_list_sum : int list -> int = fun lst ->
  match lst with
  | [] -> 0
  | x :: xs -> x + (get_list_sum xs);;

(* Function to initialize the puzzle grid with Unknown cells *)
let initialize_puzzle : int -> int -> puzzle = fun rows cols ->
  List.init rows (fun _ -> List.init cols (fun _ -> Unknown))

(* Function to validate whether a row configuration satisfies its hint *)
let validate_row : row_hint -> cell list -> bool = fun hint row ->
  (* Implement the validation logic *)
  false

(* Function to validate whether a column configuration satisfies its hint *)
let validate_column : column_hint -> cell list -> bool = fun hint column ->
  (* Implement the validation logic *)
  false

(* Function to generate possible configurations for a row based on its hint *)
let rec generate_row_configurations : row_hint -> int -> int list -> cell list list = fun hint length acc->
  let total_filled = get_list_sum hint in (* get total amount filled for max beginning empty slots*)
  if total_filled > length then
    raise Error "Invalid hint given (generate_row_configurations)" (* more hints than available row indices*)
  else if total_filled = length then
    acc @ (List.init length (fun _ -> Filled)) (* if equal, then the only configuration is all filled*)
  else
    match hint with
    | [] -> acc (* reached end, acc contains only available config*)
    | x :: xs -> [
      generate_row_configurations hint (length - 1) (acc @ [Empty]); (*configuration with putting empty in current slot*)
      generate_row_configurations xs (length - x) (acc @ (List.init x (fun _ -> Filled))) (*configuration with putting Filled and going to further hints*)
      ]

(* Function to generate possible configurations for a column based on its hint *)
let generate_column_configurations : column_hint -> int -> int list -> cell list list = fun hint length acc->
  let total_filled = get_list_sum hint in (* get total amount filled for max beginning empty slots*)
  if total_filled > length then
    raise Error "Invalid hint given (generate_column_configurations)" (* more hints than available column indices*)
  else if total_filled = length then
    acc @ (List.init length (fun _ -> Filled)) (* if equal, then the only configuration is all filled*)
  else
    match hint with
    | [] -> acc (* reached end, acc contains only available config*)
    | x :: xs -> [
      generate_column_configurations hint (length - 1) (acc @ [Empty]); (*configuration with putting empty in current slot*)
      generate_column_configurations xs (length - x) (acc @ (List.init x (fun _ -> Filled))) (*configuration with putting Filled and going to further hints*)
      ]

(* Function to update the puzzle grid based on row constraints *)
let update_row_with_guaranteed_cells : row_hint -> cell list -> cell list = fun hint row ->
  (* Implement the update logic *)
  row

(* Function to update the puzzle grid based on column constraints *)
let update_column_with_guaranteed_cells : column_hint -> cell list -> cell list = fun hint column ->
  (* Implement the update logic *)
  column

(* Function to apply row and column updates to the puzzle grid *)
let update_puzzle : puzzle -> row_hint list -> column_hint list -> puzzle = fun puzzle row_hints col_hints ->
  (* Implement the update logic *)
  puzzle

(* Function to check if the puzzle is solved *)
let is_solved : puzzle -> bool = fun puzzle ->
  (* Implement the check logic *)
  false

(* Recursive function to solve the puzzle using backtracking or logical deduction *)
let solve_nonogram : row_hint list -> column_hint list -> puzzle option = fun row_hints col_hints ->
  let row_size = List.length row_hints in
  let col_size = List.length col_hints in
  let p = initialize_puzzle row_size col_size in(*init puzzle to Unknown*)


  None