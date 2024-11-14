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

let get_first_elem_of_list : 'a list -> 'a = fun lst ->
  match lst with
  | [] -> raise Error "Empty list has no first element"
  | x :: _ -> x

let get_rest_of_list : 'a list -> 'a list = fun lst ->
  match lst with
  | [] -> []
  | x :: xs -> xs

let rec get_list_sum : int list -> int = fun lst ->
  match lst with
  | [] -> 0
  | x :: xs -> x + (get_list_sum xs);;

let rec remove_from_list : int list -> int -> int list = fun lst amt ->
  match lst with
  | [] -> []
  | x :: xs -> if amt > 0 then remove_from_list xs (amt - 1) else xs

(* Function to initialize the puzzle grid with Unknown cells *)
let initialize_puzzle : int -> int -> puzzle = fun rows cols ->
  List.init rows (fun _ -> List.init cols (fun _ -> Unknown))

(* Function to validate whether a row configuration satisfies its hint *)
let rec validate_row : row_hint -> cell list -> bool = fun hint row ->
  let hint_size = List.length hint in
  match row with
  | [] -> if hint_size > 0 then false else true (*if row is through but still hints left, false, else its a good solution*)
  | x :: xs -> begin
    if hint_size = 0 then
      if x = Filled then false else validate_row hint xs (*if no more hints, but still some row, then all rest must be Empty or Unknown. If not return false*)
    else
      let this_hint = get_first_elem_of_list hint in 
      if x = Filled then 
        validate_row (get_rest_of_list hint) (remove_from_list row this_hint) (*if current row index is Filled, apply the hint and recurse with rest of hint and row*)
      else
        validate_row hint xs (*if current row index is Empty or Unknown, keep hints intact and recurse with rest of row*)
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
let rec generate_column_configurations : column_hint -> int -> int list -> cell list list = fun hint length acc->
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

let is_filled_same : cell list -> cell list -> bool = fun ref cmp ->
  List.for_all2 (fun x y -> if x = Filled then (x=y) else true) ref cmp

let is_empty_same : cell list -> cell list -> bool = fun ref cmp ->
  List.for_all2 (fun x y -> if x = Empty then (x=y) else true) ref cmp

let is_same : cell list -> cell list -> bool = fun ref cmp ->
  (is_filled_same ref cmp) && (is_empty_same ref cmp)

let consensus_at_position configs pos =
  let values = List.map (fun config -> List.nth config pos) configs in (* get the values at pos of each config*)
  match List.fold_left (fun acc v -> if acc = Some v then acc else None) (Some (List.hd values)) (List.tl values) with (* if all values are same, return that value, else return Unknown*)
  | Some value -> value
  | None -> Unknown

(* Function to update the puzzle grid based on row constraints *)
let update_row_with_guaranteed_cells : row_hint -> cell list -> cell list = fun hint row ->
  let configs = generate_row_configurations hint (List.length row) [] in
  (*keep only the configs that actually have the same Filled and Empty slots as row*)
  let valid_configs = List.filter (fun config -> is_same row config) configs in
  if List.length valid_configs = 0 then
    raise Error "No valid configurations found (update_row_with_guaranteed_cells)"
  else
    List.mapi (fun i cell -> (* for each cell in the configs, if it was Unknown originally, update if possible
                                else, keep it as it was*)
      match cell with
      | Unknown -> consensus_at_position valid_configs i
      | _ -> cell
    ) row

(* Function to update the puzzle grid based on column constraints *)
let update_column_with_guaranteed_cells : column_hint -> cell list -> cell list = fun hint column ->
  let configs = generate_column_configurations hint (List.length column) [] in
  (*keep only the configs that actually have the same Filled and Empty slots as column*)
  let valid_configs = List.filter (fun config -> is_same column config) configs in
  if List.length valid_configs = 0 then
    raise Error "No valid configurations found (update_column_with_guaranteed_cells)"
  else
    List.mapi (fun i cell -> (* for each cell in the configs, if it was Unknown originally, update if possible
                                else, keep it as it was*)
      match cell with
      | Unknown -> consensus_at_position valid_configs i
      | _ -> cell
    ) column

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