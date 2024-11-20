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
let update_line_with_guaranteed_cells _hint line configs =
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