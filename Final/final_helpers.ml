(*
 * final_helpers.ml: utility types/functions/values for
 * 2016 CS 4 final exam
 *)

open Printf

(* ---------------------------------------------------------
   Types.
 ----------------------------------------------------------- *)

(* direction type: horizontal or vertical *)
type dir = H | V

(* piece length: 2 or 3 *)
type len = P2 | P3

(* location of a piece on the board *)
type loc = int * int

(* ---------------------------------------------------------
   Parameters.
 ----------------------------------------------------------- *)

let board_size = 6

(* ---------------------------------------------------------
   Helper functions.
 ----------------------------------------------------------- *)

(* convert a dir to a string *)
let string_of_dir = function
  | H -> "H"
  | V -> "V"

(* convert a len to an int *)
let int_of_len = function
  | P2 -> 2
  | P3 -> 3

(* convert an int to a len *)
let len_of_int = function
  | 2 -> P2
  | 3 -> P3
  | n -> invalid_arg (sprintf "invalid piece length: %d" n)

(* Check that a row or column index exists on the board. *)
let valid_index i = i >= 0 && i < board_size
 
(* Check that a (row, column) location exists on the board. *)
let valid_loc (r, c) = valid_index r && valid_index c

(* Move a location tuple by i rows and j columns if possible. *)
let move_loc (r, c) i j = 
  let r' = r + i in
  let c' = c + j in
    if valid_loc (r', c') then Some (r', c') else None
  
(* Convert a (row, column) location to a string. *)
let string_of_loc (r, c) = sprintf "(%d, %d)" r c

(* Convert a list of locs to a string. *)
let string_of_locs locs = 
  String.concat ", " (List.map string_of_loc locs)

(*
 * Map a function which returns an option type over a list of
 * option values.  If any of the resulting option values is None, 
 * return None.  Otherwise return Some (list of resulting values).
 *)
let rec mapMaybe f lst =
  match lst with
    | [] -> Some []
    | h :: t -> 
        (match f h with
           | None -> None
           | Some h' -> 
              (match mapMaybe f t with
                 | None -> None
                 | Some t' -> Some (h' :: t')))

(* Generate a range of ints between m and n-1. *)
let rec range m n = if m >= n then [] else m :: range (m + 1) n

(* Compute the minimum of a list. *)
let min_list = function
  | [] -> failwith "minimum of empty list"
  | h :: t -> List.fold_left min h t

(* Compute the maximum of a list. *)
let max_list = function
  | [] -> failwith "maximum of empty list"
  | h :: t -> List.fold_left max h t

(*
 * Return a list of values that are in lst1 but not lst2.
 * Assumes that lst1 and lst2 have no repeated elements.
 * Preserves the order of lst1 (which isn't essential).
 *)
let difference lst1 lst2 =
  let rec iter lst1 lst2 result =
    match lst1 with
      | [] -> result
      | h :: t -> 
          if List.mem h lst2 
            then iter t lst2 result
            else iter t lst2 (h :: result)
  in List.rev (iter lst1 lst2 [])

(* 
 * Return a list of values that are between the values in lst1 and lst2.
 * Assume that lst1 and lst2 are lists of ints with no repeats or gaps.
 *)
let betweens lst1 lst2 =
  let all_vals   = List.append lst1 lst2 in
  let min_val    = min_list all_vals in
  let max_val    = max_list all_vals in
  let range_vals = range min_val (max_val + 1) in
    difference range_vals all_vals

(*
 * Return true if a piece name is valid.
 * Piece names are single letters from "a" to "z" or "@" for the blue piece.
 *)
let valid_piece_name s =
  (String.length s = 1) 
  && (let c = s.[0] in (c = '@') || (c >= 'a' && c <= 'z'))

(* Return true if a piece length is either 2 or 3. *)
let valid_piece_length n = n = 2 || n = 3

(*
 * Return a list of all keys in a hash table.
 *)
let hash_keys table =
  let result = ref [] in
  begin
    Hashtbl.iter (fun key _ -> result := key :: !result) table;
    !result
  end

(*
 * Drop n items from the front of a list.
 *)
let rec drop n lst =
  match (n, lst) with
    | _ when n < 0 -> invalid_arg "drop: negative argument"
    | (0, _) -> lst
    | (m, []) -> failwith "drop: not enough elements to drop"
    | (m, h :: t) -> drop (n - 1) t

(*
 * Return a list of all the elements at the front of a list
 * for which a predicate returns true.
 *)
let takeWhile pred lst =
  let rec iter lst result =
    match lst with
      | h :: t when pred h -> iter t (h :: result)
      | _ -> result
  in List.rev (iter lst [])

(*
 * Transpose a list of lists, where all sublists are the same length.
 *)
let transpose lol =
  let rec iter seqs =
    if List.hd seqs = []
      then []
      else (List.map List.hd seqs) :: (iter (List.map List.tl seqs))
  in iter lol

(*
 * Make a board list (list of list of one-character strings) from a 
 * single string.  The string contains 6 6-character substrings,
 * separated by spaces.
 *)
let make_board_list_from_string s =
  let string_to_list t =
    assert (String.length t = 6);
    List.map (fun i -> String.make 1 t.[i]) [0; 1; 2; 3; 4; 5]
  in
  assert (String.length s = 41);  (* 36 characters and 5 spaces *)
  assert (s.[6]  = ' ');
  assert (s.[13] = ' ');
  assert (s.[20] = ' ');
  assert (s.[27] = ' ');
  assert (s.[34] = ' ');
  let s1 = String.sub s 0 6 in
  let s2 = String.sub s 7 6 in
  let s3 = String.sub s 14 6 in
  let s4 = String.sub s 21 6 in
  let s5 = String.sub s 28 6 in
  let s6 = String.sub s 35 6 in
    [string_to_list s1;
     string_to_list s2;
     string_to_list s3;
     string_to_list s4;
     string_to_list s5;
     string_to_list s6]

