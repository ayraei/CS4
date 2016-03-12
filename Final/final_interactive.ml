open Final_helpers
open Final
open Printf
open Str

(* ---------------------------------------------------------
 * Playing Rush Hour interactively.
 * --------------------------------------------------------- *)

let play_rush_hour init_list =
  let valid_read name dist legal_moves =
    try
      let n = int_of_string dist in
        List.mem (name, n) legal_moves
    with _ -> false
  in

  let rec run board =
    let moves = board#get_all_piece_moves in
    begin
      printf "Possible moves:\n";
      List.iter (fun (p, d) -> printf "  (%s, %d)\n" p d) moves;
      print_newline ();
      printf "Enter move: ";
      let line = read_line () in
      let words = Str.split (regexp "[ \t]+") line in
      let len = List.length words in
        match () with
          | _ when len = 1 && List.hd words = "quit" -> ()
          | _ when len = 2 
                && valid_read (List.nth words 0) (List.nth words 1) moves ->
                let name = List.nth words 0 in
                let dist = int_of_string (List.nth words 1) in
                  begin
                    board#move_piece name dist;
                    board#print;
                    if board#won_game
                      then printf "\nYOU WIN!!!\n\n"
                      else run board
                  end
          | _ ->
                begin
                  printf "Invalid input; please try again!\n";
                  run board
                end
    end
  in

  let board = make_board () in
    begin
      board#initialize_from_list init_list;
      board#print;
      run board
    end

(* Some practice boards. *)

let very_easy_board =
  make_board_list_from_string "abbcc_ addeff @@gehi j_gkhi j__k__ jllk__"

let easy_board =
  make_board_list_from_string "_abbcd _aefcd @@ef__ __gh__ __ghii ______"

let medium_board =
  make_board_list_from_string "aaab__ cd_b_e cd@@_e fgg__e fhh___ fiiijj"

let hard_board =
  make_board_list_from_string "aaabbc ddd_ec @@__ef gg__ef h_ijkk h_ij__"

(* The hardest possible Rush Hour puzzle: 93 moves to solve. *)
let hardest_board =
  make_board_list_from_string "aaabcd effbcd e_@@cd ggh___ _ih_jj _ikkll"

(* Uncomment whichever one you want to play. *)

let _ = play_rush_hour very_easy_board

(* let _ = play_rush_hour easy_board *)
(* let _ = play_rush_hour medium_board *)
(* let _ = play_rush_hour hard_board *)
(* let _ = play_rush_hour hardest_board *)

