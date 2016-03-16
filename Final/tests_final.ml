(* Tests for CS 4 final exam, 2016 *)

open OUnit2
open Final_helpers
open Final

(* 
 * Return true if two lists have the same elements, even if not
 * in the same order. 
 *)
let same_lists lst1 lst2 =
  let lst1' = List.sort compare lst1 in
  let lst2' = List.sort compare lst2 in
    lst1' = lst2'

let assert_equal_lists ~msg:m lst1 lst2 =
  assert_bool m (same_lists lst1 lst2)

let assert_equal_list_options ~msg:m maybe_lst1 maybe_lst2 =
  match (maybe_lst1, maybe_lst2) with
    | (None, None) -> assert_bool m true
    | (None, _)    -> assert_bool m false
    | (_, None)    -> assert_bool m false
    | (Some lst1, Some lst2) -> assert_bool m (same_lists lst1 lst2)

(* Specialized assert_equal for the result of the if_move method. *) 
let assert_equal_if_move ~msg:m move_result1 move_result2 =
  match (move_result1, move_result2) with
    | (None, None) -> assert_bool m true
    | (None, _)    -> assert_bool m false
    | (_, None)    -> assert_bool m false
    | (Some (lst1a, lst1b, lst1c), Some (lst2a, lst2b, lst2c)) ->
        assert_bool m 
          (same_lists lst1a lst2a 
           && same_lists lst1b lst2b 
           && same_lists lst1c lst2c)

let p1 = make_piece "a" P2 H
let p2 = make_piece "b" P3 H
let p3 = make_piece "c" P2 V
let p4 = make_piece "d" P3 V
(*
let b = make_board ()
let pieces board = board#pieces_table
let piece_at_loc board = board#piece_at_location_table
let make_board_list = make_board_list_from_string
*)
let all_tests = "all" >:::
[ 
  "Piece tests" >:: (fun c ->
     assert_equal ~msg: "piece test 1a" p1#name "a";
     assert_equal ~msg: "piece test 1b" p1#len P2;
     assert_equal ~msg: "piece test 1c" p1#dir H;
     assert_equal_lists ~msg: "piece test 1d" 
       p1#locs [(0, 0); (0, 1)];

     assert_equal ~msg: "piece test 2a" p2#name "b";
     assert_equal ~msg: "piece test 2b" p2#len P3;
     assert_equal ~msg: "piece test 2c" p2#dir H;
     assert_equal_lists ~msg: "piece test 2d" 
       p2#locs [(0, 0); (0, 1); (0, 2)];

     assert_equal ~msg: "piece test 3a" p3#name "c";
     assert_equal ~msg: "piece test 3b" p3#len P2;
     assert_equal ~msg: "piece test 3c" p3#dir V;
     assert_equal_lists ~msg: "piece test 3d" 
       p3#locs [(0, 0); (1, 0)];

     assert_equal ~msg: "piece test 4a" p4#name "d";
     assert_equal ~msg: "piece test 4b" p4#len P3;
     assert_equal ~msg: "piece test 4c" p4#dir V;
     assert_equal_lists ~msg: "piece test 4d" 
       p4#locs [(0, 0); (1, 0); (2, 0)];

     assert_equal_list_options ~msg: "piece test 5a" 
       (p1#if_place (0, 0)) (Some [(0, 0); (0, 1)]);
     assert_equal_list_options ~msg: "piece test 5b" 
       (p1#if_place (1, 2)) (Some [(1, 2); (1, 3)]);
     assert_equal_list_options ~msg: "piece test 5c" 
       (p1#if_place (5, 4)) (Some [(5, 4); (5, 5)]);
     assert_equal_list_options ~msg: "piece test 5d" 
       (p1#if_place (5, 5)) None;

     assert_equal ~msg: "piece test 6a" (p1#place (5, 4)) ();
     assert_equal_lists ~msg: "piece test 6b" 
       p1#locs [(5, 4); (5, 5)];

     assert_equal_if_move ~msg: "piece test 7" 
       (p1#if_move (-3))
       (Some ([(5, 4); (5, 5)], [(5, 3)], [(5, 1); (5, 2)]));

     assert_equal_if_move ~msg: "piece test 8a" 
       (p1#if_move 1) None;   (* invalid move *)
     assert_equal_lists ~msg: "piece test 8b"
       p1#possible_moves [-4; -3; -2; -1];

     assert_equal ~msg: "piece test 9a" (p1#move (-3)) ();
     assert_equal_lists ~msg: "piece test 9b"
       p1#locs [(5, 1); (5, 2)];

     assert_equal_list_options ~msg: "piece test 10a"
       (p2#if_place (3, 1)) (Some [(3, 1); (3, 2); (3, 3)]);
     assert_equal ~msg: "piece test 10b" (p2#place (3, 1)) ();
     assert_equal_lists ~msg: "piece test 10c"
       p2#locs [(3, 1); (3, 2); (3, 3)];
     assert_equal_if_move ~msg: "piece test 10d"
       (p2#if_move 3) None;
     assert_equal_lists ~msg: "piece test 10e"
       p2#possible_moves [-1; 1; 2];
     assert_equal_if_move ~msg: "piece test 10f"
       (p2#if_move 1) (Some ([(3, 1)], [], [(3, 4)]));

     assert_equal_list_options ~msg: "piece test 11a"
       (p4#if_place (0, 0)) (Some [(0, 0); (1, 0); (2, 0)]);
     assert_equal_list_options ~msg: "piece test 11b"
       (p4#if_place (1, 2)) (Some [(1, 2); (2, 2); (3, 2)]);
     assert_equal_list_options ~msg: "piece test 11c"
       (p4#if_place (3, 5)) (Some [(3, 5); (4, 5); (5, 5)]);
     assert_equal_list_options ~msg: "piece test 11d"
       (p4#if_place (5, 2)) None;
  );
(*
  "Board tests" >:: (fun c ->
     assert_equal ~msg: "board test 1a" (pieces b) (Hashtbl.create 25);
     assert_equal ~msg: "board test 1b" (piece_at_loc b) (Hashtbl.create 36);

     assert_equal ~msg: "board test 2" b#to_list
       (make_board_list "______ ______ ______ ______ ______ ______");

     b#place_piece "a" P3 H (0, 0);
     b#place_piece "b" P2 V (0, 3);
     assert_bool "board test 3a" (Hashtbl.mem (pieces b) "a");
     assert_bool "board test 3b" (Hashtbl.mem (pieces b) "b");

     let p1 = Hashtbl.find (pieces b) "a" in
       begin
         assert_equal ~msg: "board test 4a" p1#name "a";
         assert_equal ~msg: "board test 4b" p1#len P3;
         assert_equal ~msg: "board test 4c" p1#dir H;
         assert_equal_lists ~msg: "board test 4d"
           p1#locs [(0, 0); (0, 1); (0, 2)]
       end;

    let p2 = Hashtbl.find (pieces b) "b" in
       begin
         assert_equal ~msg: "board test 5a" p2#name "b";
         assert_equal ~msg: "board test 5b" p2#len P2;
         assert_equal ~msg: "board test 5c" p2#dir V;
         assert_equal_lists ~msg: "board test 5d"
           p2#locs [(0, 3); (1, 3)]
       end;

    assert_equal ~msg: "board tests 6a" 
      (Hashtbl.find (piece_at_loc b) (0, 0)) "a";
    assert_equal ~msg: "board tests 6b" 
      (Hashtbl.find (piece_at_loc b) (0, 1)) "a";
    assert_equal ~msg: "board tests 6c" 
      (Hashtbl.find (piece_at_loc b) (0, 2)) "a";
    assert_equal ~msg: "board tests 6d" 
      (Hashtbl.find (piece_at_loc b) (0, 3)) "b";
    assert_equal ~msg: "board tests 6e" 
      (Hashtbl.find (piece_at_loc b) (1, 3)) "b";

    b#place_piece "@" P2 H (2, 2);
    assert_bool "board tests 7a" (Hashtbl.mem (pieces b) "@");
    let blue_piece = Hashtbl.find (pieces b) "@" in
      begin
        assert_equal ~msg: "board tests 7b" blue_piece#name "@";
        assert_equal ~msg: "board tests 7c" blue_piece#len P2;
        assert_equal ~msg: "board tests 7d" blue_piece#dir H;
        assert_equal_lists ~msg: "board tests 7e" blue_piece#locs
          [(2, 2); (2, 3)]
      end;

    assert_equal ~msg: "board tests 8a" 
      (Hashtbl.find (piece_at_loc b) (0, 0)) "a";
    assert_equal ~msg: "board tests 8b" 
      (Hashtbl.find (piece_at_loc b) (0, 1)) "a";
    assert_equal ~msg: "board tests 8c" 
      (Hashtbl.find (piece_at_loc b) (0, 2)) "a";
    assert_equal ~msg: "board tests 8d" 
      (Hashtbl.find (piece_at_loc b) (0, 3)) "b";
    assert_equal ~msg: "board tests 8e" 
      (Hashtbl.find (piece_at_loc b) (1, 3)) "b";
    assert_equal ~msg: "board tests 8f" 
      (Hashtbl.find (piece_at_loc b) (2, 2)) "@";
    assert_equal ~msg: "board tests 8g" 
      (Hashtbl.find (piece_at_loc b) (2, 3)) "@";

    assert_equal ~msg: "board tests 9a" b#to_list
      (make_board_list "aaab__ ___b__ __@@__ ______ ______ ______");
    b#move_piece "@" 1;
    assert_equal ~msg: "board tests 9b" b#to_list
      (make_board_list "aaab__ ___b__ ___@@_ ______ ______ ______");

    (* These tests should raise exceptions (invalid moves). *)
    assert_bool "board tests 10a"
      (try b#move_piece "a" 1; false with _ -> true);
    assert_bool "board tests 10b"
      (try b#move_piece "b" 1; false with _ -> true);
    assert_bool "board tests 10c"
      (try b#move_piece "b" (-1); false with _ -> true);

    b#move_piece "@" (-3);
    assert_equal ~msg: "board tests 11a" b#to_list
      (make_board_list "aaab__ ___b__ @@____ ______ ______ ______");
    b#move_piece "b" 4;
    assert_equal ~msg: "board tests 11b" b#to_list
      (make_board_list "aaa___ ______ @@____ ______ ___b__ ___b__");
    b#move_piece "a" 3;
    assert_equal ~msg: "board tests 11c" b#to_list
      (make_board_list "___aaa ______ @@____ ______ ___b__ ___b__");
    assert_bool "board tests 11d"
      (try b#move_piece "b" (-4); false with _ -> true);
    b#move_piece "b" (-3);
    assert_equal ~msg: "board tests 11e" b#to_list
      (make_board_list "___aaa ___b__ @@_b__ ______ ______ ______");

    assert_equal_lists ~msg: "board tests 12a" b#get_all_piece_moves
      [("a", -3); ("a", -2); ("a", -1); 
       ("@", 1); 
       ("b", 1); ("b", 2); ("b", 3)];
    assert_equal ~msg: "board tests 12b" b#won_game false;

    b#clear;
    assert_equal ~msg: "board tests 13a" (pieces b) (Hashtbl.create 25);
    assert_equal ~msg: "board tests 13b" (piece_at_loc b) (Hashtbl.create 36);
    assert_equal ~msg: "board tests 13c" b#to_list
      (make_board_list "______ ______ ______ ______ ______ ______");
    assert_equal_lists ~msg: "board tests 13d" b#get_all_piece_moves [];

    b#place_piece "@" P2 H (2, 4);
    assert_bool "board tests 14" b#won_game;

    let hardest_board =
      make_board_list "aaabcd effbcd e_@@cd ggh___ _ih_jj _ikkll"
    in
      begin
        b#initialize_from_list hardest_board;
        assert_equal ~msg: "board tests 15a" b#to_list hardest_board;
        assert_equal_lists ~msg: "board tests 15b" b#get_all_piece_moves
          [("c", 1); ("j", -1); ("d", 1); ("@", -1)]
      end;

    let won_board =
      make_board_list "aaabcd effbcd e___@@ ggh___ _ih_jj _ikkll"
    in
      begin
        b#initialize_from_list won_board;
        assert_bool "board tests 16" b#won_game
      end

  );
*)
]

let _ = run_test_tt_main all_tests

