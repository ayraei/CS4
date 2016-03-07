(* Tests for lab7.ml *)

open OUnit2
open Lab7

(* Convert On/Off values to integers. *)
let on_off_to_int = function
  | On  -> 1
  | Off -> 0

(* Get all cells from a game object as a list of lists. *)
let get_all_cells game =
  let row game r =
    List.map (fun c -> on_off_to_int (game#peek r c)#state) [0; 1; 2; 3; 4]
  in
    List.map (row game) [0; 1; 2; 3; 4]

let init1 = 
  [| 
     [| On;  On;  On;  Off; Off |];
     [| Off; Off; Off; On;  Off |];
     [| On;  On;  Off; On;  On  |];
     [| Off; Off; Off; On;  Off |];
     [| On;  On;  On;  Off; Off |]
  |]

(* Sample light objects. *)
let l1 = make_light ()
let l2 = make_light ()
let l3 = make_light ()
let l4 = make_light ()

(* Sample board object. *)
let b1 = make_board ()

(* Sample game object. *)
let game1 = make_game ()

let all_tests = "all" >:::
[ 
  "Light tests" >:: (fun c ->
     assert_equal l1#state Off;
     assert_equal l2#state Off;
     assert_equal l3#state Off;
     assert_equal l4#state Off;

     l1#toggle;
     l2#toggle;
     assert_equal l1#state On;
     assert_equal l2#state On;
     assert_equal l3#state Off;
     assert_equal l4#state Off;

     l1#set_state Off;
     l3#set_state On;
     assert_equal l1#state Off;
     assert_equal l2#state On;
     assert_equal l3#state On;
     assert_equal l4#state Off;

     l1#add_neighbor l2;
     l1#add_neighbor l3;
     l1#add_neighbor l4;

     l1#toggle;
     assert_equal l1#state On;
     assert_equal l2#state On;
     assert_equal l3#state On;
     assert_equal l4#state Off;

     l1#update;
     assert_equal l1#state Off;
     assert_equal l2#state Off;
     assert_equal l3#state Off;
     assert_equal l4#state On;
  );

  "Board tests" >:: (fun c ->
     assert_equal (b1 0 0)#state Off;
     assert_equal (b1 4 0)#state Off;
     assert_equal (b1 0 4)#state Off;
     assert_equal (b1 4 4)#state Off;
     (b1 0 0)#set_state On;
     assert_equal (b1 0 0)#state On;
     assert_equal (b1 4 0)#state Off;  (* check for aliasing *)
     assert_equal (b1 0 4)#state Off;
     assert_equal (b1 4 4)#state Off;
  );

  "Game tests" >:: (fun c ->
     game1#init init1;
     assert_equal (game1#peek 0 0)#state On;
     assert_equal (game1#peek 0 4)#state Off;
     assert_equal (game1#peek 4 0)#state On;
     assert_equal (game1#peek 4 4)#state Off;
     
     (* Run an example game. *)

     assert_equal (get_all_cells game1)
       [[1;1;1;0;0];[0;0;0;1;0];[1;1;0;1;1];[0;0;0;1;0];[1;1;1;0;0]];

     game1#play 1 0;
     assert_equal (get_all_cells game1) 
        [[0;1;1;0;0];[1;1;0;1;0];[0;1;0;1;1];[0;0;0;1;0];[1;1;1;0;0]];

     game1#play 1 1;
     assert_equal (get_all_cells game1) 
       [[0;0;1;0;0];[0;0;1;1;0];[0;0;0;1;1];[0;0;0;1;0];[1;1;1;0;0]];

     game1#play 1 2;
     assert_equal (get_all_cells game1) 
       [[0;0;0;0;0];[0;1;0;0;0];[0;0;1;1;1];[0;0;0;1;0];[1;1;1;0;0]];

     game1#play 2 1;
     assert_equal (get_all_cells game1) 
       [[0;0;0;0;0];[0;0;0;0;0];[1;1;0;1;1];[0;1;0;1;0];[1;1;1;0;0]];

     game1#play 3 0;
     assert_equal (get_all_cells game1) 
       [[0;0;0;0;0];[0;0;0;0;0];[0;1;0;1;1];[1;0;0;1;0];[0;1;1;0;0]];

     game1#play 3 1;
     assert_equal (get_all_cells game1) 
       [[0;0;0;0;0];[0;0;0;0;0];[0;0;0;1;1];[0;1;1;1;0];[0;0;1;0;0]];

     game1#play 3 3;
     assert_equal (get_all_cells game1) 
       [[0;0;0;0;0];[0;0;0;0;0];[0;0;0;0;1];[0;1;0;0;1];[0;0;1;1;0]];

     game1#play 3 4;
     assert_equal (get_all_cells game1) 
       [[0;0;0;0;0];[0;0;0;0;0];[0;0;0;0;0];[0;1;0;1;0];[0;0;1;1;1]];

     game1#play 4 1;
     assert_equal (get_all_cells game1) 
       [[0;0;0;0;0];[0;0;0;0;0];[0;0;0;0;0];[0;0;0;1;0];[1;1;0;1;1]];

     game1#play 4 3;
     assert_equal (get_all_cells game1) 
       [[0;0;0;0;0];[0;0;0;0;0];[0;0;0;0;0];[0;0;0;0;0];[1;1;1;0;0]];

     game1#play 0 1;
     assert_equal (get_all_cells game1) 
       [[1;1;1;0;0];[0;1;0;0;0];[0;0;0;0;0];[0;0;0;0;0];[1;1;1;0;0]];

     game1#play 1 0;
     assert_equal (get_all_cells game1) 
       [[0;1;1;0;0];[1;0;0;0;0];[1;0;0;0;0];[0;0;0;0;0];[1;1;1;0;0]];

     game1#play 1 1;
     assert_equal (get_all_cells game1) 
       [[0;0;1;0;0];[0;1;1;0;0];[1;1;0;0;0];[0;0;0;0;0];[1;1;1;0;0]];

     game1#play 1 2;
     assert_equal (get_all_cells game1) 
       [[0;0;0;0;0];[0;0;0;1;0];[1;1;1;0;0];[0;0;0;0;0];[1;1;1;0;0]];

     game1#play 2 3;
     assert_equal (get_all_cells game1) 
       [[0;0;0;0;0];[0;0;0;0;0];[1;1;0;1;1];[0;0;0;1;0];[1;1;1;0;0]];

     game1#play 3 0;
     assert_equal (get_all_cells game1) 
       [[0;0;0;0;0];[0;0;0;0;0];[0;1;0;1;1];[1;1;0;1;0];[0;1;1;0;0]];

     game1#play 3 1;
     assert_equal (get_all_cells game1) 
       [[0;0;0;0;0];[0;0;0;0;0];[0;0;0;1;1];[0;0;1;1;0];[0;0;1;0;0]];

     game1#play 3 3;
     assert_equal (get_all_cells game1) 
       [[0;0;0;0;0];[0;0;0;0;0];[0;0;0;0;1];[0;0;0;0;1];[0;0;1;1;0]];

     game1#play 3 4;
     assert_equal (get_all_cells game1) 
       [[0;0;0;0;0];[0;0;0;0;0];[0;0;0;0;0];[0;0;0;1;0];[0;0;1;1;1]];

     game1#play 4 3;
     assert_equal (get_all_cells game1) 
       [[0;0;0;0;0];[0;0;0;0;0];[0;0;0;0;0];[0;0;0;0;0];[0;0;0;0;0]];

  );

]

(* TODO: Add tests for all error conditions! *)

let _ = run_test_tt_main all_tests

