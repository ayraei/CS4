(* Tests for midterm.ml *)

open OUnit2
open Midterm

let to_false _ = false

(* Utility functions for tests. *)
let descending i = if i = 0 then None else Some (i, (i - 1))
let halving i = if i = 0 then None else Some (i, i/2)
let sum lst = List.fold_right ( + ) lst 0
let product lst = List.fold_right ( * ) lst 1

let all_tests = "all" >:::
[ 
  "Problem 3.1: find_smallest" >:: (fun c ->
    assert_bool  "find_smallest error" 
      (try to_false (find_smallest []) with Failure _ -> true);
    assert_equal (find_smallest [1;2;3;4;5]) (1, 0);
    assert_equal (find_smallest [5;4;3;2;1;2;3;4;5]) (1, 4);
    assert_equal (find_smallest [5;1;4;1;3;1;2;1;1]) (1, 1)
  );

  "Problem 3.1: replace" >:: (fun c ->
    assert_equal (replace 0 42 [1;2;3;4;5]) [42;2;3;4;5];
    assert_equal (replace 0 42 [1;1;1;1;1]) [42;1;1;1;1];
    assert_equal (replace 1 42 [1;2;3;4;5]) [1;42;3;4;5];
    assert_equal (replace 2 42 [1;2;3;4;5]) [1;2;42;4;5];
    assert_equal (replace 3 42 [1;2;3;4;5]) [1;2;3;42;5];
    assert_equal (replace 4 42 [1;2;3;4;5]) [1;2;3;4;42];
    assert_bool "replace error"
      (try to_false (replace 5 42 []) with Failure _ -> true);
    assert_bool "replace error" 
      (try to_false (replace 5 42 [1;2;3;4;5]) with Failure _ -> true)
  );

  "Problem 3.1: swap_smallest_with_first" >:: (fun c ->
    assert_equal (swap_smallest_with_first [1]) [1];
    assert_equal (swap_smallest_with_first [1;2;3;4;5]) [1;2;3;4;5];
    assert_equal (swap_smallest_with_first [5;3;1;2;4]) [1;3;5;2;4];
    assert_equal (swap_smallest_with_first [5;4;3;2;1]) [1;4;3;2;5];
    assert_bool "swap_smallest_with_first error"
      (try to_false (swap_smallest_with_first []) with Failure _ -> true)
  );

  "Problem 3.1: minimum_element_sort" >:: (fun c ->
    assert_equal (minimum_element_sort []) [];
    assert_equal (minimum_element_sort [1]) [1];
    assert_equal (minimum_element_sort [1;2;3;4;5]) [1;2;3;4;5];
    assert_equal (minimum_element_sort [5;4;3;2;1]) [1;2;3;4;5];
    assert_equal (minimum_element_sort [1;2;3;4;5;1;2;3;4;5]) [1;1;2;2;3;3;4;4;5;5];
    assert_equal (minimum_element_sort [5;4;3;2;1;2;3;4;5]) [1;2;2;3;3;4;4;5;5]
  );

  "Problem 3.2: flip" >:: (fun c ->
    assert_equal (flip [] 0 0) [];
    assert_equal (flip [1] 0 1) [1];
    assert_equal (flip [1;2;3;4;5;6] 0 4) [4;3;2;1;5;6];
    assert_equal (flip [1;2;3;4;5;6] 1 4) [1;5;4;3;2;6];
    assert_equal (flip [1;2;3;4;5;6] 2 4) [1;2;6;5;4;3];
    assert_equal (flip [1;1;2;2;3;3;4;4;5;5;6;6] 3 7) [1;1;2;5;5;4;4;3;3;2;6;6];
    assert_bool "flip error 1"
      (try to_false (flip [] 1 1) with Failure _ -> true);
    assert_bool "flip error 2"
      (try to_false (flip [1;2;3;4;5;6] 3 4) with Failure _ -> true);
    assert_bool "flip error 3"
      (try to_false (flip [1;2;3;4;5;6] 2 5) with Failure _ -> true);
    assert_bool "flip error 4"
      (try to_false (flip [1;2;3;4;5;6] 1 6) with Failure _ -> true);
    assert_bool "flip error 5"
      (try to_false (flip [1;2;3;4;5;6] 0 8) with Failure _ -> true)
  );

  "Problem 3.2: flip_rubik_1d" >:: (fun c ->
    assert_equal (flip_rubik_1d [1;2;3;4;5;6] 0) [4;3;2;1;5;6];
    assert_equal (flip_rubik_1d [1;2;3;4;5;6] 1) [1;5;4;3;2;6];
    assert_equal (flip_rubik_1d [1;2;3;4;5;6] 2) [1;2;6;5;4;3];
    assert_bool "flip_rubik_1d error"
      (try to_false (flip_rubik_1d [1;2;3;4;5;6] 3) with Failure _ -> true)
  );

  "Problem 3.2: rubik_1d" >:: (fun c ->
    assert_equal (rubik_1d [1;2;3;4;5;6] []) [1;2;3;4;5;6];
    assert_equal (rubik_1d [2;3;5;1;4;6] []) [2;3;5;1;4;6];
    assert_equal (rubik_1d [1;2;3;4;5;6] [1]) [1;5;4;3;2;6];
    assert_equal (rubik_1d [2;3;5;1;4;6] [2]) [2;3;6;4;1;5];
    assert_equal (rubik_1d [1;2;3;4;5;6] [1;2]) [1;5;6;2;3;4];
    assert_equal (rubik_1d [1;2;3;4;5;6] [0;1;2;1;2;1;0;2;0]) [2;3;5;1;4;6];
    assert_equal (rubik_1d [2;3;5;1;4;6] [0;2;0;1;2;1;2;1;0]) [1;2;3;4;5;6]
  );

  "Problem 3.3: binary_to_integer" >:: (fun c ->
    assert_equal (binary_to_integer []) 0;
    assert_equal (binary_to_integer [0]) 0;
    assert_equal (binary_to_integer [0;0;0;0]) 0;
    assert_equal (binary_to_integer [1;1;1;1]) 15;
    assert_equal (binary_to_integer [0;0;0;0;1]) 16;
    assert_equal (binary_to_integer [0;1;0;0;1]) 18;
    assert_equal (binary_to_integer [0;1;0;0;1;0;0;0;0;0]) 18;
    assert_equal 
      (binary_to_integer [1;0;0;1;0;0;1;1] 
         + binary_to_integer [0;1;0;1;1;0;1;1]) 419
  );

  "Problem 3.3: add_digits" >:: (fun c ->
    assert_equal (add_digits 0 0 0) (0,0);
    assert_equal (add_digits 0 0 1) (1,0);
    assert_equal (add_digits 0 1 0) (1,0);
    assert_equal (add_digits 0 1 1) (0,1);
    assert_equal (add_digits 1 0 0) (1,0);
    assert_equal (add_digits 1 0 1) (0,1);
    assert_equal (add_digits 1 1 0) (0,1);
    assert_equal (add_digits 1 1 1) (1,1)
  );

  "Problem 3.3: badd" >:: (fun c ->
    assert_equal (badd [0;0;0;0] [0;0;0;0]) [0;0;0;0];
    assert_equal (badd [0;0;0;0] [1;1;1;1]) [1;1;1;1];
    assert_equal (badd [1;0;0;1] [0;0;0;0]) [1;0;0;1];
    assert_equal (badd [1;0;0;1] [1;0;0;1]) [0;1;0;0;1];
    assert_bool "badd error"
      (try to_false (badd [1;0;0;1] [1;0;1]) with Failure _ -> true);
    assert_equal 
      (badd [1;0;0;1;0;0;1;1] [0;1;0;1;1;0;1;1]) [1;1;0;0;0;1;0;1;1];
    assert_equal 
      (binary_to_integer (badd [1;0;0;1;0;0;1;1] [0;1;0;1;1;0;1;1])) 419
  );

  "Problem 4.1: unfold" >:: (fun c ->
    assert_equal (unfold descending 10) [10;9;8;7;6;5;4;3;2;1];
    assert_equal (unfold halving 64) [64;32;16;8;4;2;1]
  );
  
  "Problem 4.1: all_suffixes" >:: (fun c ->
    assert_equal (all_suffixes []) [];
    assert_equal (all_suffixes [1]) [[1]];
    assert_equal (all_suffixes [1;2;3;4;5]) 
      [[1;2;3;4;5];[2;3;4;5];[3;4;5];[4;5];[5]]
  );

  "Problem 4.2: mapReduce1_rec" >:: (fun c ->
    assert_equal (mapReduce1_rec (fun n -> 3 + n) ( * ) 42 []) 42;
    assert_equal (mapReduce1_rec (fun n -> 3 + n) ( * ) 1 [1;2;3;4;5]) 6720;
    assert_equal (mapReduce1_rec (fun n -> -n) (+) 0 [1;2;3;4;5]) (-15)
  );

  "Problem 4.2: mapReduce1_iter" >:: (fun c ->
    assert_equal (mapReduce1_iter (fun n -> 3 + n) ( * ) 42 []) 42;
    assert_equal (mapReduce1_iter (fun n -> 3 + n) ( * ) 1 [1;2;3;4;5]) 6720;
    assert_equal (mapReduce1_iter (fun n -> -n) (+) 0 [1;2;3;4;5]) (-15)
  );

  "Problem 4.3: mapReduce_tree" >:: (fun c ->
    assert_equal (mapReduce_tree (fun n -> 5 * n) (+) (Leaf 3)) 15;
    assert_equal (mapReduce_tree (fun n -> n + 2) ( * ) 
      (Branch
        (Branch (Leaf 1, Leaf 2),
         Branch (Leaf 3,
                 Branch 
                   (Branch (Leaf 4, Leaf 5),
                    Branch (Leaf 6,
                            Branch
                              (Branch (Leaf 7, Leaf 8),
                               Leaf 9)))))))
      19958400
  );

  "Problem 4.3: mapReduceN" >:: (fun c ->
    assert_equal (mapReduceN product (+) 42 [[]]) 42;
    assert_equal (mapReduceN product (+) 42 [[];[];[];[];[]]) 42;
    assert_equal (mapReduceN product (+) 0 [[2;3;4];[5;6;7];[8;9;10]]) 
      (2 * 5 * 8 + 3 * 6 * 9 + 4 * 7 * 10);
    assert_equal (mapReduceN sum ( * ) 1 [[2;3;4];[5;6;7];[8;9;10]]) 
      ((2 + 5 + 8) * (3 + 6 + 9) * (4 + 7 + 10));
    assert_bool "mapReduceN error"
      (try to_false (mapReduceN product (+) 42 []) with Failure _ -> true)
  );

]

let _ = run_test_tt_main all_tests

