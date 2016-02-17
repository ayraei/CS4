(* midterm.mli: interface file for the CS 4 midterm exam, 2016 *)

(* 3.1 *)
val find_smallest : int list -> int * int
val replace : int -> int -> int list -> int list
val swap_smallest_with_first : int list -> int list
val minimum_element_sort : int list -> int list

(* 3.2 *)
val flip : int list -> int -> int -> int list
val flip_rubik_1d : int list -> int -> int list
val rubik_1d : int list -> int list -> int list

(* 3.3 *)
val binary_to_integer : int list -> int
val bxor : int -> int -> int
val band : int -> int -> int
val bor : int -> int -> int
val add_digits : int -> int -> int -> int * int
val badd : int list -> int list -> int list

(* 4.1 *)
val unfold : ('a -> ('b * 'a) option) -> 'a -> 'b list
val ascending_list : int list
val all_suffixes : 'a list -> 'a list list

(* 4.2 *)
val mapReduce1_rec : 
  ('a -> 'b) -> ('b -> 'c -> 'c) -> 'c -> 'a list -> 'c
val mapReduce1_iter : 
  ('a -> 'b) -> ('c -> 'b -> 'c) -> 'c -> 'a list -> 'c

(* 4.3 *)
type 'a tree = Leaf of 'a | Branch of 'a tree * 'a tree
val mapReduce_tree : ('a -> 'b) -> ('b -> 'b -> 'b) -> 'a tree -> 'b
val mapReduceN :
  ('a list -> 'b) -> ('b -> 'c -> 'c) -> 'c -> 'a list list -> 'c
