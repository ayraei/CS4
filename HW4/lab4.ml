(* PART A *)

(* Question 1 *)

type mobile = Mobile of branch * branch  (* left and right branches *)
and branch =
  | Weight    of int * int     (* length and weight *)
  | Structure of int * mobile  (* length and sub-mobile *)

let make_mobile l r = Mobile (l, r)
let make_weight l w = Weight (l, w)
let make_structure l m = Structure (l, m)

(* part a *)

(* accessor that returns the left branch of a mobile *)
let left_branch (l, _) = l

(* accessor that returns the right branch of a mobile *)
let right_branch (_, r) = r

(* return a branch's length *)
let branch_length b = function
  | Weight (l, _) -> l
  | Structure (l, _) -> l

(* return a branch's structure *)
let branch_structure = function
  | Weight (_, w) -> `Weight w
  | Structure (_, s) -> `Structure s

(* part b *)

(* branch_weight1: returns the weight of a branch *)
(* total_weight1: returns the total weight of a mobile *)
let rec branch_weight1 = function
  | Weight (_, w) -> w
  | Structure (_, s) -> total_weight1 s
and total_weight1 = function
  | Mobile (b1, b2) -> (branch_weight1 b1) + (branch_weight1 b2)

(* branch_weight2: returns the weight of a branch *)
(* total_weight2: returns the total weight of a mobile *)
let rec branch_weight2 b =
  match branch_structure b with
    | `Weight w -> w
    | `Structure s -> total_weight2 s
and total_weight2 m =
  (branch_weight2 (left_branch m)) + 
  (branch_weight2 (right_branch m))

(* part c *)

(* returns true if a mobile is balanced. A mobile is balanced if the torque
 * applied by its top-left branch is equal to that applied by its top-right
 * branch, and all its submobiles are balanced.
 *)
let rec is_balanced m =
  let pred =
    (
     (branch_length left_branch m) * (branch_weight2 left_branch m) =
     (branch_length right_branch m) * (branch_weight2 right_branch m)
    ) in
  match (get_structure left_branch m, get_structure right_branch m) with
    | (`Weight _, `Weight _) -> pred
    | (`Weight _, `Structure _) -> pred && is_balanced right_branch m
    | (`Structure _, `Weight _) -> pred && is_balanced left_branch m
    | (`Structure _, `Structure _) -> pred && is_balanced left_branch m &&
        is_balanced right_branch m

(* part d *)
type mobile'  = { left: branch'; right: branch' }
and  branch'  = Branch' of int * contents
and  contents = Weight' of int | Structure' of mobile'

(* primed accessors *)

let make_mobile' l r = { left = l; right = r }
let make_weight' l w = Branch' (l, Weight' w)
let make_structure' l m = Branch' (l, Structure' m)

(* accessor that returns the left branch of a mobile *)
let left_branch' { left; _ } = left

(* accessor that returns the right branch of a mobile *)
let right_branch' { _; right } = right

(* return a branch's length *)
let branch_length' (l, _) = l

(* return a branch's structure *)
let branch_structure' (_, c) = c

(* TODO:
 * branch_weight'
 * total_weight'
 * is_balanced'
 * these should all be trivial to modify from part c
 *)

(* Question 2 *)

type tree = Tree of elem list
and elem =
  | Num of int
  | Sub of tree

(* makes a copy of a tree, squaring all the numbers in it. *)
let rec square_tree = function
  | [] -> []
  | Num of h :: t -> Num of (h * h) :: square_tree t
  | Sub of h :: t -> square_tree h :: square_tree t

(* makes a copy of a tree, squaring all the numbers in it. *)
let square_tree' t =
  let rec helper e =
    match e with
      | [] -> []
      | Num -> e * e
      | Sub s -> List.map helper s
  in List.map helper t

(* Question 3 *)

(* map a function to a tree *)
let tree map func tr =
  let rec helper e =
    match e with
      | [] -> []
      | Num e -> func e
      | Sub s -> List.map helper s
  in List.map helper tr

let square_tree'' tree = tree_map (fun n -> n * n) tree

(* Question 4 *)

let rec subsets = function
  | [] -> [[]]
  | h :: t -> let rest = subsets t in
      rest @ (List.map (fun elem -> h :: elem) rest)

(* How it works:
 * The base case is an empty list, for which all possible subsets are just
 * the empty list.
 * If the list has one or more elements, then we set aside the first
 * element (h), and find all possible subsets of the remaining list (rest).
 * Then, we append those results with the list of h cons'd with each element
 * in rest, which gives us every possible subset combined with the h element.
 * This process thus recursively returns all possible subsets of the original
 * list.
 *)

(* Question 5 *)

let rec accumulate op initial sequence =
  match sequence with
    | [] -> initial
    | h :: t -> op h (accumulate op initial t)

let map p sequence =
  accumulate (fun x r -> <??>) [] sequence

let append seq1 seq2 =
  accumulate (fun x r -> x :: r) <??> <??>

let length sequence =
  accumulate <??> 0 sequence

(* Question 6 *)

let rec accumulate_n op init seqs =
  match seqs with
    | [] -> failwith "empty list"
    | [] :: _ -> []
    | h :: t -> accumulate op init <??> :: accumulate_n op init <??>

(* Question 7 *)

let rec map2 f x y =
  match (x, y) with
    | ([], []) -> []
    | ([], _) -> failwith "unequal lists"
    | (_, []) -> failwith "unequal lists"

let matrix_times_vector m v = map <??> m

let transpose mat = accumulate_n <??> <??> mat

let matrix_times_matrix m n =
  let cols = transpose n in
    map <??> m

(* PART B *)

(* Question 1 *)

let rec filter predicate sequence =
  match sequence with
    | [] -> []
    | h :: t when predicate h -> h :: filter predicate t
    | _ :: t -> filter predicate t

(* Sorts a list of integers in ascending order, returning the sorted list. *)
let rec quicksort l cmp =
  match l with
    | [] -> []
    | pivot :: t ->
        quicksort (filter (fun x -> cmp x pivot) t) cmp @ [pivot] @
        quicksort (filter (fun x -> not (cmp x pivot)) t) cmp

(* Question 2 *)
(* Quicksort is an instance of generative recursion because the recursive calls
 * are being made on sublists (less than/greater than pivot) defined within
 * the function instead of making recursive calls on sections of the original
 * list.
 *)

(* Question 3 *)
(* Running Ben's code results in a "stack overflow (looping recursion?)" error.
 * The case in Ben's merge_sort of a 1-element list will result in an
 * even_half list that is empty, and an odd_half list that is one element long.
 * However, merge_sort gets recursively called on these two sublists,
 * so the 1-element list will create an infinite loop, because it is impossible
 * to reduce the 1-element list to two empty lists.
 *)

(* Question 4 *)

let rec insert_in_order new_result a_list cmp =
  match a_list with
    | [] -> [new_result]
    | h :: t when cmp new_result h -> new_result :: a_list
    | h :: t -> h :: insert_in_order new_result t cmp

let rec insertion_sort a_list cmp =
  match a_list with
    | [] -> []
    | h :: t -> insert_in_order h (insertion_sort t) cmp

(* This is an example of structural recursion, since we are not generating
 * any new subsets of the original data, but merely recursively calling
 * the function on portions of the original input.
 *)

(* PART C *)

(* Question 1 *)

type expr =
  | Int of int            (* constant *)
  | Var of string         (* variable *)
  | Add of expr * expr    (* expr1 + expr2 *)
  | Mul of expr * expr    (* expr 1 * expr2 *)
  | Pow of expr * int     (* expr^n *)

(* From set 3. *)
(* Takes two int arguments x, y and returns x^y *)
let rec pow x y =
    if y = 0
    then 1
    else x * pow x (y - 1)

(* Performs algebraic simplification on an expr expression. *)
let rec simplify1 = function
  | Add of (Int a, Int b) -> a + b
  | Mul of (Int a, Int b) -> a * b
  | Pow of (Int a, Int b) -> pow a b
  | Add of (Int of 0, expr)
  | Add of (expr, Int of 0) -> expr
  | Mul of (Int of 0, expr)
  | Mul of (expr, Int of 0) -> 0
  | Mul of (Int of 1, expr)
  | Mul of (expr, Int of 1) -> expr
  | Pow of (_, 0) -> 1
  | Add of (expr1, expr2) -> Add of (simplify1 expr1, simplify1 expr2)

let rec simplify expr =
  let e = simplify1 expr in
    if expr = e
      then expr
      else simplify e

(* Question 2 *)

(* Takes the derivative of a given expr expression. *)
let deriv expr =
    (* TODO *)
    5

let derivative expr var =
  let e = simplify expr in
  let d = deriv e var in
    simplify d
