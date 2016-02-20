(* name: Joanne Li *)
(* login: jli9 *)

(* PART 1 *)

(* Question a *)
(*
desugar let rec f y z acc ... -> let rec f y z acc = fun y z acc -> ...

Given: f 3 4 0
substitute if acc > z then acc else f (y + z) (z + acc) (acc + y) for f
substitute 3 for y
substitute 4 for z
substitute 0 for acc
  -> if 0 > 4 then 0 else f (3 + 4) (4 + 0) (0 + 3)
evaluate if 0 > 4 -> false
evaluate f (3 + 4) (4 + 0) (0 + 3)
    evaluate 3 + 4 -> 7
    evaluate 4 + 0 -> 4
    evaluate 0 + 3 -> 3
    apply f to 7, 4, 3
        substitute 7 for y
        substitute 4 for z
        substitute 3 for acc
          -> if 3 > 4 then 3 else f (7 + 4) (4 + 3) (3 + 7)
        evaluate if 3 > 4 -> false
        evaluate f (7 + 4) (4 + 3) (3 + 7)
            evaluate 7 + 4 -> 11
            evaluate 4 + 3 -> 7
            evaluate 3 + 7 -> 10
            apply f to 11, 7, 10
                substitute 11 for y
                substitute 7 for z
                substitute 10 for acc
                  -> if 10 > 7 then 10 else f (11 + 7) (7 + 10) (10 + 11)
                evaluate if 10 > 7 -> true
                evaluate 10 -> 10
result: 10
*)

(* Question b *)
(*
Given: let x = 7
evaluate x
    Look up value of x
    x has value 7
result: x associated with 7

Given: let y = x * 3
evaluate y
    evaluate x -> 7
    evaluate 3 -> 3
    evaluate * -> *
    apply * to 7, 3 -> 21
result: y associated with 21

Given: (fun x y -> ((fun y -> x * y) x) - ((fun x -> y * x * y) y))
         (x + y)
         (x - y)

evaluate (fun x y -> (...)) // first line
There is lambda shielding on x and y, so we do not substitute x or y for their
earlier values; rather, we use the values found in the arguments.
    evaluate fun x y -> fun x' y' (we will use primes to distinguish x and y)
        substitute x' -> (x + y)
        evaluate (x + y)
            evaluate x -> 7
            evaluate y -> 21
            evaluate + -> +
            apply + to 7, 21 -> 28
        result: x' associated with 28
        substitute y' -> (x - y)
        evaluate (x - y)
            evaluate x -> 7
            evaluate y -> 21
            evaluate - -> -
            apply - to 7, 21 -> (-14)
        result: y' associated with (-14)
        apply fun (the outermost function) to 28, (-14)
            substitute x' into expression where x' is not being lambda-shielded
            substitute y' into expression where y' is not being lambda-shielded
              -> ((fun y -> 28 * y) 28) - ((fun x -> (-14) * x * (-14)) (-14))
                evaluate ((fun y -> 28 * y) 28)
                    substitute 28 for y -> 28 * 28
                    evaluate 28 * 28 -> 784
                evaluate ((fun x -> (-14) * x * (-14)) (-14))
                    substitute (-14) for x -> (-14) * (-14) * (-14)
                    evaluate (-14) * (-14) * (-14) -> (-2744)
                evaluate - -> -
                apply - to 784, (-2744) -> 3528
result -> 3528
*)

(* PART 2 *)

(* Question a *)

(* The asymptotic time complexity for solve_quad is O(1). That is, there is
 * a constant maximum time required to execute this function.
 * We treat sqrt as a constant-time function because it is an Ocaml primitive
 * arithmetic function, in addition to addition, subtraction, multiplication,
 * and division. The function solve_quad is not recursive, so it only runs
 * once to find the solution. The function calls and arithmetic operators
 * are performed a set number of times, which is not dependent on the length
 * of any of the inputs. (For example, solve_quad calls disc exactly once,
 * and disc performs 5 arithmetic operations regardless of a, b, or c.)
 *)

(* Question b *)
(*
let rec f x y acc =
  if x > 0 then f (x - 1) y (acc + x)
  else if y > 0 then f x (y - 1) (acc + y)
  else acc
*)

(* The asymptotic time complexity for f is O(x + y).
 * The function f calls itself recursively as long as x > 0 or y > 0. In each
 * iteration of the function, it makes at most one recursive call to itself, so
 * it is linearly recursive. Each recursive call to itself decreases either
 * x or y by 1 until both reach 0, so the number of times this function will
 * get called varies with x + y. Every other operation that happens is a
 * constant-time arithmetic operation, and both recursive branches of the
 * conditional have 3 of these constant-time operations and one recursive
 * function call, which gives us something like 4(x + y), which still works out
 * to O(x + y).
 *)

(* Question c *)

(* The asymptotic time complexity for bouncy is O(log_2 y).
 * The recursive helper function inside bouncy is a linear iterative process.
 * The function calls itself recursively starting with k = 1 and ending when
 * k > y, where each subsequent call doubles k. It takes log_2 y + 2 iterations
 * for k to exceed (not just equal, which is where the extra +1 comes from) y.
 * All the other comparisons and operators are constant-time.
 *)

(* Question d *)

(* The time complexity for ramanujan_number is O(max_n ^ 4).
 * The variables n1, n2, n3, n4 can be thought of as a counter, with each
 * 'digit' counting up to max_n before resetting to 1. When this happens, it
 * increments the next digit to its left, except for n1, which will return the
 * final result upon exceeding max_n. So to exhaustively search every 'digit'
 * n1...n4, we must iterate through max_n^4 4-tuples.
 *)

(* The first function finds the largest Ramanujan number; the second returns
 * the first one it finds.
 *)

(* The time complexity for ramanujan_number_2 is O(max_n ^ 4).
 * This function is quite similar to the previous one. At worst-case performance
 * (i.e., it doesn't find a Ramanujan number), it has the same time complexity
 * as the previous function, that is, O(max_n ^ 4). Since we are looking for
 * worst-case time complexity, this function is still O(max_n ^ 4).
 *)

(* PART 3 *)

(* Question 1 *)

(* Takes a list of integers and returns a two-tuple containing the smallest
 * number in the list and the integer index of the number.
 *)
let find_smallest lst =
  match lst with
    | [] -> failwith "empty list"
    | h :: t ->
      let rec iter index value lst =
        match lst with
          | [] -> (value, index)
          | h :: t when h > value -> iter (index + 1) h t
          | _ :: t -> iter index value t
      in iter 0 h t

(* Takes an integer index, another integer, and a list of integers, and returns
 * a new list which results from replacing the integer in the old list at the
 * given index with the new integer.
 *)
let replace index value lst =
  let rec iter curr index value lst =
    match lst with
      | [] -> failwith "invalid index"
      | h :: t when curr = index -> value :: t
      | h :: t -> h :: iter (curr + 1) index value t
  in iter 0 index value lst

(* Takes a list of integers and returns a list of integers of the same length
 * in which the smallest number in the list has been swapped with the first
 * number in the list.
 *)
let swap_smallest_with_first lst =
  match find_smallest lst, lst with
    | _, 0, h :: t -> h :: t
    | smallest, index -> smallest :: replace (index - 1) h t

(* Takes a list of integers and returns a list of sorted integers. *)
let rec minimum_element_sort lst =
  List.hd lst :: minimum_element_sort (List.tl lst)

(* Question 2 *)

(* Takes a list of integers, an index, and a count. Returns a list of the same
 * length with the same elements, but with the section starting at index and
 * extending count elements flipped (reversed).
 *)
let flip lst index count =
  let rec helper lst index count curr lst2 =
    match lst with
      | [] when index <> 0 || count <> 0 -> failwith "invalid inputs"
      | h :: t when curr < index -> h :: helper t index count (curr + 1) lst2
      | h :: t when count > 0 -> helper t index (count - 1) curr (h :: lst2)
      | l -> lst2 @ l
  in helper lst index count 0 []

(* Takes a list of integers and an integer index and flips a section of four
 * adjacent values in the list, starting from index. Used for solving 1-D
 * Rubik's cube problem.
 *)
let flip_rubik_1d lst index =
  match index with
    | 0
    | 1
    | 2 -> flip lst index 4
    | _ -> failwith "flip_rubik_1d: invalid input"

(* Takes an initial (possibly scrambled list of integers 1 through 6,
 * and a list of integers between 0 to 2 of arbitrary length, representing
 * moves on a 1-D Rubik's cube.
 * Outputs the first list after the second argument's moves are applied.
 *)
let rec rubik_1d cube moves =
  match moves with
    | [] -> cube
    | h :: t -> rubik_1d (flip_rubik_1d cube h) t

(* Question 3 *)
(* TODO *)

(* PART 4 *)

(* Question 1 *)

(* Part 1 *)

(* Takes a function and initial value, generates a list. *)
let rec unfold func init =
  match func init with
    | None -> []
    | Some (x, y) -> x :: unfold func y

(* Part 2 *)

(* Computes an ascending list of integers ranging from -100 to 100, by
 * increments of 4.
 *)

let ascending i = if i > 100 then None else Some (i, (i + 4))

let ascending_list = unfold ascending (-100)

(* Part 3 *)

(* Takes a list and returns a list of all suffixes of the original list. *)
let all_suffixes lst =
  let helper lst = if lst = [] then None else Some (lst, List.tl lst)
  in unfold helper lst

(* Question 2 *)

(* Ben's implementation must iterate over the sequence of values twice, once
 * from within the accumulate function, and once to map f to the list.
 * Thus it is time-inefficient. It is also not space-efficient, since this
 * dual iterations over lists generates a second list that must be stored
 * somewhere in memory.
 *)

(* Recursive version of MapReduce. *)
let rec mapReduce1_rec f op initial sequence =
  match sequence with
    | [] -> initial
    | h :: t -> op (f h) (mapReduce1_rec f op initial t)

(* Iterative version of MapReduce. *)
let mapReduce1_iter f op init lst =
  let rec iter lst accum =
    match lst with
      | [] -> accum
      | h :: t -> iter t (op (f h) accum)
  in iter lst init

(* It does not make sense for a MapReduce-style function to have a reducing
 * operator that is not commutative. Because of the distributed nature
 * of the mapping process, the outputs from mapping (inputs to reduce process)
 * are never guaranteed in a particular order, so any operators whose outputs
 * are dependent on operand order should not be used.
 *
 * Due to the order of their traversal through the list, both implementations
 * give the same results even when a non-commutative operator is used.
 * And, of course, they give the same results for commutative operators.
 *)

(* Question 3 *)

type 'a tree =
  | Leaf of 'a
  | Branch of 'a tree * 'a tree

(* MapReduce for trees as defined above. *)
let rec mapReduce_tree mapper reducer data =
  match data with
    | Leaf l -> mapper l
    | Branch (b1, b2) ->
        reducer (mapReduce_tree mapper reducer b1)
                (mapReduce_tree mapper reducer b2)

(*  MapReduce for a multi-argument mapper. *)
let rec mapReduceN mapper reducer init lst =
  match lst with
    | [] -> failwith "no sequences"
    | h :: _ when h = [] -> init
    | l -> reducer (mapper (List.map List.hd l))
                   (mapReduceN mapper reducer init (List.map List.tl l))
