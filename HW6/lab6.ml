(* PART A *)

(* Question 1 *)
let factorial n =
  let rec iter m r =
    if m = 0
      then r
      else iter (m - 1) (r * m)
  in iter n 1
in
  factorial 3

(*

FRAME 0 (initial environment)
  parent: none
  bindings:
    - : [primitive function -]
    * : [primitive function *]

FRAME 1 (fun n -> let rec iter m r = ... in iter n 1)
  env: FRAME 0
  param: n
  body: let rec iter m r = ... in iter n 1

FRAME 2
  parent: FRAME 0
  bindings:
    n : 3

FRAME 3 (fun m -> (fun r -> if m = 0 ... (r * m)))
  env: FRAME 1
  param: m
  body: if m = 0 then r else iter (m - 1) (r * m)

*)

(* Question 2 *)

let rec factorial n =
  if n = 0 then 1 else n * factorial (n - 1)

let factorial =
  let f = ref (fun n -> 0) in
    (* TODO *)
    
(* PART B *)

(* Question 1 *)

exception Stat_error of string

(* First attempt at a statistics object. *)
let make_stat_1 () =
  let sum = ref 0.0 in
  let sumsq = ref 0.0 in
  let n = ref 0 in
  object
    method append x =
      begin
        n := !n + 1;
        sum := !sum +. x;
        sumsq := !sumsq +. (x *. x)
      end
    method mean =
      if n = 0 then raise (Stat_error "need at least one value for mean")
      else !sum /. (float_of_int n)
    method variance =
      if n = 0 then raise (Stat_error "need at least one value for variance")
      else (sumsq -. (sum *. sum /. (float_of_int n))) /. (float_of_int n)
    method stdev =
      if n = 0 then raise (Stat_error "need at least one value for stdev") else
      sqrt (sumsq -. (sum *. sum /. (float_of_int n))) /. (float_of_int n))
    method clear =
      begin
        sum := 0.0;
        sumsq := 0.0;
        n := 0
      end
  end

(* Question 2 *)

(* Slightly more efficiently written statistics object. *)
let make_stat_2 () =
  let sum = ref 0.0 in
  let sumsq = ref 0.0 in
  let n = ref 0 in
  object (self)
    method append x =
      begin
        n := !n + 1;
        sum := !sum +. x;
        sumsq := !sumsq +. (x *. x)
      end
    method private _variance =
      (sumsq -. (sum *. sum /. (float_of_int n))) /. (float_of_int n)
    method mean =
      if n = 0 then raise (Stat_error "need at least one value for mean")
      else !sum /. (float_of_int n)
    method variance =
      if n = 0 then raise (Stat_error "need at least one value for variance")
      else self#_variance
    method stdev =
      if n = 0 then raise (Stat_error "need at least one value for stdev") else
      sqrt self#_variance
    method clear =
      begin
        sum := 0.0;
        sumsq := 0.0;
        n := 0
      end
  end

(* PART C *)

(* Question 1 *)

module type PRIORITY_QUEUE =
  sig
    exception Empty
    type elem
    type t
    val empty : t
    val is_empty : t -> bool
    val insert : t -> elem -> t
    val find_min : t -> elem
    val delete_min : t -> t
    val from_list : elem list -> t
  end

module PriorityQueue : (PRIORITY_QUEUE with type elem = int) =
  struct
    exception Empty
    
    type elem = int
    type t =
      | Leaf
      | Node of int * int * t * t
    
    let empty = Leaf
    let is_empty q = q = Leaf
    (* private helper to merge two heaps *)
    let rec merge l r = match (l, r) with
      | (_, Leaf) -> l
      | (Leaf, _) -> r
      | (Node (l1, l2, l3, l4), Node (r1, _, _, _)) when l1 < r1 ->
          Node (l1, l2, l3, merge l4 r)
      | (Node (l1, _, _, _), Node (r1, r2, r3, r4)) ->
          Node (r1, r2, r3, merge r4 l)
    let insert q item = merge q (Node (item, 0, Leaf, Leaf))
    let find_min (Node (x, _, _, _)) = x
    let delete_min (Node (x, _, l, r)) = merge l r
    let from_list lst =
      let rec iter lst q = match lst with
        | [] -> q
        | h :: _ -> insert q h
      in iter lst Leaf
  end

(* Takes a list of ints and converts it to a priority queue, then uses
 * heapsort to return a list of the elements in ascending order.
 *)
let heap_sort lst =
  let q = PriorityQueue.from_list lst in
  let rec iter q lst = match q with
    | PriorityQueue.empty -> lst
    | PriorityQueue.t ->
        iter (PriorityQueue.delete_min q) ((PriorityQueue.find_min q) :: lst)
  in List.reverse (iter q [])
    
(* Question 2 *)

(* TODO: still trying to get question 1 to work... *)
