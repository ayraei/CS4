(* name: Joanne Li *)
(* login: jli9 *)
(*
PARTS DONE
1a 1b
2a 2b 2c 2d
3.1 3.2 3.3
4.1.1 4.1.2 4.1.3 4.2 4.3
*)

(* PART 1 *)

(* Question a *)
(*
let rec f y z acc =
  if acc > z
    then acc
    then f (y + z) (z + acc) (acc + y) ;;

f 3 4 0 ;;
*)

(* Question b *)
(*
let x = 7 ;;
*)

(* PART 2 *)

(* Question a *)
(*
let disc a b c = sqrt (b *. b -. 4.0 *. a *. c)
let solve_quad a b c =
  let d = disc a b c in
  let a2 = 2.0 *. a in
    (((-. b) +. d) /. a2, ((-. b) -. d ) /. a2)
*)

(* Question b *)
(*
let rec f x y acc =
  if x > 0 then f (x - 1) y (acc + x)
  else if y > 0 then f x (y - 1) (acc + y)
  else acc
*)

(* Question c *)
(*
let bouncy y =
  let rec iter h k =
    if k > y
      then h
      else iter (h + 1) (k * 2)
  in iter 0 1
*)

(* Question d *)

(* PART 3 *)

(* Question 1 *)

(* Question 2 *)

(* Question 3 *)

(* PART 4 *)

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
          | [] -> index
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

(* Takes a list of integers and returns a list of integers of the same length
 * in which the smallest number in the list has been swapped with the first
 * number in the list.
 *)
let swap_smallest_with_first (h :: t) as lst =
  match (find_smallest lst) with
    | (_, 0) -> lst
    | (smallest, index) -> smallest :: replace (index - 1) h t

(* Takes a list of integers and returns a list of sorted integers. *)
let rec minimum_element_sort lst =
  let h :: t = swap_smallest_with_first lst in
    h :: minimum_element_sort t

(* Question 2 *)

(* Question 3 *)
