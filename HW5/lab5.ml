(* PART A *)

(* Question 1 *)

(* Computes the nth fibonacci number using a while loop. *)
let fibonacci n =
  if n = 0 then 0 else
    let f1 = ref 1 in
    let f0 = ref 0 in
    let tmp = ref 0 in
    let i = ref 0 in
      while !i < n do
        tmp := !f0 + !f1;
        f0 := !f1;
        f1 := !tmp;
        i := !i + 1
      done;
    !f1

(* Computes the nth fibonacci number using a for loop. *)
let fibonacci2 n =
  if n = 0 then 0 else
    let f1 = ref 1 in
    let f0 = ref 0 in
    let tmp = ref 0 in
      for i = 1 to n do
        tmp := !f0 + !f1;
        f0 := !f1;
        f1 := !tmp
      done;
    !f1

(* Question 2 *)

(* Imperative implementation of bubble sort. *)
let bubble_sort arr =
  if Array.length arr < 2 then () else
    let tmp = ref arr.(0) in
    let sorted = ref false in
      while not !sorted do
        sorted := true;
        for i = 0 to (Array.length arr) - 2 do
          if arr.(i) > arr.(i + 1) then
            begin
              tmp := arr.(i);
              arr.(i) <- arr.(i + 1);
              arr.(i + 1) <- !tmp;
              sorted := false
            end
          else ()
        done;
      done;
    ()

(* PART B *)

(* Question a *)

let meters_per_foot = 0.3048

let get_meters len =
  match len with
    | `Meter m -> m
    | `Foot f -> f *. meters_per_foot
    | `Inch i -> i *. meters_per_foot /. 12.

let length_add a b = `Meter (get_meters a +. get_meters b)

(* Question b *)

(* mass abstraction *)

let grams_per_slug = 14593.903203

let get_grams wt =
  match wt with
    | `Gram g -> g
    | `Kilo k -> 1000. *. k
    | `Slug s -> s *. grams_per_slug

let mass_add a b = `Gram (get_grams a +. get_grams b)

(* time abstraction *)

let get_seconds tm =
  match tm with
    | `Second s -> s
    | `Minute m -> 60. *. m
    | `Hour h -> 3600. *. h
    | `Day d -> 86400. *. d

let time_add a b = `Second (get_seconds a +. get_seconds b)

(* Question c *)

(* takes two tagged data values and adds them if compatible; signals an
 * error if not compatible.
 *)
let unit_add a b =
  match (a, b) with
    | (`Length a, `Length b) -> length_add a b
    | (`Mass a, `Mass b) -> mass_add a b
    | (`Time a, `Time b) -> time_add a b

(* There is no combinatorial explosion when adding more unit classes, because
 * we handily introduced the unit "class" tag-- `Length, `Mass, `Time.
 * We can add a single case in the unit_add function for each class introduced,
 * regardless of what combination of units was used and regardless of how many
 * types of units are being defined.
 *)

(* PART C *)

(* Question 1 *)

(* TODO *)

(* Question 2 *)

(* TODO *)
