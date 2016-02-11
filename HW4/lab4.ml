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
let left_branch mob =
    mob

(* accessor that returns the right branch of a mobile *)
let right_branch mob =
    mob

(* PART B *)

(* PART C *)

(* Question 1 *)

type expr =
  | Int of int            (* constant *)
  | Var of string         (* variable *)
  | Add of expr * expr    (* expr1 + expr2 *)
  | Mul of expr * expr    (* expr 1 * expr2 *)
  | Pow of expr * int     (* expr^n *)

(* Performs algebraic simplification on an expr expression. *)
let rec simplify1 expr =
    5

let rec simplify expr =
  let e = simplify1 expr in
    if expr = e
      then expr
      else simplify e

(* Question 2 *)

(* Takes the derivative of a given expr expression. *)
let deriv expr =
    5

let derivative expr var =
  let e = simplify expr in
  let d = deriv e var in
    simplify d
