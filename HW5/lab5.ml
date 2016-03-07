(* PART A *)

(* Question 1 *)

(* Computes the nth fibonacci number using a while loop. *)
let fibonacci n =
  if n = 0 then 0 else
    let f1 = ref 1 in
    let f0 = ref 0 in
    let tmp = ref 0 in
    let i = ref 1 in
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
      for i = 2 to n do
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
    | (_, _) -> failwith "unit_add: Incompatible types"

(* There is no combinatorial explosion when adding more unit classes, because
 * we handily introduced the unit "class" tag-- `Length, `Mass, `Time.
 * We can add a single case in the unit_add function for each class introduced,
 * regardless of what combination of units was used and regardless of how many
 * types of units are being defined.
 *)

(* PART C *)

(* Question 1 *)

(* Creates a gram object. *)
let rec make_gram g =
  let slugs_in_gram = 1. /. 14593.903203 in
  let check_compatibility other = other#unit_type = `Gram ||
                                  other#unit_type = `Slug
  in
    object (self)
      method get_grams = g
      method get_slugs = g *. slugs_in_gram
      method unit_type = `Gram
      method compatible other = check_compatibility other
      method add other =
        if not (check_compatibility other)
        then failwith "make_gram: incompatible object"
        else
          make_gram (g +. other#get_grams)
    end

(* Question 2 *)

(* Define a number as a message-passing object. *)
(* "value" is an int. *)
let rec make_number i =
  object
    method value = i
    method show = string_of_int i
    method is_zero = i = 0
    method is_number = true
    method evaluate _ _ = make_number i  (* must evaluate to an object *)
    method derive _ = make_number 0  (* derivative of a number is 0 *)
  end

(* Define a variable as a message-passing object. *)
(* "varname" is a string. *)
let rec make_variable v =
  object
    method value = failwith "variable has no numerical value"
    method show  = v
    method is_zero = false
    method is_number = false
    method evaluate v' n =
      if v = v'
        then make_number n
        else make_variable v
    method derive v' =
      if v = v'
        then make_number 1  (* d/dx(x) = 1 *)
        else make_number 0  (* d/dx(y) = 0 *)
  end

(* Define a sum as a message-passing object. *)
let rec make_sum expr1 expr2 =
  match () with
    | _ when expr1#is_zero -> expr2  (* expr + 0 = expr *)
    | _ when expr2#is_zero -> expr1  (* 0 + expr = expr *)
    | _ when expr1#is_number && expr2#is_number ->  (* add numbers *)
          make_number (expr1#value + expr2#value)
    | _ ->  (* create a new object representing the sum *)
          object
            method value = 
              failwith "sum expression has no numerical value"
            method show = "(" ^ expr1#show ^ " + " ^ expr2#show ^ ")"
            method is_zero = false
            method is_number = false
            method evaluate v n = 
              make_sum (expr1#evaluate v n) (expr2#evaluate v n)
            method derive v = 
              make_sum (expr1#derive v) (expr2#derive v)
          end

(* Evaluate a message-passing expression with a number 
   substituted for a variable. *)
let evaluate expr v n = expr#evaluate v n
  
(* Return the string representation of an expression. *)
let show expr = expr#show

(* Return the derivative of an expression. *)
let differentiate expr v = expr#derive v

(* part a *)

(* Define a product as a message-passing object. *)
let rec make_product expr1 expr2 =
  match () with
    | _ when expr1#is_zero -> make_number 0
    | _ when expr2#is_zero -> make_number 0
    | _ when expr1#is_number && expr1#value = 1 -> expr2
    | _ when expr2#is_number && expr2#value = 1 -> expr1
    | _ when expr1#is_number && expr2#is_number ->
          make_number (expr1#value * expr2#value)
    | _ ->
          object
            method value =
              failwith "product expression has no numerical value"
            method show = "(" ^ expr1#show ^ " * " ^ expr2#show ^ ")"
            method is_zero = false
            method is_number = false
            method evaluate v n =
              make_product (expr1#evaluate v n) (expr2#evaluate v n)
            method derive v =
              make_sum (make_product (expr1#derive v) expr2)
                       (make_product (expr1) (expr2#derive v))
          end

(* part b *)

(* part 1 *)
(* f = x^3*y + 3*x^2*y^2 + y^2 + 2 *)
(* INPUT
let f =
make_sum
 (make_product 
  (make_variable "x")
  (make_product 
   (make_variable "x")
   (make_product
    (make_variable "x")
    (make_variable "y"))))
 (make_sum
  (make_product 
   (make_number 3)
   (make_product
    (make_variable "x")
    (make_product
     (make_variable "x")
     (make_product
      (make_variable "y")
      (make_variable "y")))))
  (make_sum
   (make_product 
    (make_variable "y")
    (make_variable "y"))
   (make_number 2)))
*)

(* RESULT
val f :
  < derive : string -> 'a; evaluate : string -> int -> 'a; is_number : 
    bool; is_zero : bool; show : string; value : int >
  as 'a = <obj>
*)

(* part 2 *)
(* INPUT
let dfdx = differentiate f "x"
*)

(* RESULT
val dfdx :
  < derive : string -> 'a; evaluate : string -> int -> 'a; is_number : 
    bool; is_zero : bool; show : string; value : int >
  as 'a = <obj>
*)

(* part 3 *)
(* INPUT
show dfdx
*)

(* RESULT
- : string =
"(((x * (x * y)) + (x * ((x * y) + (x * y)))) + (3 * ((x * (y * y)) + (x * (y * y)))))"
*)

(* part 4 *)
(* INPUT
show (evaluate f "x" 3)
*)

(* RESULT
- : string =
"((3 * (3 * (3 * y))) + ((3 * (3 * (3 * (y * y)))) + ((y * y) + 2)))"
 *)

(* part 5 *)
(* INPUT
show (evaluate (evaluate f "x" 3) "y" 4)
*)

(* RESULT
- : string = "558"
 *)

(* part 6 *)
(* INPUT
show (evaluate (evaluate dfdx "x" 3) "y" 4)
*)

(* RESULT
- : string = "396"
 *)
