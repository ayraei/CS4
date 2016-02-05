(* PART A *)

(* Question 1 *)

(* Defines a record type called point *)
type point = { x : float; y : float }
(* Defines a record type called segment *)
type segment = { startp : point; endp : point }

(* Takes a segment as argument and returns a point that is the midpoint *)
let midpoint_segment { startp; endp } =
    { x = (startp.x +. endp.x) /. 2.; y = (startp.y +. endp.y) /. 2. }

(* Takes a segment as argument and returns the length of the segment *)
let segment_length { startp ; endp } =
    sqrt((startp.x -. endp.x) *. (startp.x -. endp.x) +.
    (startp.y -. endp.y) *. (startp.y -. endp.y))

(* Takes a point as argument and prints its representation to terminal *)
let print_point { x; y } =
    Printf.printf "(%g, %g)\n" x y

(* Takes two floats x, y and returns a point with those coordinates *)
let make_point x y =
    { x = x; y = y }

(* Takes two points and returns a segment with them as endpoints *)
let make_segment a b =
    { startp = a; endp = b }

(* Takes a point and returns a two-tuple of the (x, y) coordinates *)
let get_coords { x; y } =
    ( x, y )

(* Takes a segment and returns a two-tuple of the (start, end) points *)
let get_points { startp; endp } =
    ( startp, endp )

(* Question 2 *)

(* Defines a rectangle representation using its lower-left and upper-right
 * points *)
type rectangle = { lowLeft : point; upRight : point }
(* Defines a rectangle representation using its lower/upper x-values and
 * lower/upper y-values *)
type rectangle2 = { lowX : float; upX : float; lowY : float; upY : float }

(* Accessors and methods for rectangle1 *)
(* Takes a rectangle, returns a segment corresponding to its upper segment. *)
let rectangle_upper_segment { lowLeft; upRight } =
    {
      startp = { x = lowLeft.x; y = upRight.y };
      endp = { x = upRight.x; y = upRight.y }
    }

(* Takes a rectangle, returns a segment corresponding to its lower segment. *)
let rectangle_lower_segment { lowLeft; upRight } =
    {
      startp = { x = lowLeft.x; y = lowLeft.y };
      endp = { x = upRight.x; y = lowLeft.y }
    }

(* Takes a rectangle, returns a segment corresponding to its left segment. *)
let rectangle_left_segment { lowLeft; upRight } =
    {
      startp = { x = lowLeft.x; y = lowLeft.y };
      endp = { x = lowLeft.x; y = upRight.y }
    }

(* Takes a rectangle, returns a segment corresponding to its right segment. *)
let rectangle_right_segment { lowLeft; upRight } =
    {
      startp = { x = upRight.x; y = lowLeft.y };
      endp = { x = upRight.x; y = upRight.y }
    }

(* Takes a rectangle and returns its perimeter *)
let rectangle_perimeter rectangle =
    segment_length (rectangle_upper_segment rectangle) +.
    segment_length (rectangle_lower_segment rectangle) +.
    segment_length (rectangle_left_segment rectangle) +.
    segment_length (rectangle_right_segment rectangle)

(* Takes a rectangle and returns its area *)
let rectangle_area rectangle =
    segment_length (rectangle_upper_segment rectangle) *.
    segment_length (rectangle_left_segment rectangle)

(* Accessors and methods for rectangle2 *)
(* Takes a rectangle, returns a segment corresponding to its upper segment. *)
let rectangle_upper_segment2 { lowX; upX; lowY; upY } =
    {
      startp = { x = lowX; y = upY };
      endp = { x = upX; y = upY }
    }

(* Takes a rectangle, returns a segment corresponding to its lower segment. *)
let rectangle_lower_segment2 { lowX; upX; lowY; upY } =
    {
      startp = { x = lowX; y = lowY };
      endp = { x = upX; y = lowY }
    }

(* Takes a rectangle, returns a segment corresponding to its left segment. *)
let rectangle_left_segment2 { lowX; upX; lowY; upY } =
    {
      startp = { x = lowX; y = lowY };
      endp = { x = lowX; y = upY }
    }

(* Takes a rectangle, returns a segment corresponding to its right segment. *)
let rectangle_right_segment2 { lowX; upX; lowY; upY } =
    {
      startp = { x = upX; y = lowY };
      endp = { x = upX; y = upY }
    }

(* Takes a rectangle and returns its perimeter *)
let rectangle_perimeter2 rectangle2 =
    segment_length (rectangle_upper_segment2 rectangle2) +.
    segment_length (rectangle_lower_segment2 rectangle2) +.
    segment_length (rectangle_left_segment2 rectangle2) +.
    segment_length (rectangle_right_segment2 rectangle2)

(* Takes a rectangle and returns its area *)
let rectangle_area2 rectangle2 =
    segment_length (rectangle_upper_segment2 rectangle2) *.
    segment_length (rectangle_left_segment2 rectangle2)

(* Functions for testing *)

(* Takes two point arguments and creates a rectangle. Assumes valid points *)
let make_rectangle p1 p2 =
    { lowLeft = p1; upRight = p2 }

(* Takes four float arguments and creates a rectangle2. First two arguments
 * assumed to be x-values and last two arguments y-values *)
let make_rectangle2 a b c d =
    { lowX = a; upX = b; lowY = c; upY = d }

(* Question 3 *)
let make_pair x y = fun m -> m x y
let first z = z (fun x y -> x)
let second z = z (fun x y -> y)

(* Evaluation *)
(* [TODO] *)

(* Question 4 *)

(* Takes two int arguments x, y and returns x^y *)
let rec pow x y =
    if y = 0
    then 1
    else x * pow x (y - 1)

(* Takes two int arguments x, y and returns the integer log base x of y *)
let rec int_log x y =
    if y mod x <> 0
    then 0
    else 1 + int_log x (y / x)

(* Returns a pair representation of two nonnegative integers *)
let make_pairi a b =
    (pow 2 a) * (pow 3 b)

(* Returns the first element in a pair *)
let firsti p =
    int_log 2 p

(* Returns the second element in a pair *)
let secondi p =
    int_log 3 p

(* Question 5 *)
let zero = []

let is_zero = function
  | [] -> true
  | () :: _ -> false

let succ u = () :: u

(* Takes one unary argument and returns a unary integer one less than it *)
let prev = function
  | [] -> invalid_arg "Argument is zero, cannot decrement!\n"
  | _ :: a -> a

(* Takes an int and returns the unary representation of it *)
let rec integer_to_unary i =
    if i = 0
    then zero
    else succ (integer_to_unary (i - 1))

(* Takes one unary argument and returns the corresponding integer *)
let rec unary_to_integer u =
    if is_zero u
    then 0
    else 1 + (unary_to_integer (prev u))

(* Takes two unary arguments and adds them in unary *)
let rec unary_add a b =
    if is_zero b
    then a
    else succ (unary_add a (prev b))

(* Now, use the other representation *)

type nat = Zero | Succ of nat

let zero' = Zero

let is_zero' = function
  | Zero -> true
  | Succ _ -> false

let succ' u = Succ u

(* Takes one unary argument and returns a unary integer one less than it *)
let prev' = function
  | Zero -> invalid_arg "Argument is zero, cannot decrement!\n"
  | Succ u -> u

(* Takes an int and returns the unary representation of it *)
let rec integer_to_unary' i =
    if i = 0
    then zero'
    else succ' (integer_to_unary' (i - 1))

(* Takes one unary argument and returns the corresponding integer *)
let rec unary_to_integer' u =
    if is_zero' u
    then 0
    else 1 + (unary_to_integer' (prev' u))

(* Takes two unary arguments and adds them in unary *)
let rec unary_add' a b =
    if is_zero' b
    then a
    else succ' (unary_add' a (prev' b))

(* No, besides the name changes, the abstraction layer removes the need to
 * change anything about the implementation of the function definitions
 * listed.
 *)

(* Question 6 *)
let zero = fun s -> fun z -> z
let add1 n = fun s -> fun z -> s (n s z)

(* one in Church numeral representation *)
let one = fun s -> fun z -> s (z)
(* two in Church numeral representation *)
let two = fun s -> fun z -> s (s (z))
(* three in Church numeral representation *)
let three = fun s -> fun z -> s (s (s (z)))
(* four in Church numeral representation *)
let four = fun s -> fun z -> s (s (s (s (z))))
(* five in Church numeral representation *)
let five = fun s -> fun z -> s (s (s (s (s (z)))))
(* six in Church numeral representation *)
let six = fun s -> fun z -> s (s (s (s (s (s (z))))))
(* seven in Church numeral representation *)
let seven = fun s -> fun z -> s (s (s (s (s (s (s (z)))))))
(* eight in Church numeral representation *)
let eight = fun s -> fun z -> s (s (s (s (s (s (s (s (z))))))))
(* nine in Church numeral representation *)
let nine = fun s -> fun z -> s (s (s (s (s (s (s (s (s (z)))))))))
(* ten in Church numeral representation *)
let ten = fun s -> fun z -> s (s (s (s (s (s (s (s (s (s (z))))))))))

(* Church numeral addition *)
let add m n s z = fun m -> fun n -> fun s -> fun z -> (m s) ((n s) z)

(* Converts a Church numeral to integer *)
(* ?? *)

(* Question 7 *)
(* [TODO] *)

(* PART B *)

(* Question 1 *)
(* [TODO] *)

(* Question 2 *)
(* [TODO] *)

(* Question 3 *)
(* [TODO] *)

(* Question 4 *)
(* [TODO] *)

(* Question 5 *)
(* [TODO] *)

(* Question 6 *)
(* [TODO] *)

(* Question 7 *)
(* [TODO] *)
