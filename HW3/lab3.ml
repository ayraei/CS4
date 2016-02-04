(* PART A *)

(* Question 1 *)

(* Defines a record type called point *)
type point = { x : float; y : float }
(* Defines a record type called segment *)
type segment = { startp : point; endp : point }

(* Takes a segment as argument and returns a point that is the midpoint *)
let midpoint_segment { startp; endp } =
    { (startp.x +. endp.x) /. 2.; (startp.y +. endp.y) /. 2. }

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
    segment_length (rectangle_lower_segment rectantle) +.
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
    segment_length (rectangle_lower_segment2 rectantle2) +.
    segment_length (rectangle_left_segment2 rectangle2) +.
    segment_length (rectangle_right_segment2 rectangle2)

(* Takes a rectangle and returns its area *)
let rectangle_area2 rectangle2 =
    segment_length (rectangle_upper_segment2 rectangle2) *.
    segment_length (rectangle_left_segment2 rectangle2)

(* Functions for testing *)

(* Takes two point arguments and creates a rectangle. Assumes valid points *)
let make_rectangle p1 p2 =
    match p1, p2 with
      | p1, p2 when p1.x < p2.x && p1.y < p2.y
      | p2, p1 when p1.x >= p2.x && p1.y >= p2.y
      | { lowLeft = { x = p1.x; y = p2.y };
          upRight = { x = p2.x; y = p1.y }
        } when p1.x < p2.x && p1.y >= p2.y
      | { lowLeft = { x = p2.x; y = p1.y};
          upRight = { x = p1.x; y = p2.y}
        } when p1.x >= p2.x && p1.y < p2.y

(* Takes four float arguments and creates a rectangle2. First two arguments
 * assumed to be x-values and last two arguments y-values *)
let make_rectangle2 a b c d =
    { lowX = a; upX = b; lowY = c; upY = d }

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
