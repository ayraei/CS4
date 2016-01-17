(* PART A *)

(* Question 1 *)
(*
 * 1. int = 10
 * 2. float = 10
 * 3. int = 12
 * 4. Error: This expression has type float but an expression was expected of
 *    type int
 *    This error occurred because we used integer addition "+" instead of
 *    floating-point addition "+." for floating-point arguments.
 * 5. Error: This expression has type int but an expression was expected of type
 *    float
 *    This error occurred because we used floating-point addition "+." instead
 *    of integer addition "+" but gave it integer arguments.
 * 6. Error: This expression has type float but an expression was expected of
 *    type int
 *    This error occurred because we used the integer addition operator "+" but
 *    one of the arguments is a floating-point number.
 * 7. Error: This expression has type int but an expression was expected of type
 *    float
 *    This error occurred because we specified floating-point addition "+."
 *    but one of the arguments is an integer, and Ocaml does not do automatic
 *    type casting from an int to a float.
 * 8. float = 7.2
 * 9. int = 5
 * 10. int = 7
 * 11. val a : int = 3
 * 12. val b : int = 4
 * 13. bool = false
 * 14. bool = true
 * 15. bool = false
 *     This is different from the previous expression because [???? FINISH]
 * 16. (int * int * int) list = [(1, 2, 3)]
 *     This result indicates we have entered a list with a tuple entry, since
 *     we separated the entries with commas instead of semicolons.
 * 17. int = 4
 * 18. Error: Syntax error
 *     Unlike in Python, "and" is not an operator
 * 19. int = 6
 * 20. Error: This expression has type int but an expression was expected of
 *     type unit
 *     This is not a syntax error, but rather a type error, because when the
 *     "else" is left off an if/then/else, it is assumed to return the unit
 *     value "()". This leads to a problem because the return types of both
 *     the then and else clauses must be the same, but the then clause
 *     here returns an int.
 *)

(* Question 2 *)
(* Takes three integers as arguments and returns the sum of the squares of the
 * two larger numbers.
 *)
let sum_of_squares_of_two_largest x y z =
    if a > c && b > c then a * a + b * b else
        if a > b && c > b then a * a + c * c else
            b * b + c * c

(* Question 3 *)
(* If b is positive, then the result will be a + b. If b is zero or negative,
 * the result will be a - b. As the function name implies, the function
 * computes a + abs(b).
 *)

(* PART B *)

(* Question 1 *)

