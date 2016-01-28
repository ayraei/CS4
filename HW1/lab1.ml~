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
 *     This is different from the previous expression because lists are not
 *     primitive types, so the two lists are represented as two distinct
 *     entities/objects. Since we used the == comparison, which checks whether
 *     two objects are the same object, it evaluates to false.
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
let sum_of_squares_of_two_largest a b c =
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
(*
With an interpreter that uses normal-order evaluation, we get (briefly):
test 0 (p ())
fun test -> if x = 0
    then 0
    else y
if 0 = 0
    then 0
    else (p ())
BUT because "p ()" is not a primitive operator, the interpreter will keep
attempting to substitute in its definition and the program will be stuck in
an infinite loop.

With an interpreter that uses applicative-order evaluation, we get (briefly):
test 0 (p ())
fun test -> if x = 0
    then 0
    else y
if 0 = 0
Replace with 'then' clause
0
*)

(* Question 2 *)
(* TODO *)

(* Question 3 *)
(*
evaluate add_a 2 5
evaluate 2 -> 2
evaluate 5 -> 5
desugar add_a -> fun add_a a b -> if ...
apply fun add_a a b ->
    if a = 0
    then b
    else inc (add_a (dec a) b)
substitute 2 for a and 5 for b in fun expression
    -> if 2 = 0
       then 5
       else inc (add_a (dec 2) 5)
evaluate if 2 = 0 then 5 else inc (add_a (dec 2) 5)
evaluate 2 = 0
    evaluate 2 -> 2
    evaluate 0 -> 0
    evaluate = -> =
    apply = to 2, 0 -> false
replace with false clause -> inc (add_a (dec 2) 5)
evaluate inc (add_a (dec 2) 5)
    evaluate add_a (dec 2) 5
        evaluate dec 2
            evaluate 2 -> 2
            evaluate dec -> [primitive function]
            apply dec to 2 -> 1
        evaluate 5 -> 5
        desugar add_a -> fun add_a a b -> if ...
        apply fun add_a a b ->
            if a = 0
            then b
            else inc (add_a (dec a) b)
        substitute 1 for a and 5 for b in fun expression
            -> if 1 = 0
               then 5
               else inc (add_a (dec 1) 5)
        evaluate if 1 = 0 then ...
        evaluate 1 = 0
            evaluate 1 -> 1
            evaluate 0 -> 0
            evaluate = -> =
            apply = to 1, 0 -> false
        replace with false clause -> inc (add_a (dec 1) 5)
        evaluate inc (add_a (dec 1) 5)
            evaluate add_a (dec 1) 5
                evaluate dec 1
                    evaluate 1 -> 1
                    evaluate dec -> [primitive function]
                    apply dec to 1 -> 0
                evaluate 5 -> 5
                desugar add_a -> fun add_a a b -> if ...
                apply fun add_a a b ->
                    if a = 0
                    then b
                    else inc (add_a (dec a) b)
                substitute 0 for a and 5 for b in fun expression
                    -> if 0 = 0
                       then 5
                       else inc (add_a (dec 0) 5)
                evaluate if 0 = 0 then ...
                evaluate 0 = 0
                    evaluate 0 -> 0
                    evaluate 0 -> 0
                    evaluate = -> =
                    apply = to 0, 0 -> true
                replace with true clause -> then 5
                evaluate 5 -> 5
                evaluate inc -> [primitive function]
                apply inc to 5 -> 6
    evaluate inc -> [primitive function]
    apply inc to 6 -> 7
result: 7

evaluate add_b 2 5
    evaluate 2 -> 2
    evaluate 5 -> 5
    desugar add_b -> fun add_b a b -> if ...
    apply fun add_b a b ->
        if a = 0
        then b
        else add_b (dec a) (inc b)
    substitute 2 for a and 5 for b in fun
        -> if 2 = 0
           then 5
           else add_b (dec 2) (inc 5)
    evaluate if 2 = 0
        evaluate 2 = 0
            evaluate 2 -> 2
            evaluate 0 -> 0
            evaluate = -> =
            apply = to 2, 0 -> false
    replace with false clause -> else add_b (dec 2) (inc 5)
    evaluate add_b (dec 2) (inc 5)
        evaluate dec 2
            evaluate 2 -> 2
            evaluate dec -> [primitive function]
            apply dec to 2 -> 1
        evaluate inc 5
            evaluate 5 -> 5
            evaluate inc -> [primitive function]
            apply inc to 5 -> 6
        apply add_b to 1, 6
        desugar add_b -> fun add_b a b -> if ...
        apply fun add_b a b -> if ...
        substitute 1 for a and 6 for b in fun
            -> if 1 = 0
               then 6
               else add_b (dec 1) (inc 6)
        evaluate if 1 = 0
            evaluate 1 = 0
                evaluate 1 -> 1
                evaluate 0 -> 0
                evaluate = -> =
                apply = to 1, 0 -> false
        replace with false clause -> else add_b (dec 1) (inc 6)
        evaluate add_b (dec 1) (inc 5)
            evaluate dec 1
                evaluate 1 -> 1
                evaluate dec -> [primitive function]
                apply dec to 1 -> 0
            evaluate inc 6
                evaluate 6 -> 6
                evaluate inc -> [primitive function]
                apply inc to 6 -> 7
            desugar add_b -> fun add_b a b -> if ...
            apply fun add_b a b -> if ...
            substitute 0 for a and 7 for b in fun expression
                -> if 0 = 0
                   then 7
                   else add_b (dec 0) (inc 7)
            evaluate if 0 = 0
                evaluate 0 = 0
                    evaluate 0 -> 0
                    evaluate 0 -> 0
                    evaluate = -> =
                    apply = to 0, 0 -> true
                replace with true clause -> 7
result: 7
*)

(* PART C *)

(* Question 1 *)
(* This function computes the factorial of the input number,
   which for a number n is equal to n * (n-1) * ... * 1. *)
let rec factorial n =
    if n = 0 then 1 else n * factorial (n - 1)

(* 1 A *)
(* This function takes a non-negative integer argument and computes that term
 * of the infinite series expansion of e.
 *)
let e_term x =
    1. /. (float_of_int (factorial x))

(* 1 B *)
(* This function takes a positive integer and computes an approximation of e
 * by summing up that many terms of the infinite series expansion of e.
 *)
let rec e_approximation n =
    if n = 0
    then e_term 0
    else (e_term n) +. e_approximation (n - 1)

(* 1 C *)
(* Evaluating "e_approximation 20" gave the result
 * "float = 2.71828182845904553", and "exp 1.0" gave the result
 * "float = 2.71828182845904509"
 *)

(* 1 D *)
(* Evaluating "e_approximation 100" gave the result "float = infinity".
 * This answer is the result of an overflow error. For large enough n,
 * the number of bits required to represent n! exceeds the size of an
 * int in Ocaml, and the value overflows as 0 without throwing an error.
 * This leads to a divide-by-zero error in the function e_term.
 *)

(* Question 2 *)
(* is_even returns true if its argument is an even, non-negative integer and
 * false if the argument is an odd non-negative integer (zero is even).
 * is_odd returns true if its argument is odd, and false otherwise.
 *)
let rec is_odd x = 
    if x = 0
    then false
    else is_even (x - 1)
and is_even x =
    if x = 0
    then true
    else is_odd (x - 1)

(* Question 3 *)
(* Recursive definition of function f(n). *)
let rec f_rec n =
    if n < 3
    then n
    else f (n - 1) + 2 * f (n - 2) + 3 * f (n - 3)

(* Helper function used to define f_iter iteratively. *)
let rec f_help a b c max =
    if max = (a + 1)
    then a + 2 * b + 3 * c
    else f_help (a + 2 * b + 3 * c) a b max
(* Iterative definition of function f(n). *)
let f_iter n =
    if n < 3
    then n
    else f_help 2 2 0 n

(* Question 4 *)
(* Takes two integer arguments corresponding to row number and row index
 * and computes that element of Pascal's triangle.
 *)
let rec pascal_coefficient n i =
    match n, i with
        | 1, 1 -> 1
        | _, _ -> 2
