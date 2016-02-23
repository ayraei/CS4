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
