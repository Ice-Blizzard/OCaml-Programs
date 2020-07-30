open Array

(* val multipling : int array array -> int array array -> int array array =
  <fun>
*)
let multipling a b =
  let n = length a in
  let c = make_matrix n n 0 in
  for i = 0 to (n - 1) do
    for j = 0 to (n - 1) do
      for k = 0 to (n - 1) do
        c.(i).(j) <- c.(i).(j) + (a.(i).(k) * b.(k).(j))
      done
    done
  done;
  c

(* val fast_matrix_exponentiation : int array array -> int -> int array array =
  <fun>
 *)
let rec fast_matrix_exponentiation e k =
  let n = length e in
  if (k == 0) then
    let c = make_matrix n n 0 in
    for i = 0 to (n - 1) do
      c.(i).(i) <- 1
    done;
    c
  else
    if (k == 1) then
      e
    else
      if (k mod 2 == 0) then
        multipling (fast_matrix_exponentiation e (k/2))
          (fast_matrix_exponentiation e (k/2))
      else
        multipling (fast_matrix_exponentiation e (k - 1)) 
          (fast_matrix_exponentiation e 1);;
