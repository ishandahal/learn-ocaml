let _ : bool = 42 = 42
let _ : float = 3.14 /. 2.0
let _ = 42 * 10
let _ = 4.2 ** 7.
let _ : bool = "hi" == "hi"
let _ = "hi" = "hi"
let _ = assert (2110 <> 3110)
let _ = assert (not (2110 = 3110))
let _ = if 2 < 1 then 42 else 7
let double x = x * 2
let _ = assert (double 2 = 4)

let _ =
  assert (double 7 = 14);
  print_endline "hello world"

let _ =
  assert (double 8 = 16);
  Printf.printf "val %i doubled was %i\n%!" 8 18;

  let _ = 3 + 2 in
  print_string "something\n"

let cube x : float = x ** 3.

let _ =
  print_string "printing 3 cubed: ";
  print_float (cube 3.);
  print_endline "\n";
  print_endline "just printed a new line before this"

let sign x = if x < 0 then -1 else if x > 0 then 1 else 0
let _ = assert (sign (-10) = -1)
let _ = assert (sign 10 = 1)
let _ = assert (sign 0 = 0)
let area_of_circle r : float = Float.pi *. (r ** 2.)
let close_enough x y eps = Float.abs x -. Float.abs y < eps
let _ = assert (close_enough (area_of_circle 1.) Float.pi 0.1)

let _ =
  assert (close_enough (area_of_circle 2.3) (Float.pi *. (2.3 ** 2.)) 0.00001)

let root_mean_square x y = sqrt (((x ** 2.) +. (y ** 2.)) /. 2.)
let _ = print_float (root_mean_square 2. 3.)
let _ = assert (close_enough (root_mean_square 2. 3.) 2.8045 0.001)

let valid_date (d : int) (m : string) =
  if
    m = "Jan" || m = "Mar" || m = "May" || m = "Jul" || m = "Aug" || m = "Oct"
    || m = "Dec"
  then d >= 1 && d <= 31
  else if m = "Apr" || m = "Jun" || m = "Sep" || m = "Nov" then
    d >= 1 && d <= 31
  else if m = "Feb" then d >= 1 && d <= 28
  else false

let _ = assert (not (valid_date 29 "Feb"))
let _ = assert (valid_date 2 "Feb")

let rec fibonacci_tr n pp p =
  if n <= 1 then p else fibonacci_tr (n - 1) p (pp + p)

let fibonacci n = fibonacci_tr n 0 1

(* if n = 1 || n = 2 then 1 else fibonacci (n - 1) + fibonacci (n - 2) *)
let _ = assert (fibonacci 2 = 1)
let rec fib_limit n = if fibonacci n < 0 then n - 1 else fib_limit (n + 1)
let lim = fib_limit 0

let _ =
  print_newline ();
  print_string "fib limit: ";
  print_int lim;
  print_newline ()

let divide ~numerator:x ~denominator:y = x /. y

let _ =
  assert (close_enough (divide ~numerator:2. ~denominator:3.) (2. /. 3.) 0.0001)

(* ======================= *)
(* Ocaml from beginning: chap - names and functions *)

(** [args_nonzero arg1 arg2] is true is both arguments are non zero false otherwise*)
let args_nonzero arg1 arg2 = arg1 <> 0 && arg2 <> 0

let _ = assert (not (args_nonzero 0 0))
let _ = assert (not (args_nonzero 1 0))
let _ = assert (not (args_nonzero 0 1))
let _ = assert (args_nonzero 1 1)

(** [sum n] is the sum of 1 .. n (inclusive)*)
let rec sum n = if n = 1 then 1 else n + sum (n - 1)

let _ = assert (sum 1 = 1)
let _ = assert (sum 3 = 6)

(** [power x n] is x raised to the power of n*)
let rec power x n = if n = 0 then 1 else x * power x (n - 1)

let _ = assert (power 2 0 = 1)
let _ = assert (power 2 2 = 4)
let _ = assert (power 2 3 = 8)

(** [ isconsonant c] is true is c is a lower-case consonant char flase otherwise *)
let isconsonant = function 'a' | 'e' | 'i' | 'o' | 'u' -> false | _ -> true

let _ = assert (isconsonant 'b')
let _ = assert (not (isconsonant 'e'))

(** [not x] is negation of [x] which is a boolen*)
let not = function false -> true | _ -> false

let _ = assert (not false)
let _ = assert (not (not true))

(** [sum n] is the sum of ints 1 .. n (inclusive)*)
let rec sum = function 1 -> 1 | n -> n + sum (n - 1)

let _ = assert (sum 1 = 1)
let _ = assert (sum 3 = 6)

(** [power x n] is x raised to the power of n*)
let rec power x = function 0 -> 1 | n -> x * power x (n - 1)

let _ = assert (power 2 0 = 1)
let _ = assert (power 2 2 = 4)
let _ = assert (power 2 3 = 8)

(** [isupper c] is true if c is uppercase false otherwise*)
let isupper = function 'A' .. 'Z' -> true | _ -> false

let () = assert (isupper 'A')
let () = assert (not (isupper 'a'))

(** [islower c] is true if c is lowercase false otherwise*)
let islower = function 'a' .. 'a' -> true | _ -> false

let () = assert (islower 'a')
let () = assert (not (islower 'A'))
