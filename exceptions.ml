(** [smallest lst] is smallest positive element of lst. Raises Not_found 
exception if no positive number is found. *)
let smallest l =
  let rec smallest_inner max_val found l' =
    match l' with
    | [] -> if not found then raise Not_found else max_val
    | h :: t ->
        if h < max_val && h >= 1 then smallest_inner h true t
        else smallest_inner max_val found t
  in
  smallest_inner max_int false l

(** [smallest_or_zero l] is smallest positive integer in l. Returns 0 if none found.*)
let smallest_or_zero l = try smallest l with Not_found -> 0

let _ = assert (smallest_or_zero [ -1; -2; -3 ] = 0)
let _ = assert (smallest_or_zero [ 1; -1; -2; -3 ] = 1)

exception Negative of string

let rec sqrt_inner n root =
  if root * root > n then root - 1 else sqrt_inner n (root + 1)

(** [sqrt num] is int closest to square root of num.*)
let sqrt n =
  if n < 0 then raise (Negative "negative argument") else sqrt_inner n 1

let find_root n = try sqrt n with Negative "negative argument" -> 0
let _ = assert (find_root 24 = 4)
let _ = assert (find_root (-24) = 0)
