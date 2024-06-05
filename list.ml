open StdLabels

(** sum[lst] sums elements of the list. Requires lst be of type int*)
let rec sum lst = match lst with [] -> 0 | h :: t -> h + sum t

let () = assert (sum [ 1; 2; 3 ] = 6)
let () = assert (sum [] = 0)

(** length [lst] produces count of elements in lst *)
let rec length = function [] -> 0 | _ :: t -> 1 + length t

let () = assert (length [ 1; 2; 3 ] = 3)
let () = assert (length [] = 0)

(** append[lst1 lst2] appends elems of lst1 in front of lst2. Usage 
    append [1;2;3] [1;2;3] -> [1;2;3;1;2;3]*)
let rec append lst1 lst2 =
  match lst1 with [] -> lst2 | h :: t -> h :: append t lst2

let () = assert (append [ 1; 2 ] [ 1; 2 ] = [ 1; 2; 1; 2 ])

(** empty[lst] returns true if lst is empty false otherwise*)
let empty = function [] -> true | _ :: _ -> false

let () = assert (empty [])
let () = assert (not (empty [ 1; 2; 3 ]))
(* let rec from i j lst = if i > j then lst else i :: from (i + 1) j lst *)

(** [evens lst] is list of even indexed elemnets of lst*)
let rec evens = function _ :: snd :: t -> snd :: evens t | _ -> []

let () = assert (evens [ 1; 2; 3; 4 ] = [ 2; 4 ])
let () = assert (evens [ 1; 2; 3 ] = [ 2 ])

(** [count_true lst] is the number of [true] values in lst*)
let rec count_true = function
  | [] -> 0
  | true :: t -> 1 + count_true t
  | false :: t -> count_true t
(* | h :: t -> (if h then 1 else 0) + count_true t *)

let () = assert (count_true [] = 0)
let () = assert (count_true [ false; true; true ] = 2)
let () = assert (count_true [ false ] = 0)

(** [count_true_tr lst] tail recursive implementation of count_true*)
let count_true_tr =
  let rec helper acc = function
    | [] -> acc
    | true :: t -> helper (1 + acc) t
    | false :: t -> helper acc t
    (* | h :: t -> helper (if h then acc + 1 else acc) t *)
  in
  helper 0

let () = assert (count_true_tr [] = 0)
let () = assert (count_true_tr [ false; true; true ] = 2)
let () = assert (count_true_tr [ false ] = 0)

(* let build_palindrome lst = lst @ List.rev lst *)

(** [build_palindrome lst] is a palindrome made of lst *)
let build_palindrome lst =
  let rec drop_last = function
    | [ _ ] | [] -> []
    | h :: t -> h :: drop_last t
  in
  lst @ List.rev (drop_last lst)

(** [ ispalindrome lst] is true if lst is palindrome. *)
let ispalindrome lst = lst = List.rev lst

let _ = assert (ispalindrome [ 1 ])
let _ = assert (ispalindrome [])
let _ = assert (ispalindrome [ 1; 2; 3; 2; 1 ])
let _ = assert (ispalindrome (build_palindrome [ 1; 2 ]))
let _ = assert (not (ispalindrome [ 1; 3; 2; 1 ]))

(** [drop_last_tr lst] lst without the last element. Returns empty list if lst is empty*)
let drop_last_tr =
  let rec drop_last_tr_helper acc = function
    | [ _ ] | [] -> List.rev acc
    | h :: t -> drop_last_tr_helper (h :: acc) t
  in
  drop_last_tr_helper []

let _ = assert (drop_last_tr [ 1; 2; 3 ] = [ 1; 2 ])
let _ = assert (drop_last_tr [] = [])

(** [member elem lst] is true if elem is in lst false otherwise *)
let rec member elem = function
  | [] -> false
  | h :: t -> h = elem || member elem t

let _ = assert (member 2 [ 1; 2; 3 ])
let _ = assert (not (member 2 [ 1; 3 ]))

(** [make_set lst] is a set of lst. *)
let rec make_set lst =
  match lst with
  | [] -> []
  | h :: t -> if member h t then make_set t else h :: make_set t

let _ = assert (make_set [ 1; 2; 2; 3; 1 ] = [ 2; 3; 1 ])
let _ = assert (make_set [ 3; 2; 3; 1; 5; 5; 4 ] = [ 2; 3; 1; 5; 4 ])

(** [rev lst] is reversed lst.*)
let rev =
  let rec rev_helper acc = function
    | [] -> acc
    | h :: t -> rev_helper (h :: acc) t
  in
  rev_helper []

let _ = assert (rev [ 1; 2; 3 ] = [ 3; 2; 1 ])
