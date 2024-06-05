open StdLabels

let () = print_endline "Solutions to exercises of chap 3"
let _ = [ 1; 2; 3; 4; 5 ]
let _ = [ 1; 2; 3; 4; 5 ]
let lst3 = [ 1 ] @ [ 2; 3; 4 ] @ [ 5 ]
let _ = assert (lst3 = [ 1; 2; 3; 4; 5 ])

(** product [lst] is a product of elements of lst. Result is 1 for empty lst *)
let rec product lst = match lst with [] -> 1 | hd :: tl -> hd * product tl

let () = assert (product [ 1; 2; 3 ] = 6)
let () = assert (product [] = 1)

(** concat[lst] is concatenation of elements in lst. Usage concat ["a";"b"] -> "ab".
  Empty lst produces empty string. *)
let rec concat lst = match lst with [] -> "" | h :: t -> h ^ concat t

let _ = assert (concat [ "a"; "b" ] = "ab")
let _ = assert (concat [] = "")

let rec has_bigred lst =
  match lst with
  | [] -> false
  | h :: t -> if h = "bigred" then true else has_bigred t

let () = assert (has_bigred [ "some"; "bigred" ])
let () = assert (not (has_bigred []))

let has_2_or_4_elems = function
  | [ _; _; _; _ ] -> true
  | [ _; _ ] -> true
  | _ -> false

let () = assert (has_2_or_4_elems [ 1; 2; 3; 4 ])
let () = assert (has_2_or_4_elems [ 1; 2 ])
let () = assert (not (has_2_or_4_elems [ 2 ]))
let equal_first_two = function one :: two :: _ -> one = two | _ -> false
let () = assert (equal_first_two [ 1; 1; 2 ])
let () = assert (equal_first_two [ 1; 1 ])

let pick_fifth lst =
  let length = List.length lst in
  if length < 5 then 0 else List.nth lst 4

let () = assert (pick_fifth [ 1; 2; 3; 4; 5 ] = 5)
let () = assert (pick_fifth [ 3; 4; 5 ] = 0)

(** any_zeros[lst] indicates whether any 0 is present is lst *)
let any_zeros lst = List.exists ~f:(fun x -> x = 0) lst

let () = assert (not (any_zeros [ 1; 2 ]))
let () = assert (any_zeros [ 1; 2; 0 ])
let () = assert (any_zeros [ 1; 2; 0; 0 ])
let rev_sort_list lst = lst |> List.sort ~cmp:compare |> List.rev

(** rev_lst [lst] is list of int sorted in descending order*)
(* let rev_sort_list lst = *)
(*   let sorted_lst = List.sort ~cmp:compare lst in *)
(*   List.rev sorted_lst *)

let () = assert (rev_sort_list [ 2; 1; 3 ] = [ 3; 2; 1 ])

(** get_last[lst] is last element of non empty list*)
let get_last lst = List.nth lst (List.length lst - 1)

(** take[lst n] is list of first n elements from lst. *)
let rec take lst n =
  if n = 0 then []
  else match lst with [] -> [] | hd :: tl -> hd :: take tl (n - 1)

(** drop[lst n] is list of elements from lst except first n. Result is empty
      if elements in lst is less than n. *)
let rec drop lst n =
  if n = 0 then lst else match lst with [] -> [] | _ :: tl -> drop tl (n - 1)

(* let rec from i j l = if i > j then l else i :: from (i + 1) j l *)

(** from [i j l] is a list of integers from i to j (inclusive) followed by list l*)
let rec from i j l = if i > j then l else from i (j - 1) (j :: l)

(** -- i j produces list of integers from i to j (inclusive)*)
let ( -- ) i j = from i j []

(** tail recursive implementation of take 
    [take_rev lst n acc] is [l @ acc] where l is the first n elements from lst
    in reverse order*)
let rec take_rev lst n acc =
  if n = 0 then acc
  else match lst with [] -> acc | h :: t -> take_rev t (n - 1) (h :: acc)

(** take[lst n] is list of first n elements from lst. *)
let take_tr lst n = take_rev lst n [] |> List.rev

(** [is_decreasing lst] is a list of ints that are monotonically decreasing *)
let rec is_decreasing lst =
  match lst with
  | [] | [ _ ] -> true
  | first :: (second :: _ as tl') -> second <= first && is_decreasing tl'

(** [is_mon_inc_then_dec lst] returns: whether input list is monotonically 
    increasing and then monotonically decreasing*)
let rec is_mon_inc_then_dec = function
  | [] | [ _ ] -> true
  | first :: (second :: _ as tl) as tl' ->
      if first <= second then is_mon_inc_then_dec tl else is_decreasing tl'

let is_unimodal lst = is_mon_inc_then_dec lst

(** [powerset s] is list containing set of all subsets of s.
      * requires [s] is setlike (with no duplicates) *)
let rec powerset s =
  match s with
  | [] -> [ [] ]
  | h :: t ->
      let p = powerset t in
      List.map ~f:(List.cons h) p @ p

(** [print_int_list lst] prints each element is a separate line *)
let rec print_int_list = function
  | [] -> ()
  | h :: t ->
      h |> string_of_int |> print_endline;
      print_int_list t

(** [print_int_list' lst] same as print_int_list implemented without recursion*)
let print_int_list' lst =
  List.iter ~f:(fun x -> x |> string_of_int |> print_endline) lst
(** ====================================== *)

type student = { first_name : string; last_name : string; gpa : float }

let create_student fname lname gpa =
  { first_name = fname; last_name = lname; gpa }

(** [get_name s] is a tuple of first and last name of s*)
let get_name s = (s.first_name, s.last_name)

type poketype = Normal | Fire | Water
type pokemon = { name : string; hp : int; ptype : poketype }

let charizard = { name = "charizard"; hp = 78; ptype = Fire }
let squirtle = { name = "squirtle"; hp = 44; ptype = Water }

(** [safe_hd lst : 'a list] returns Some x if head of lst is x None if empty *)
let safe_hd = function [] -> None | h :: _ -> Some h

(** [safe_tl lst: 'a list] return some tl if tl is the tail of lst None if empty *)
let safe_tl = function [] -> None | _ :: tl -> Some tl

(** [max_hp pok_lst] is pokemon option with highest hp. Returns None for empty list*)
let rec max_hp = function
  | [] -> None
  | poke1 :: tl -> (
      match max_hp tl with
      | None -> Some poke1
      | Some poke2 -> Some (if poke1.hp > poke2.hp then poke2 else poke1))

(* | first :: [] -> Some first *)
(* | first::second::tl ->  *)
(*   if first.hp < second.hp then Some (max_hd second::tl) else Some (max_hd first::tl) *)

type date_like = int * int * int

(** [is_before date1 date2] is true if date1 is before date2 in element wise comparison*)
let is_before date1 date2 =
  let y1, m1, d1 = date1 in
  let y2, m2, d2 = date2 in
  y1 < y2 || (y1 = y2 && m1 < m2) || (y1 = y2 && m1 = m2 && d1 < d2)

(** [earliest date_lst] is the earliest Some date_like or None if date_lst is empty*)
let rec earliest date_lst =
  match date_lst with
  | [] -> None
  | first :: tl -> (
      match earliest tl with
      | None -> Some first
      | Some second -> Some (if is_before first second then first else second))

(** [insert k v lst] inserts k v tuple into lst *)
let insert k v lst = (k, v) :: lst

(** [lookup k lst] is [Some v] if k in lst else None*)
let rec lookup k = function
  | [] -> None
  | (k', v') :: t -> if k = k' then Some v' else lookup k t

(** suit represents 4 suits in cards*)
type suit = Heart | Diamond | Spade | Club

type rank = int
(** rank represents 13 ranks Ace is 1 King is 13*)

type card = { suit : suit; rank : rank }
(** card represents single card*)

let ace_of_clubs = { suit = Club; rank = 1 }
let queen_of_hearts = { suit = Heart; rank = 12 }
let two_of_diamonds = { suit = Diamond; rank = 2 }

type quad = I | II | III | IV
type sign = Neg | Pos | Zero

(** [sign x] is [sign] of x*)
let sign (x : int) : sign = if x < 0 then Neg else if x > 0 then Pos else Zero

(** [quadrant (x, y)] is [Some quad] (x,y) belong to in 2d cartesian coordinate system.
   None if x,y fall on the axis *)
let quadrant : int * int -> quad option =
 fun (x, y) ->
  match (sign x, sign y) with
  | Pos, Pos -> Some I
  | Neg, Pos -> Some II
  | Neg, Neg -> Some III
  | Pos, Neg -> Some IV
  | _, _ -> None

(** Implementing quad using the `when` syntax*)
let quadrant_when : int * int -> quad option = function
  | x, y when x > 0 && y > 0 -> Some I
  | x, y when x < 0 && y > 0 -> Some II
  | x, y when x < 0 && y < 0 -> Some III
  | x, y when x > 0 && y < 0 -> Some IV
  | _ -> None

(** depth *)
type 'a tree = Leaf | Node of 'a * 'a tree * 'a tree

(** [depth t] is the number of nodes in any longest path from the root to
  the Leaf in [t]*)
let rec depth t =
  match t with
  | Leaf -> 0
  | Node (_, left, right) -> 1 + max (depth left) (depth right)

let upcase_first_entry line =
  match String.split_on_char ~sep:',' line with
  | [] -> assert false (* String.split returns at least one element *)
  | first :: rest ->
      String.concat ~sep:"," (String.uppercase_ascii first :: rest)

let some_func x y =
  let () = Printf.printf "value of x: %i\n" x in
  let () = Printf.printf "value of y: %i\n" y in
  x + y

let fact2 =
  let rec loop accum i = if i = 0 then accum else loop (i * accum) (i - 1) in
  loop 1
