open StdLabels

(** [insert elem lst] is elem inserted into appropriate position in sorted lst.*)
let rec insert elem = function
  | [] -> [ elem ]
  | h :: t -> if elem < h then elem :: h :: t else h :: insert elem t

(** [sort_lst lst] is sorted list. *)
let rec sort_lst = function [] -> [] | h :: t -> insert h (sort_lst t)

let _ = assert (sort_lst [ 2; 1; 3 ] = [ 1; 2; 3 ])

(** take[lst n] is list of first n elements from lst. *)
let rec take lst n =
  if n = 0 then []
  else match lst with [] -> [] | hd :: tl -> hd :: take tl (n - 1)

(** drop[lst n] is list of elements from lst except first n. Result is empty
      if elements in lst is less than n. *)
let rec drop lst n =
  if n = 0 then lst else match lst with [] -> [] | _ :: tl -> drop tl (n - 1)

(** [merge lst1 lst2] is merged sorted lst1 and lst2. *)
let rec merge lst1 lst2 =
  match (lst1, lst2) with
  | [], _ -> lst2
  | _, [] -> lst1
  | h1 :: t1, h2 :: t2 ->
      if h1 < h2 then h1 :: merge t1 lst2 else h2 :: merge lst1 t2

(** [merge_sort lst] merge sort implementation*)
let rec merge_sort lst =
  match lst with
  | [] -> []
  | [ _ ] -> lst
  | _ :: _ ->
      let mid = List.length lst / 2 in
      let left = take lst mid in
      let right = drop lst mid in
      merge (merge_sort left) (merge_sort right)

let _ = assert (merge_sort [] = [])
let _ = assert (merge_sort [ 2; 1; 3 ] = [ 1; 2; 3 ])
let _ = assert (merge_sort [ 9; 8; 7; 6; 5; 4 ] = [ 4; 5; 6; 7; 8; 9 ])

(** [insert_rev elem lst] is elem placed in appropriate location in reverse sorted lst*)
let rec insert_rev elem = function
  | [] -> [ elem ]
  | h :: t -> if elem > h then elem :: h :: t else h :: insert_rev elem t

(** [insertion_sort_rev lst] is lst sorted in reverse order *)
let rec insertion_sort_rev = function
  | [] -> []
  | h :: t -> insert_rev h (insertion_sort_rev t)

let _ = assert (insertion_sort_rev [] = [])
let _ = assert (insertion_sort_rev [ 1; 2; 3 ] = [ 3; 2; 1 ])
let _ = assert (insertion_sort_rev [ 2; 1; 3 ] = [ 3; 2; 1 ])

(** [is_sorted lst] is true if lst is sorted false otherwise. Returns true if lst is empty.*)
let rec is_sorted = function
  | [] -> true
  | [ _ ] -> true
  | f :: s :: t -> f <= s && is_sorted t

let _ = assert (is_sorted [ 1; 2; 3; 4 ] = true)
let _ = assert (not (is_sorted [ 2; 3; 4; 1 ] = true))

(** [sort lst] is sorted lst. Implementation where both functions are combined in one.*)
let sort =
  let rec insert_rev elem = function
    | [] -> [ elem ]
    | h :: t -> if elem < h then elem :: h :: t else h :: insert_rev elem t
  in
  let rec sort_helper = function
    | [] -> []
    | h :: t -> insert_rev h (sort_helper t)
  in
  sort_helper

let _ = assert (sort [] = [])
let _ = assert (sort [ 2; 1; 3 ] = [ 1; 2; 3 ])
let _ = assert (sort [ 9; 8; 7; 6; 5; 4 ] = [ 4; 5; 6; 7; 8; 9 ])
