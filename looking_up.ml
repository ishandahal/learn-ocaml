open StdLabels

(** [keys_count d] is count of unique keys in [d] dictionary.*)
let rec keys_count = function [] -> 0 | _ :: t -> 1 + keys_count t

let () = assert (keys_count [ (1, 2); (2, 3); (3, 4) ] = 3)

(** [replace k v d] is d with value of k replaced by v. Raises Not_found if k not found.*)
let rec replace k v = function
  | [] -> raise Not_found
  | (k', v') :: t -> if k' = k then (k', v) :: t else (k', v') :: replace k v t

(** [build_dict l1 l2] is a dictionary with sequential value of l1 as key and l2 as value.*)

let build_dict (l1 : 'a list) (l2 : 'b list) =
  let rec build_dict_inner (l1 : 'a list) (l2 : 'b list) d =
    match (l1, l2) with
    | [], [] -> d
    | [], _ -> raise (Invalid_argument "lists are unequal")
    | _, [] -> raise (Invalid_argument "lists are unequal")
    | h1 :: t1, h2 :: t2 -> build_dict_inner t1 t2 ((h1, h2) :: d)
  in
  build_dict_inner l1 l2 []

(** [inverse d] is a pair of lists one containing key and another value of d.*)
let inverse d =
  let rec inverse_inner d' l1 l2 =
    match d' with
    | [] -> (l1, l2)
    | (k, v) :: t -> inverse_inner t (k :: l1) (v :: l2)
  in
  inverse_inner d [] []

(** [mk_lists d] same as inverse but different implemtation approach.*)
let rec mk_lists d =
  match d with
  | [] -> ([], [])
  | (k, v) :: tail ->
      (* ( match mk_lists tail with k', v' -> (k :: k', v :: v')) *)
      let k', v' = mk_lists tail in
      (k :: k', v :: v')

(** [member key d] is true if key is key in [d] false otherwise.*)
let rec member key = function
  | [] -> false
  | (k, _) :: t -> if k = key then true else member key t

(** [member_l ele l] is true if ele in l false otherwise.*)
let rec member_l elem = function
  | [] -> false
  | h :: t -> if h = elem then true else member_l elem t

(** [union d1 d2] is a union of two dictionaries. For overlapping keys key
        value pairs in d1 prevail. *)
let rec union d1 d2 =
  match d2 with
  | [] -> d1
  | (k, v) :: more ->
      if member k d1 then union d1 more else (k, v) :: union d1 more

(** [member_all x ls] is true if x is member of all lists in ls. false otherwise*)
let member_all x ls =
  let booleans = List.map ~f:(member_l x) ls in
  not (member_l false booleans)

let () = assert (member_all 1 [ [ 1; 2 ]; [ 1; 3 ]; [ 1; 4 ] ])
let () = assert (not (member_all 1 [ [ 2 ]; [ 1; 3 ]; [ 1; 4 ] ]))

(** [div demon num] is / with num and denom switched positions.*)
let div denom num = num / denom

(** [mapll lls f] is map over list of list of list.*)
let mapll f lls = List.map ~f:(List.map ~f:(List.map ~f)) lls

(**[truncatel n l] is list l truncated to length n.*)
let rec truncate_l n l =
  match (n, l) with
  | 0, _ | _, [] -> []
  | a, _ when a < 0 -> raise (Invalid_argument "truncate")
  | _, h :: t -> h :: truncate_l (n - 1) t

(** [ truncate n ls] is list of lists truncated to length n.*)
let truncate n = List.map ~f:(truncate_l n)

(** [first_of_l default l] is the first element of list. default if list is empty.*)
let first_of_l default = function [] -> default | h :: _ -> h

(** [all_first_of_l default l] is list of first element of list of lists.
  default value is used if list is empty.*)
let all_first_of_l default = List.map ~f:(first_of_l default)
