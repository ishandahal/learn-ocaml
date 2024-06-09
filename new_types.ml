(** [rect] is rectangle with height and width.*)
type rect = Square of int | Rectangle of int * int

(** [area rect] area of rectangle.*)
let area rect = match rect with Rectangle (h, w) -> h * w | Square l -> l * l

let r = Rectangle (2, 3)
let s = Square 4
let _ = assert (area r = 6)
let _ = assert (area s = 16)

(** [rotate rect] is rect rotated such that it is at least as tall as it is wide.*)
let rotate rect =
  match rect with Rectangle (h, w) when h < w -> Rectangle (w, h) | _ -> rect

let _ = assert (rotate r = Rectangle (3, 2))
let _ = assert (rotate (Rectangle (3, 3)) = Rectangle (3, 3))
let _ = assert (rotate s = Square 4)

(** [sequence] is one of nil or a sequence. An emplementation of list.*)
type 'a sequence = Nil | Cons of 'a * 'a sequence

let empty = Nil
let one = Cons (0, Nil)
let _ = Cons ("apple", empty)
let two = Cons (2, Cons (1, Nil))
let four = Cons ([ 1; 2 ], Cons ([ 3; 4 ], Cons ([ 5 ], Cons ([], Nil))))

(** [length s] is length of sequence.*)
let rec length = function Nil -> 0 | Cons (_, t) -> 1 + length t

let _ = assert (length four = 4)
let _ = assert (length two = 2)

(** [append s1 s2] is s1 appended on to s2. Similar to [ @ ] operator in [list]).*)
let rec append s1 s2 =
  match s1 with Nil -> s2 | Cons (h, t) -> Cons (h, append t s2)

let _ = assert (append one two = Cons (0, Cons (2, Cons (1, Nil))))

(** [take n s] is sequence s with n elements. Returns whole sequence if 
  n is greater than length of s. Raises [Invalid_argument] for n < 0.*)
let take n s =
  let rec take_inner n s =
    match (n, s) with
    | 0, _ -> Nil
    | _, Nil -> s
    | _, Cons (h, t) -> Cons (h, take_inner (n - 1) t)
  in
  if n < 0 then raise (Invalid_argument "take")
  else if n > length s then s
  else take_inner n s

let _ = assert (take 1 Nil = Nil)
let _ = assert (take 1 one = one)
let _ = assert (take 1 two = Cons (2, Nil))

(** [drop n s] is sequence with first n elements dropped. Returns empty
    sequence for n > length of s. Raises [Invalid_argument] for n < 0.*)
let drop n s =
  let rec drop_inner n' s' =
    if n' = 0 then s'
    else match s' with Cons (_, t) -> drop_inner (n' - 1) t | Nil -> Nil
  in
  if n < 0 then raise (Invalid_argument "drop") else drop_inner n s

let _ = assert (drop 1 one = Nil)
let _ = assert (drop 2 two = Nil)
let _ = assert (drop 30 four = Nil)
let _ = assert (drop 3 four = Cons ([], Nil))

(** [arithmetic] is user defined type that represents arithmetic operations 
  on integers. *)
type arithmetic =
  | Number of int
  | Add of (arithmetic * arithmetic)
  | Sub of (arithmetic * arithmetic)
  | Mul of (arithmetic * arithmetic)
  | Div of (arithmetic * arithmetic)
  | Pow of (arithmetic * arithmetic)

let one = Number 1
let two = Number 2
let add1 = Add (one, two)
let mul1 = Mul (add1, add1)
let div1 = Div (two, two)

(** [evaluate proposition] is result of evaluating proposition.*)
let rec evaluate proposition =
  match proposition with
  | Number i -> i
  | Add (i, j) -> evaluate i + evaluate j
  | Sub (i, j) -> evaluate i - evaluate j
  | Mul (i, j) -> evaluate i * evaluate j
  | Div (i, j) -> evaluate i / evaluate j
  | Pow (i, j) ->
      let rec pow base exp =
        match evaluate exp with
        | 0 -> Number 1
        | n -> Mul (base, pow base (Sub (Number n, Number 1)))
      in
      evaluate (pow i j)

let _ = assert (evaluate (Add (one, one)) = evaluate two)
let _ = assert (evaluate (Pow (two, two)) = 2 * 2)
let _ = assert (evaluate mul1 = 9)

(** [evaluate_op e] is Some int after evaluating e. None is [ Division_by_zero ] error.*)
let evaluate_op e = try Some (evaluate e) with Division_by_zero -> None

let _ = assert (evaluate_op (Div (div1, Number 0)) = None)
let _ = assert (evaluate_op (Div (add1, Number 3)) = Some 1)

(* ============================================ *)

(**[tr] is tree data structure *)
type 'a tr = LF | Br of 'a * 'a tr * 'a tr

let t1 = Br (1, LF, LF)
let t2 = Br (3, Br (2, LF, LF), t1)
let t5 = Br (5, t2, t2)

(** [tr_length tr] is count of nodes in tree.*)
let rec tr_length = function
  | LF -> 0
  | Br (_, l, r) -> 1 + tr_length l + tr_length r

let () = assert (tr_length t2 = 3)
let () = assert (tr_length t1 = 1)

(**[max i j] is highest value between i and j.*)
let max i j = if i > j then i else j

(**[max_depth tr] is length of deepest branch from root of tree.*)
let rec max_depth = function
  | LF -> 0
  | Br (_, l, r) -> 1 + max (max_depth l) (max_depth r)

let _ = assert (max_depth t1 = 1)
let _ = assert (max_depth t2 = 2)
let _ = assert (max_depth t5 = 3)

(** [print_tree tr] prints values of each node. Requires tree of ints.*)
let rec print_tree = function
  | LF ->
      print_string " lf ";
      ""
  | Br (v, l, r) ->
      print_string " ";
      print_string (string_of_int v);
      print_string " ";
      print_tree l ^ print_tree r

let _ = print_tree t2
let _ = print_endline " "
let _ = print_tree t5
let _ = print_endline " "

(** [map_tr f tr] tree with f applied to every element of tr.*)
let rec map_tr f = function
  | LF -> LF
  | Br (e, l, r) -> Br (f e, map_tr f l, map_tr f r)

let _ = print_tree (map_tr (fun x -> x * x) t5)

(** [member elem tr] is true if elem is in tr; false otherwise.*)
let rec member elem = function
  | LF -> false
  | Br (e, l, r) -> if e = elem then true else member elem l || member elem r

let () = assert (member 2 t2)
let () = assert (member 5 t5)
let () = assert (not (member 5 t2))
let () = assert (not (member 15 t5))

(** [flip tr] is tree flipped such that it is mirror image to tr.*)
let rec flip = function LF -> LF | Br (e, l, r) -> Br (e, flip r, flip l)

let t3 = Br (10, t2, LF)
let _ = print_endline " "
let _ = print_tree t3
let _ = print_endline " "
let _ = print_tree (flip t3)

(** [same_shape t1 t2] is true if t1 and t2 are of same shape; false otherwise.*)
let rec same_shape t1 t2 =
  match (t1, t2) with
  | LF, LF -> true
  | Br (_, l1, r1), Br (_, l2, r2) -> same_shape l1 l2 && same_shape r1 r2
  | _, _ -> false

let t6 = Br (3, Br (3, LF, LF), t1)
let _ = assert (same_shape t2 t6)
let _ = assert (not (same_shape t1 t6))

(** [insert k tr] is BST with k inserted in the right place. 
    Does not insert repeat elements.*)
let rec insert k = function
  | LF -> Br (k, LF, LF)
  | Br (k', l, r) ->
      if k = k' then Br (k', l, r)
      else if k < k' then Br (k', insert k l, r)
      else Br (k', l, insert k r)

(** [tree_of_list t l] is BST with elements of l.*)
let rec tree_of_list tree = function
  | [] -> tree
  | h :: t -> tree_of_list (insert h tree) t
