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
