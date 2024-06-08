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
