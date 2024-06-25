module type Map = sig
  type ('k, 'v) t
  (** [('k 'v) t] is the type of maps that bind keys of type ['k]
    to values of type ['v]. *)

  val empty : ('k, 'v) t
  (** [empty] does not bind any keys. *)

  val insert : 'k -> 'v -> ('k, 'v) t -> ('k, 'v) t
  (** [insert k v m] is the map that binds [k] to [v], and also contains
    all the bindings of [m]. If [k] was already bound in [m], that old 
    binding is superseded by the binding to [v] in the returned map. *)

  val lookup : 'k -> ('k, 'v) t -> 'v
  (** [lookup k m] is the value bound to [k] in [m]. Raises: [Not_found] if [k]
    is not bound in [m]. *)

  val bindings : ('k, 'v) t -> ('k * 'v) list
  (** [bindings m] is an association list containing the same bindings as [m].
    The keys in the list are guaranteed to be unique. *)
end

module AssocListMap : Map = struct
  (** The list [(k1,v1); ...; (kn,vn)] binds key [ki] to value [vi].
  If a key appears more than once in the list, it is bound to the 
  left-most occurrence in the list. *)

  type ('k, 'v) t = ('k * 'v) list

  let empty = []
  let insert k v m = (k, v) :: m
  let lookup k m = Stdlib.List.assoc k m
  let keys m = Stdlib.List.(m |> map fst |> sort_uniq Stdlib.compare)
  let bindings m = m |> keys |> Stdlib.List.map (fun k -> (k, lookup k m))
end

module type DirectAddressMap = sig
  type 'v t
  (** [t] is the type of maps that bind keys of type int to values of
  type ['v]. *)

  val insert : int -> 'v -> 'v t -> unit
  (** [insert k v m] mutates map [m] to bind [k] to [v]. If [k] was 
    already bound in [m], that binding is replaced by the binding to
    [v] in the new map. Requires: [k] is in bounds for [m]. *)

  val find : int -> 'v t -> 'v option
  (** [find k m] is [Some v] if [k] is bound to [v] in [m], and [None]
    if not. Requires: [k] is in bounds for [m]. *)

  val remove : int -> 'v t -> unit
  (** [remove k m] mutates [m] to remove any binding of [k]. If [k] was 
    not bound in [m], then map is unchanged. Requires: [k] is in bounds
    for [m]. *)

  val create : int -> 'v t
  (** [create c] creates a map with capacity [c]. Keys [0] through [c-1]
    are _in bounds_ for the map. *)

  val of_list : int -> (int * 'v) list -> 'v t
  (** [of_list c lst] is a map containing the same bindings as 
    association list [lst] and with capacity [c]. Requires: [lst] does
    not contain any duplicate keys, and every key in [lst] 
    is in bounds for capacity. *)

  val bindings : 'v t -> (int * 'v) list
  (** [bindings m] is an association list containing the same bindings
    as [m]. There are no duplicate keys in the list. *)
end

module ArrayMap : DirectAddressMap = struct
  type 'v t = 'v option array
  (** AF: [[|Some v0; Some v1; ... |]] represents {0 : v0, 1 : v1, ...}.
    If element [i] of the array is instead [None], then [i] is not bound 
    in the map. RI: None. *)

  (** Efficiency O(1). *)
  let insert k v m = m.(k) <- Some v

  (** Efficiency O(1). *)
  let find k m = m.(k)

  (** Efficiency O(1). *)
  let remove k m = m.(k) <- None

  (** Efficiency O(c). *)
  let create c = Array.make c None

  (** Efficiency O(c). *)
  let of_list c lst =
    let map' = create c in
    Stdlib.List.iter (fun (k, v) -> insert k v map') lst;
    map'

  (** Efficiency O(c). *)
  let bindings m =
    let lst = ref [] in
    (* for i = 0 to Array.length m - 1 do *)
    (*   match m.(i) with None -> () | Some v -> lst := (i, v) :: !lst *)
    (* done; *)
    (* !lst *)
    let add_binding k v =
      match v with None -> () | Some v' -> lst := (k, v') :: !lst
    in
    Array.iteri add_binding m;
    !lst
end

module type TableMap = sig
  type ('k, 'v) t
  (** [('k, 'v) t] is the type of mutable table-based maps that bind
    keys of type ['k] to values of type ['v]. *)

  val insert : 'k -> 'v -> ('k, 'v) t -> unit
  (** [insert k v m] mutates map [m] to bind [k] to [v]. If [k] was 
    already bound in [m], that binding is replaced by the binding to
    [v]. *)

  val find : 'k -> ('k, 'v) t -> 'v option
  (** [find k m] is [Some v] if [m] binds [k] to [v], and [None] if [m]
    does not bind [k]. *)

  val remove : 'k -> ('k, 'v) t -> unit
  (** [remove k m] mutates [m] to remove any binding of [k]. If [k] was
    not bound in [m], the map is unchanged. *)

  val create : ('k -> int) -> int -> ('k, 'v) t
  (** [create hash c] creates a new table map with capacity [c] that
    will use [hash] as the function to convert keys to integers. 
      Requires: The output of [hash] is always non-negative, and [hash]
    runs in constant time. *)

  val bindings : ('k, 'v) t -> ('k * 'v) list
  (** [bindings m] is an associated list containing the same bindings
    as [m]. *)

  val of_list : ('k -> int) -> ('k * 'v) list -> ('k, 'v) t
  (** [of_list hash lst] creates a map with the same bindings as [lst],
    using [hash] as the hash function. Requires: [lst] does not 
    contain any duplicate keys. *)
end

module HashMap : TableMap = struct
  type ('k, 'v) t = {
    hash : 'k -> int;
    mutable size : int;
    mutable buckets : ('k * 'v) list array;
  }
  (** AF: If [buckets] is 
      [| [(k11,v11); (k12,v12); ...]
         [(k21,v21); (k22,v22); ...] 
         ... |]
      that represents the map 
      {k11:v11, k12:v12, ...,
       k21:v21, K22:v22, ..., ...}.
      RI: No key appears more than once in the array (so, no
      duplicate keys in association lists.) All keys are 
      in the right buckets: if [k] is in [buckets] at index
      [b] then [hash k = b]. The output of [hash] must always
      be non-negative. [hash] must run in constant time. *)

  (** [capacity tab] is the number of buckets in [tab]. 
      Efficiency: O(1) *)
  let capacity { buckets; _ } = Array.length buckets

  (** [index k tab] is the index at which key [k] should be stored in the 
    buckets of [tab].
  Efficiency: O(1). *)
  let index k tab = tab.hash k mod capacity tab

  (** [insert_no_resize k v tab] inserts a binding from [k] to [v] in [tab]
    and does not resize the table, regardless of what happens to the 
      load factor.
  Efficiendy: expected O(L). *)
  let insert_no_resize k v tab =
    let b = index k tab in
    (* O(1) *)
    let old_bucket = tab.buckets.(b) in
    tab.buckets.(b) <- (k, v) :: Stdlib.List.remove_assoc k old_bucket;
    (* O(L) *)
    if not (Stdlib.List.mem_assoc k old_bucket) then tab.size <- tab.size + 1;
    ()

  (** [load_factor tab] is the load factor of [tab]. i.e., the number of
      bindings divided by the number of buckets. *)
  let load_factor tab = float_of_int tab.size /. float_of_int (capacity tab)

  (** [rehash tab new_capacity] replaces the buckets array of [tab] with a new
      array of size [new_capacity], and re-inserts all the bindings of [tab]
      into the new array. The keys are re-hashed, so the bindings will likely 
      land in different buckets. 
      Efficiency: O(n), where n is the number of bindings. *)
  let rehash tab new_capacity =
    (* insert [(k,v)] into [tab] *)
    let rehash_binding (k, v) = insert_no_resize k v tab in
    (* insert all bindings of bucket into [tab] *)
    let rehash_bucket bucket = Stdlib.List.iter rehash_binding bucket in
    let old_buckets = tab.buckets in
    tab.buckets <- Array.make new_capacity [];
    tab.size <- 0;
    (* O(n) *)
    Array.iter rehash_bucket old_buckets

  (** [resize_if_needed tab] resizes and rehashes [tab] if the load factor
      is too big or too small. Load factors are allowed to range from 1/2 to 2. *)
  let resize_if_needed tab =
    let lf = load_factor tab in
    if lf > 2.0 then rehash tab (capacity tab * 2)
    else if lf < 0.5 then rehash tab (capacity tab / 2)
    else ()

  (** Efficiency: O(n). *)
  let insert k v tab =
    insert_no_resize k v tab;
    (* O(L) *)
    resize_if_needed tab (* O(n) *)

  (** Efficiency: expected O(L). *)
  let find k tab = Stdlib.List.assoc_opt k tab.buckets.(index k tab)

  (** [remove_no_resize k tab] removes [k] from [tab] and does not trigger
    a resize, regardless of what happens to the load factor.
    Efficiency: expected O(L). *)
  let remove_no_resize k tab =
    let b = index k tab in
    let old_bucket = tab.buckets.(b) in
    tab.buckets.(b) <- Stdlib.List.remove_assoc k old_bucket;
    if Stdlib.List.mem_assoc k old_bucket then tab.size <- tab.size - 1;
    ()

  (** Efficiency: O(n) *)
  let remove k tab =
    remove_no_resize k tab;
    (* O(L) *)
    resize_if_needed tab
  (* O(n) *)

  (** Efficiency: O(n) *)
  let create hash n = { size = 0; hash; buckets = Array.make n [] }

  (* let bindings tab = *)
  (*   let lst = ref [] in *)
  (*   let process_binding (k, v) = lst := (k, v) :: !lst in *)
  (*   let process_bucket bucket = Stdlib.List.iter process_binding bucket in *)
  (*   Array.iter process_bucket tab.buckets *)

  (** Efficiency: O(n). *)
  let bindings tab =
    let accumulate_lst acc bucket =
      Stdlib.List.fold_left (fun acc' pair -> pair :: acc') acc bucket
    in
    Array.fold_left accumulate_lst
      (* (fun acc bucket -> *)
      (*   Stdlib.List.fold_left (fun acc' kvpair -> kvpair :: acc') acc bucket) *)
      [] tab.buckets

  (** Efficiency O(n^2). *)
  let of_list hash lst =
    let open Stdlib in
    let m = create hash (List.length lst) in
    (* O(n) *)
    List.iter (fun (k, v) -> insert k v m) lst;
    (* n * O(n) is O(n^2) *)
    m
end
