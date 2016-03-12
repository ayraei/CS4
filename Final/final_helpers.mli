(* final_helpers.mli *)

type dir = H | V
type len = P2 | P3
type loc = int * int

val board_size : int

val string_of_dir : dir -> string
val int_of_len : len -> int
val len_of_int : int -> len
val valid_index : int -> bool
val valid_loc : loc -> bool
val move_loc : loc -> int -> int -> loc option
val string_of_loc : loc -> string
val string_of_locs : loc list -> string
val mapMaybe : ('a -> 'b option) -> 'a list -> 'b list option
val range : int -> int -> int list
val min_list : 'a list -> 'a
val max_list : 'a list -> 'a
val difference : 'a list -> 'a list -> 'a list
val betweens : int list -> int list -> int list
val valid_piece_name : string -> bool
val valid_piece_length : int -> bool
val hash_keys : ('a, 'b) Hashtbl.t -> 'a list
val drop : int -> 'a list -> 'a list
val takeWhile : ('a -> bool) -> 'a list -> 'a list
val transpose : 'a list list -> 'a list list
val make_board_list_from_string : string -> string list list 

