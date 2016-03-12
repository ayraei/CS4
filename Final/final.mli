(* final.mli: interface file for final.ml *)

open Final_helpers

type piece =
  < 
    name     : string; 
    len      : len;
    dir      : dir;
    loc      : loc; 
    locs     : loc list; 
    print    : unit;
    if_place : loc -> loc list option; 
    place    : loc -> unit; 
    if_move  : int -> (loc list * loc list * loc list) option;
    move     : int -> unit;
    possible_moves : int list
  >

val make_piece : string -> len -> dir -> piece

val make_board :
  unit ->
  < 
    pieces_table : (string, piece) Hashtbl.t;
    piece_at_location_table : (loc, string) Hashtbl.t;
    to_list : string list list; 

    clear : unit; 
    print : unit; 
    won_game : bool;

    place_piece : string -> len -> dir -> loc -> unit;
    move_piece : string -> int -> unit;
    get_all_piece_moves : (string * int) list;

    initialize_from_list : string list list -> unit
  >

