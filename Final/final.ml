(* Student name:    *)
(* CMS cluster login name:   *)

(* final.ml: 2016 CS 4 final exam. *)

open Printf         (* for printf and sprintf functions *)
open Final_helpers  (* for utility types/functions/values *)

(* ---------------------------------------------------------
 * Piece type.
 * This could be inferred, but defining it explicitly
 * makes the .mli file much easier to understand.
 * --------------------------------------------------------- *)

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

(* ---------------------------------------------------------
 * Piece object.
 * --------------------------------------------------------- *)

let make_piece _name _len _dir =
  (* -------------------------------------------------------
   * Helper functions.
   * ------------------------------------------------------- *)

  (* Print a representation of the piece to the terminal.
   * For debugging only. *)
  let print_piece locs =
    begin
      printf "PIECE[ name: %s len: %d dir: %s\n"
        _name (int_of_len _len) (string_of_dir _dir);
      printf "       locs: %s ]\n" (string_of_locs locs)
    end
  in

  (* 
   * Return a list (option) of the locations in a line starting from the
   * location "loc" and going "len" spaces in direction "dir".
   * If any such location location would be off the board, return None.
   *)
  let get_locs_in_line loc len dir = 
    (* TODO *)
  in

  (*
   * Move the locations n places over in the given direction and return
   * a list (option) of the new locations.  Return None if the locations
   * can't be moved because they would go off the edge of the board.
   *)
  let move_locs locs dir n =
    (* TODO *)
  in

  (*
   * Given a (non-empty) list of locations, a direction and a distance, 
   *   return a tuple (option) of:
   * (a) the newly-vacated locations when you move the locations n places over 
   *     in the  given direction;
   * (b) the vacant locations passed over by the piece during the move
   * (c) the newly-occupied locations when you move the locations n places over 
   *     in the given direction
   * OR: None if the move would be invalid on an empty board 
   *     (because it would go off an end of the board).
   *)
  let if_move_locs locs dir n =
    (* TODO *)
  in

  (* 
   * Given a list of locations and a direction, compute all the possible 
   * integer displacements that would constitute a move on an empty board.
   * Assume the list of locations is valid i.e. it represents the locations
   * of a piece (contiguous in the horizontal or vertical direction).
   * Note that 0 is not a valid move, since a move must change the position
   * of the piece.
   *)
  let get_possible_moves locs dir =
    (* TODO *)
  in
     
  (* 
   * The state variable _loc stores the first location of the piece:
   * the leftmost location for a horizontally-oriented piece or the
   * topmost location for a vertically-oriented piece.
   *)
  let _loc = ref (0, 0) in
    object (self)
      method name  = _name
      method len   = _len
      method dir   = _dir
      method loc   = !_loc
      method locs  = (* TODO *)
      method print = print_piece self#locs

      method if_place loc =
        (* TODO *)

      method place new_loc =
        (* TODO *)

      method if_move n =
        (* TODO *)

      method move n =
        (* TODO *)

      method possible_moves = 
        (* TODO *)
    end

(* ---------------------------------------------------------
 * Board object.
 * --------------------------------------------------------- *)

let make_board () =
  let pieces = Hashtbl.create 25 in              (* piece name -> piece *)
  let piece_at_location = Hashtbl.create 36 in   (* location -> piece name *)

  (*
   * Helper functions.
   *)

  (* 
   * Convert the board contents to a list of lists of strings representation,
   * for debugging purposes.
   *)
  let board_to_list () =
    (* TODO *)
  in

  (* Clear the hash tables. *)
  let clear_board () =
    (* TODO *)
  in

  (* Print a representation of the board. *)
  let print_board () =
    begin
      printf "\n     0 1 2 3 4 5\n";
      printf "   +-------------+\n";
      List.iter
        (fun r ->  (* row *)
           begin
             printf " %d | " r;
             List.iter
               (fun c ->  (* column *)
                  let s = 
                    try
                      Hashtbl.find piece_at_location (r, c)
                    with 
                      Not_found -> "_"
                  in printf "%s " s)
               (range 0 board_size);
             if r = 2   (* row 2: where the blue block exits *)
               then printf "\n"  (* leave a "space" *)
               else printf "|\n"
           end)
        (range 0 board_size);
      printf "   +-------------+\n\n"
    end
  in

  (* 
   * Return true if the game has been won i.e. if the blue piece 
   * (piece "@") exists on the board and is positioned at locations 
   * (2, 4) and (2, 5).
   *)
  let board_is_won_game () =
    (* TODO *)
  in

  (* Return true if a location is vacant. *)
  let is_vacant loc =
     (* TODO *)
  in

  (* Vacate a location on the board by adjusting the hash table contents. *)
  let vacate loc = 
     (* TODO *)
  in
    
   (*
    * Add a binding between a location and a piece by adjusting the 
    * hash table contents.
    *)
  let occupy name loc = 
    (* TODO *)
  in
    
  (*
   * Make a piece with attributes (name, len, dir) and place it
   * on the board.
   *)
  let board_place_piece name len dir loc =
    (* TODO *)
  in

  (*
   * Move a piece (represented by the piece name) on the board 
   * by the specified distance.
   *)
  let board_move_piece name distance =
    (* TODO *)
  in

  (*
   * Return a list of all the possible moves by a particular piece
   * (represented as the piece name).  Assumes the piece is on the board.
   *)
  let get_all_moves name =
    (* TODO *)
  in

  (*
   * Initialize the board using a list of lists of dimensions 6x6.
   * This assumes that the blue piece is on the correct row.
   * Supplied to the students.
   *)
  let board_initialize_from_list lst =
    (*
     * Return a list of all the (putative) pieces in a list of strings,
     * where each piece is represented as: (name, length, starting_position).
     * "starting_position" is the index of the leftmost location in the list.
     *)
     let get_pieces lst =
       let rec iter lst index results =
         match lst with 
           | [] -> results
           | h :: t ->
               let p = takeWhile (fun s -> s = h) lst in
               let len = List.length p in
                 if len = 1   (* has to be at least 1 *)
                   then iter t (index + 1) results
                   else iter (drop len lst) 
                             (index + len) 
                             ((h, len, index) :: results)
       in iter lst 0 []
     in

     (* 
      * Place all the horizontal or vertical pieces onto the board.
      * loc_maker is a function which takes the row or column coordinate 
      * extracted from the get_pieces call (coord) and converts it into 
      * a location by adding the other coordinate.
      *)
     let place_line lst dir loc_maker =
       List.iter
         (fun (name, len, coord) ->
            (* place the piece onto the board unless the name is "_" *)
            match () with
              | _ when valid_piece_name name && valid_piece_length len ->
                  let loc = loc_maker coord in           (* make the location *)
                  let len' = len_of_int len in
                    board_place_piece name len' dir loc  (* place the piece   *)
              | _ when name = "_" -> ()  (* blank space; do nothing *)
              | _ -> failwith 
                       ("board: initialize_from_list: "
                        ^ "invalid piece name or length"))
         (get_pieces lst)
     in

     (* Place all the horizontal pieces onto the board. *)
     let place_horizontal_pieces lst =
       List.iter
         (fun row ->
           place_line (List.nth lst row) H (fun col -> (row, col)))
         (range 0 board_size)
     in

     (* Place all the vertical pieces onto the board. *)
     let place_vertical_pieces lst =
       let lst' = transpose lst in
       List.iter
         (fun col ->
           place_line (List.nth lst' col) V (fun row -> (row, col)))
         (range 0 board_size)
     in

     (* 
      * The initializing list argument must be a list of lists of 
      * dimensions 6x6. Each sublist must contain only valid piece 
      * names, or "_" for empty locations.
      *)
     let valid_initializing_list lst =
       (List.length lst = board_size)
       &&
       (List.for_all (fun l -> List.length l = board_size) lst)
       &&
       (List.for_all
         (fun s -> valid_piece_name s || s = "_")
         (List.flatten lst))
     in

     begin
       clear_board ();
       if not (valid_initializing_list lst)
         then failwith "invalid initializing list"
         else
           begin
             place_horizontal_pieces lst;
             place_vertical_pieces lst
           end
     end
  in

  object (self)
    (* For testing/debugging only. *)
    method pieces_table = Hashtbl.copy pieces

    (* For testing/debugging only. *)
    method piece_at_location_table = Hashtbl.copy piece_at_location

    (* For testing/debugging only. *)
    method to_list = (* TODO *)

    method clear = 
      (* TODO *)

    method print = print_board ()

    method won_game = 
      (* TODO *)

    method place_piece name len dir loc = 
      (* TODO *)

    method move_piece name distance = 
      (* TODO *)

    method get_all_piece_moves = 
      (* TODO *)
     
    method initialize_from_list lst = 
      (* TODO *)
  end

