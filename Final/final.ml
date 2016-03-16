(* Student name: Joanne Li *)
(* CMS cluster login name: jli9 *)

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
    let (a, b) = loc in
    let _max = if (dir = H) then (b + len) else (a + len) in
    let _min = if (dir = H) then b else a in
    let (max, min) = if _max > _min then (_max, _min) else (_min, _max) in
    let result = ref [] in
      if max >= board_size || min < 0 then None else
      begin
        for i = max downto min do
          if (dir = H) then (result := ((a, i) :: !result))
          else (result := ((i, b) :: !result))
        done;
        Some !result
      end
  in

  (*
   * Move the locations n places over in the given direction and return
   * a list (option) of the new locations.  Return None if the locations
   * can't be moved because they would go off the edge of the board.
   *)
  let move_locs locs dir n =
    let (a, b) = (List.hd locs) in
    let len = List.length locs in
    let res = if (dir = H) then (b + n) else (a + n) in
    let res2 = if (dir = H) then (b + len - 1 + n) else (a + len - 1 + n) in
      if res >= board_size || res < 0 
         || res2 >= board_size || res2 < 0 then None else
      Some (List.map
            (fun (a, b) -> if (dir = H) then (a, b + n) else (a + n, b))
            locs)
  in

  (*
   * Given a (non-empty) list of locations, a direction and a distance, 
   *   return a tuple (option) of:
   * (a) the newly-vacated locations when you move the locations n places over 
   *     in the given direction;
   * (b) the vacant locations passed over by the piece during the move
   * (c) the newly-occupied locations when you move the locations n places over 
   *     in the given direction
   * OR: None if the move would be invalid on an empty board 
   *     (because it would go off an end of the board).
   *)
  let if_move_locs locs dir n =
    let (a, b) = (List.hd locs) in
    let get = function
      | None -> []
      | Some x -> x
    in
    let line_locs = get (get_locs_in_line (a, b) n dir) in
    let moved_locs = get (move_locs locs dir n) in
      if (line_locs = []) || (moved_locs = []) then None else
      Some (difference locs moved_locs,
            difference (difference line_locs locs) moved_locs,
            difference moved_locs locs)
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
    let result = ref [] in
      for i = (0 - board_size + 1) to (board_size - 1) do
        if (if_move_locs locs dir i) = None || i = 0 then () else
        result := (i :: !result)
      done;
      !result
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
      method locs  =
        match (self#len, self#dir, self#loc) with
          | (P2, H, (a, b)) -> [(a, b); (a, b + 1)]
          | (P3, H, (a, b)) -> [(a, b); (a, b + 1); (a, b + 2)]
          | (P2, V, (a, b)) -> [(a, b); (a + 1, b)]
          | (P3, V, (a, b)) -> [(a, b); (a + 1, b); (a + 2, b)]
      method print = print_piece self#locs

      method if_place loc =
        match (self#len, self#dir, loc) with
          | (_, _, (a, b)) when not (valid_loc (a, b)) ->
              invalid_arg
                (sprintf "piece: if_place: invalid location: (%d, %d)" a b)
          | (P2, H, (a, b)) -> if (b + 1) >= board_size then None else
              Some [(a, b); (a, b + 1)]
          | (P2, V, (a, b)) -> if (a + 1) >= board_size then None else
              Some [(a, b); (a + 1, b)]
          | (P3, H, (a, b)) -> if (b + 2) >= board_size then None else
              Some [(a, b); (a, b + 1); (a, b + 2)]
          | (P3, V, (a, b)) -> if (a + 2) >= board_size then None else
              Some [(a, b); (a + 1, b); (a + 2, b)]

      method place new_loc = (_loc := new_loc)

      method if_move n = if_move_locs self#locs self#dir n

      method move n =
        match (self#dir, self#loc) with
          | (H, (a, b)) -> _loc := (a, b + n)
          | (V, (a, b)) -> _loc := (a + n, b)

      method possible_moves = get_possible_moves self#locs self#dir
    end


(* ---------------------------------------------------------
 * Board object.
 * --------------------------------------------------------- *)
(*
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
    let result = ref [] in
    let inner = ref [] in
      for i = (board_size - 1) downto 0 do
        inner := [];
        for j = (board_size - 1) downto 0 do
          if Hashtbl.mem pieces_at_location (i, j) then
          inner := ((Hashtbl.find (i, j)) :: !inner)
          else inner := ("_" :: !inner)
        done;
        result := (!inner :: !result)
      done;
      !result
  in

  (* Clear the hash tables. *)
  let clear_board () =
    begin
      Hashtbl.clear pieces;
      Hashtbl.clear piece_at_location;
    end
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
    if (Hashtbl.mem pieces "@") && (Hashtbl.find pieces "@")#loc = (2, 4)
    then true else false
  in

  (* Return true if a location is vacant. *)
  let is_vacant loc =
     if (Hashtbl.mem piece_at_location loc) then false else true
  in

  (* Vacate a location on the board by adjusting the hash table contents. *)
  let vacate loc =
    let piece = ref (Hashtbl.find loc) in
    begin
      Hashtbl.remove piece_at_location loc;
      Hashtbl.remove pieces !piece
    end
  in
    
   (*
    * Add a binding between a location and a piece by adjusting the 
    * hash table contents.
    *)
  let occupy name loc =
    Hashtbl.replace piece_at_location loc name
  in
    
  (*
   * Make a piece with attributes (name, len, dir) and place it
   * on the board.
   *)
  let board_place_piece name len dir loc =
    let piece = (make_piece name len dir)
    begin
      piece#place loc;
      Hashtbl.replace pieces name piece
      Hashtbl.replace piece_at_location loc name
    end
  in

  (*
   * Move a piece (represented by the piece name) on the board 
   * by the specified distance.
   *)
  let board_move_piece name distance =
    (Hashtbl.find pieces name)#move distance
  in

  (*
   * Return a list of all the possible moves by a particular piece
   * (represented as the piece name).  Assumes the piece is on the board.
   *)
  let get_all_moves name =
    (Hashtbl.find pieces name)#possible_moves
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
    method to_list = board_to_list ()

    method clear = clear_board ()

    method print = print_board ()

    method won_game = board_is_won_game ()

    method place_piece name len dir loc = 
      let (a, b) = loc in
      match () with
        | _ when not (valid_piece_name name)
        | _ when not (valid_loc loc) ->
            raise (Invalid_argument, "board: place_piece: bad inputs")
        | _ when ((make_piece name len dir)#if_place loc) = None ->
            failwith
              sprintf "board: cannot place piece %s at location (%d, %d)"
                name a b
        | _ -> board_place_piece name len dir loc

    method move_piece name distance = 
      (* TODO *)

    method get_all_piece_moves = 
      (* TODO *)
     
    method initialize_from_list lst = board_initialize_from_list lst
  end
*)
