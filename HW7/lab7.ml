(* Question 1 *)
(* The light object *)

type light_state = On | Off

(* Creates a single Light object. *)
let make_light () =
  let curr_state = ref Off in
  let neighbors = ref [] in
  let rec update_helper = function
    | h :: t ->
        begin
          h#toggle;
          update_helper t
        end
    | [] -> ()
  in
  object (self)
    method state = !curr_state
    method set_state st = curr_state := st
    method toggle = curr_state := (if self#state = On then Off else On)
    method add_neighbor ob = neighbors := (ob :: !neighbors)
    method update =
      begin
        self#toggle;
        update_helper !neighbors
      end
  end

(* Question 2 *)
(* The game board *)

(* Checks if the row/col coordinage provided is a valid location. *)
let valid_location row col =
  row >= 0 && col >= 0 && row <= 4 && col <= 4

(* Creates a game board object that holds an array of lights. *)
let make_board () =
  let board =
    Array.map (fun arr -> Array.map make_light arr) (Array.make_matrix 5 5 ())
  in
  fun row col ->
    if (valid_location row col) then board.(row).(col)
    else failwith "make_board: invalid coordinate given!"

(* Question 3 *)
(* The game object *)

type game_input = Quit | Coords of int * int
  
let newline () = Printf.printf "\n"

(* Creates a game object that describes all the pieces of the game. *)
let make_game () =
  (*** Helper functions. ***)

  (* Connect all orthogonally adjacent lights to each other. *)
  let connect board =
    for i = 0 to 4 do
      for j = 0 to 4 do
        (* left *)
        if (j - 1) >= 0 then (board i j)#add_neighbor (board i (j - 1));
        (* right *)
        if (j + 1) <= 4 then (board i j)#add_neighbor (board i (j + 1));
        (* up *)
        if (i - 1) >= 0 then (board i j)#add_neighbor (board (i - 1) j);
        (* down *)
        if (i + 1) <= 4 then (board i j)#add_neighbor (board (i + 1) j);
      done;
    done;
  in

  (* Initialize the board given an array of arrays of on/off values 
     by setting the corresponding lights to those values. *)
  let initialize_board board init =
    if (Array.length init) != 5
    then failwith "init: initial values array incorrectly sized"
    else
    for i = 0 to 4 do
      if (Array.length init.(i)) != 5
      then failwith "init: initial values array incorrectly sized"
      else
      for j = 0 to 4 do
        (board i j)#set_state init.(i).(j)
      done;
    done;
  in

  (* Print the board states to the terminal in a readable form. *)
  let print_board board =
    begin
      for row = 0 to 4 do
        for col = 0 to 4 do
          Printf.printf "%c" 
            (match (board row col)#state with
               | On -> '0'
               | Off -> '.')
        done;
        newline ()
      done;
      newline ()
    end
  in

  (*** The message-passing board object. ***)
  let board = make_board () in
    begin
      connect board;
      object (self)
        method init values = initialize_board board values
        method peek row col =
          if (valid_location row col) then board row col
          else failwith "peek: invalid coordinate!"
        method play row col = 
          if (valid_location row col) then (board row col)#update
          else failwith "play: invalid coordinate!"
        method play_many moves =
          let t = ref moves in
          for i = 0 to (List.length moves) - 1 do
            let (a, b) = (List.hd !t) in
            self#play a b;
            t := List.tl !t
          done;
        method is_clear =
          try
            for i = 0 to 4 do
              for j = 0 to 4 do
                if (board i j)#state = On then raise Exit
              done
            done;
            true
          with Exit -> false
        method print = print_board board
      end
    end

(*** Play the game interactively. ***)

let play_game init =
  let is_digit c = c >= '0' && c <= '9' in
  let ok_coords_line line =
    String.length line = 3 
      && is_digit line.[0]
      && line.[1] = ' '
      && is_digit line.[2]
  in
  let get_input () =
    let line = read_line () in
      match line with
        | "quit" -> Quit
        | _ when ok_coords_line line ->
          let row = int_of_string (Printf.sprintf "%c" line.[0]) in
          let col = int_of_string (Printf.sprintf "%c" line.[2]) in
            if valid_location row col
              then Coords (row, col)
              else failwith "invalid coordinates"
        | _ -> failwith "invalid input line"
  in
  let rec run game =
    try
      begin
        Printf.printf "Enter move (row col): ";
        match get_input () with
          | Quit -> ()
          | Coords (row, col) -> 
              begin
                game#play row col;
                newline ();
                game#print;
                if game#is_clear
                  then Printf.printf "You win!\n\n"
                  else run game
              end
      end
    with Failure msg -> (Printf.printf "ERROR: %s\n\n" msg; run game)
  in
  let game = make_game () in
    begin
      game#init init;
      newline ();
      game#print;
      run game
    end
