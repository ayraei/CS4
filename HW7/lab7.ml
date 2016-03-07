(* Question 1 *)
(* The light object *)

type light_state = On | Off

(* Creates a single Light object. *)
let make_light () =
  let curr_state = ref Off in
  let neighbors = ref [] in
  let opposite lt = if lt#state = On then Off else On in
  object (self)
    method state = !curr_state
    method set_state st = curr_state := st
    method toggle = curr_state := (opposite self)
    method add_neighbor ob = neighbors := (ob :: !neighbors)
    method update =
      begin
        self#toggle;
        List.map (fun x -> x#toggle) !neighbors
      end
  end

(* Question 2 *)
(* The game board *)

(* Creates a game board object that holds an array of lights. *)
let make_board () =
  let ml = make_light () in
  let board =
    [|
      [| ml; ml; ml; ml; ml |];
      [| ml; ml; ml; ml; ml |];
      [| ml; ml; ml; ml; ml |];
      [| ml; ml; ml; ml; ml |];
      [| ml; ml; ml; ml; ml |]
    |]
  in
  fun row col ->
    if not (row >= 0 && col >= 0 && row <= 4 && col <= 4) then
    failwith "board: indices passed in are not within board bounds!"
    else board.(row).(col)

(* Question 3 *)
(* The game object *)

type game_input = Quit | Coords of int * int
  
let newline () = Printf.printf "\n"

(* Creates a game object that describes all the pieces of the game. *)
let make_game () =
  (*** Helper functions. ***)

  (* Connect all orthogonally adjacent lights to each other. *)
  let connect board = ()
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
        method peek row col = board row col
        method play row col = (board row col)#update
        method play_many moves =
          let t = ref moves in
          for i = 0 to (List.length moves) - 1 do
            let (a, b) = (List.hd !t) in
            self#play a b;
            t := List.tl !t
          done;
        method is_clear =
          let t = ref true in
          for i = 0 to 4 do
            for j = 0 to 4 do
              if (board i j)#state = On then
              t := false
              else ()
            done;
          done;
          !t
        method print = print_board board
      end
    end
