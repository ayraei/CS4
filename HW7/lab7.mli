type light_state = On | Off

val make_light :
  unit ->
  < add_neighbor : (< add_neighbor : 'a -> unit;
                      set_state : light_state -> unit;
                      state : light_state; 
                      toggle : unit;
                      update : unit >
                      as 'a) -> unit;
    set_state : light_state -> unit; 
    state : light_state; 
    toggle : unit;
    update : unit >

val make_board :
  unit ->
  int ->
  int ->
  < add_neighbor : (< add_neighbor : 'a -> unit;
                      set_state : light_state -> unit;
                      state : light_state; 
                      toggle : unit;
                      update : unit >
                      as 'a) -> unit;
    set_state : light_state -> unit; 
    state : light_state; 
    toggle : unit;
    update : unit >

val make_game :
  unit ->
  < init : light_state array array -> unit; 
    is_clear : bool;
    peek : int ->
           int ->
           < add_neighbor : (< add_neighbor : 'a -> unit;
                               set_state : light_state -> unit;
                               state : light_state; 
                               toggle : unit;
                               update : unit >
                             as 'a) -> unit;
             set_state : light_state -> unit; 
             state : light_state;
             toggle : unit; 
             update : unit >;
    play : int -> int -> unit; 
    play_many : (int * int) list -> unit;
    print : unit >

