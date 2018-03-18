(*  
                                CS 51
                             Spring 2018
                        Problem Set 5: Search
 
			  Game Descriptions

  Provides a GAMEDESCRIPTION module type, which will be an argument to
  the MakeGameSolver functor that creates a full Game module (see
  gameplay.ml).

  Also provides generic functors for creating a variety of Tile and Maze
  GAMEDESCRIPTIONs based on a simple input.
   
  Games have a set of states and moves that can be applied to
  deterministically change state. A neighbors function specifies what
  state is moved to depending on the current state and move. There is
  a specially designated initial state and certain states can be goal
  states. A series of moves can be executed from the initial state.
  Functionality for depicting game states is also provided for.
 *)

module type GAMEDESCRIPTION =
  sig
    (* state -- The state of the game *)
    type state
	   
    (* move -- The possible moves allowed by the game *)
    type move

    (* invalid move exception *)
    exception InvalidMove
           
    (* neighbors -- Given a state, return a list of moves and the states
     that they result in *)      
    val neighbors : state -> (state * move) list
                                            
    (* is_goal -- Predicate is true if its argument is a goal state *)
    val is_goal : state -> bool
                             
    (* initial_state -- The designated initial state for the game *)
    val initial_state : state
			
    (* compare_states -- Compare two states: -1 if s1 < s2; 0 if s1 =
     s2; +1 if s1 > s2 (consistent with Pervasives.compare) *)
    val compare_states : state -> state -> int
                                             
    (* execute_moves -- Given a list of moves, return the state that
     would result from executing that list of moves starting in the
     initial state *)    
    val execute_moves : move list -> state
                                     
    (* print_state -- Print a representation of the state *)
    val print_state : state -> unit
                               
    (* draw -- Render a set of states and a sequence of moves; used for
       debugging *)
    val draw : state list -> move list -> unit
  end
  
