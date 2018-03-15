(*  
                                CS 51
                             Spring 2018
                        Problem Set 5: Search
 
                             Maze Puzzles
 *)

open Draw
open Gamedescription

module G = Graphics ;;
  
(* Maze type definitions: 

   Mazes are two dimensional grids where the elements represent open
   space or walls *)
       
type space =
  | EmptySpace
  | Wall

type maze = space array array

type position = int * int

type direction = Up | Down | Left | Right

(* Maze puzzles -- Information about a particular instance of a maze,
   providing the dimensions of the maze, its contents, and the initial
   position and goal position *)
		  
module type MAZEINFO =
sig 
    val dims : int * int
    val maze : maze
    val initial_pos : position
    val goal_pos : position
end
  

(* MakeMazegame -- functor that given a MAZEINFO module generates a
   GAMEDESCRIPTION module implementing a a maze puzzle *)
module MakeMazeGameDescription (M : MAZEINFO)
       : (GAMEDESCRIPTION with type state = position
			   and type move = direction) = 
  struct
    
    (* Type state is where the player is currently located in the maze *)
    type state = position
                   
    (* The player can move various ways in the maze *)
    type move = direction

    (* The initial state is the initial position of the player *)
    let initial_state : state = M.initial_pos
    let goal_state : state = M.goal_pos

    (* Function that tells you whether a given state is the goal state *)
    let is_goal (s : state) : bool = 
        s = goal_state

    (* Function that maps moves to functions to be executed on the player's position *)
    let move_to_fun (m : move) : ((int * int) -> (int * int)) =
        match m with
        | Up -> fun (i, j) -> i - 1, j
        | Down -> fun (i, j) -> i + 1, j
        | Left -> fun (i, j) -> i, j + 1
        | Right -> fun (i, j) -> i, j - 1

    let neighbors (playerPos : state) : (state * move) list =
      let (w, h) = M.dims in
      let validate_pos (i, j) = i >= 0 && i < h && j >= 0 && j < h in
      [Up; Down; Left; Right]
      |> List.map (fun m -> ((move_to_fun m) playerPos), m)
      |> List.filter (fun (newPos, _) -> validate_pos newPos) (* don't go off the board *)
      |> List.filter (fun ((row, col), _) ->
                      match M.maze.(row).(col) with
                      | Wall -> false (* can't move onto a wall *)
                      | _ -> true) 

    let compare_states (s1: state) (s2: state) : int = 
        compare s1 s2

    let print_state (s: state) : unit = 
        Printf.printf("("); 
        print_int (fst s); 
        Printf.printf(", ");
        print_int (snd s);
        Printf.printf(") ")

    let execute_moves (path : move list) : state = 
        let rec execute_helper (board : state) (p : move list) : state = 
            match p with 
            | [] -> board 
            | hd :: tl -> 
                let x, y = board in 
                match hd with 
                | Left -> execute_helper (x, y + 1) tl 
                | Right -> execute_helper (x, y - 1) tl
                | Up -> execute_helper (x - 1, y) tl
                | Down -> execute_helper (x + 1, y) tl in 
        execute_helper initial_state path
                       
    (* Draws the map for a given maze. *)
    let draw_maze (maze_map : space array array) (elt_width : int) (elt_height : int) : unit =
      G.set_line_width cLINEWIDTH;
      Array.iteri (fun y m -> 
                   Array.iteri (fun x n -> 
                                match n with
                                | EmptySpace -> draw_square cUNSEENCOLOR y x elt_width elt_height
                                | Wall -> draw_square cWALLCOLOR y x elt_width elt_height
                               ) m) maze_map ;;
      
    (* Draws the heat map for a given maze. *)
    let draw_heat_map (visited: (int * int) list) (elt_width : int) (elt_height : int) : unit = 
      let rec remove_dups lst =
        match lst with
        | [] -> []
        | h::t -> h::(remove_dups (List.filter ((<>) h) t)) in
      let unique_visited = remove_dups visited in
      let incr = 200 / (List.length unique_visited) in
      let red = ref 55 in
      List.iter (fun (y, x) ->
                 let c = G.rgb !red 0 0 in
                 (red := !red + incr; G.synchronize ());
                 draw_square c y x elt_width elt_height) unique_visited ;;
      
    (* Displays a full maze animation on the screen. *)
    let display_maze (dims: int * int) (maze_map : space array array)
                     (visited: position list) (path: position list) (g: position) : unit = 
      G.open_graph "";
      G.resize_window cFRAMESIZE cFRAMESIZE;
      let height, width = (dims) in
      let elt_width = cFRAMESIZE / width in
      let elt_height = cFRAMESIZE / height in
      List.iter (fun (y, x) ->
                 G.clear_graph ();
                 draw_maze maze_map elt_width elt_height;
                 draw_heat_map visited elt_width elt_height;
                 draw_square cGOALBGCOLOR (fst g) (snd g) elt_width elt_height;
                 draw_circle cGOALCOLOR (fst g) (snd g) elt_width elt_height;
                 draw_circle cLOCCOLOR y x elt_width elt_height;
                 delay cFRAMEDELAY) path;
      ignore (G.read_key ()) ;;
      
    let draw (visited: state list) (path: move list) : unit =
      let rec moves_to_states (origin: state) (path: move list) : state list =
        match path with
        | [] -> [origin]
        | hd :: tl ->
           let (x, y) = origin in
           match hd with
           | Left -> (x, y + 1) :: moves_to_states (x, y + 1) tl
           | Right -> (x, y - 1) :: moves_to_states (x, y - 1) tl
           | Up -> (x - 1, y) :: moves_to_states (x - 1, y) tl
           | Down -> (x + 1, y) :: moves_to_states (x + 1, y) tl
      in
      display_maze M.dims M.maze visited (moves_to_states initial_state path) goal_state
                   
  end

