(*  
                                CS 51
                             Spring 2018
                        Problem Set 5: Search
 
            The Fifteen Puzzle (and similar tile puzzles)
 *)

open Draw
open Gamedescription

(* Tile type definitions *)
type tile =
  | EmptyTile
  | Tile of int

type board = tile array array

type direction = Up | Down | Left | Right
                                  
(* TILEINFO -- Information about a particular type of 15-puzzle-style
   tile game, providing the dimensions of the board and its initial
   contents *)

module type TILEINFO =
sig 
    val initial : board
    val dims : int * int
end
 
(* MakeTileGame -- a functor that given a TILEINFO module (see
   types.ml) generates a GameArg module implementing a Tile Game *)
  
module MakeTileGameDescription (T : TILEINFO)
       : (GAMEDESCRIPTION with type state = board
                           and type move = direction) = 
  struct
                                               
    (* State is a board configuration *)
    type state = board
    
    (* Which direction the empty tile can move *)
    type move = direction

    (* The initial state is the initial state provided by the TILEINFO module *)
    let initial_state : state = T.initial

    (* tile_comp -- Comparing two tiles as with
       Pervasives.compare. The empty tile is deemed greater than all
       others. *)
    let tile_comp (t1 : tile) (t2 : tile) : int =
        match t1, t2 with
        | EmptyTile, _ -> 1
        | _, EmptyTile -> -1
        | Tile n1, Tile n2 -> compare n1 n2

    (* tile_to_str -- Generate a string representation of a tile;
       useful for printing and debugging *)
    let tile_to_str (t : tile) : string = 
        match t with
        | Tile n -> string_of_int n ^ " "
        | EmptyTile -> "_ "

    let print (b : tile array) : unit =
        Array.iter (fun t ->
            Printf.printf "%s" (tile_to_str t)) b; Printf.printf("\n")

    (* flatten -- Convert board array into a single list, for easier
       manipulation *)
    let flatten (bs : tile array array) : tile array = 
        Array.fold_right (fun elt acc -> Array.append elt acc) bs (Array.make 0 EmptyTile)

    (* Goal states are those boards where the tiles are sorted. *)
    let is_goal (s: state) : bool = 
        let bs = s in
        let flat = flatten bs in
        let sorted = Array.copy flat in
        Array.sort tile_comp sorted; 
        flat = sorted

    (* find_empty -- Returns the (row, column) position of the empty tile *)
    let find_empty b : int * int = 
        let find_elt_helper elt idx acc : int * bool = 
            match elt, acc with 
            | EmptyTile, false -> (idx, true)
            | _, true -> (idx, true)
            | _, _ -> (idx + 1, false) in
        let in_row = 
            Array.fold_left 
                (fun acc elt -> acc || elt = EmptyTile) false in 
        let find_row = 
            Array.fold_left 
                (fun acc elt -> find_elt_helper elt (fst acc) (snd acc)) 
                    (0, false) in
        let find_col_helper elt acc = 
            let (row, col, acc) = acc in 
            match (in_row elt), acc with 
            | true, false -> (row, fst (find_row elt), true)
            | _, true -> (row, col, true)
            | _ -> (row + 1, -1, acc) in
        let find_rowcol = 
            Array.fold_left 
                (fun acc elt -> find_col_helper elt acc) (0, 0, false) in
        let (i, j, _) = find_rowcol b in
        (i,j)


    (* move_to_fun -- Helper function that converts moves in to
       functions to be executed on the position of the empty tile *)
    let move_to_fun (m : move) : ((int * int) -> (int * int)) =
        match m with
        | Up -> fun (i, j) -> i - 1, j
        | Down -> fun (i, j) -> i + 1, j
        | Left -> fun (i, j) -> i, j - 1
        | Right -> fun (i, j) -> i, j + 1

    (* copy_voard -- Helper function that copies a tile board *)
    let copy_board (arr : board) : board =
        Array.map Array.copy arr

    (* swap_tiles -- Helper function that destructively updates the
       position of the empty tile for a given board *)
    let swap_tiles (arr : board) 
                   (old_empty : int * int )
                   (new_empty : int * int) : unit =
        let new_row, new_col = new_empty in 
        let old_row, old_col = old_empty in
        (* store the value the empty tile will replace *)
        let tmp = arr.(new_row).(new_col) in
        (* replace that value with the empty tile *)
        arr.(new_row).(new_col) <- arr.(old_row).(old_col);
        arr.(old_row).(old_col) <- tmp

 
    (* Draws a tile game for a given board. *)
    let draw_tiles (tile_map : board) (height: int) : unit =
      Array.iteri (fun y rows ->
                   Array.iteri (fun x t ->
                                G.draw_rect (x * cTILEWIDTH)
                                            (height - cTILEWIDTH - y * cTILEWIDTH)
                                            cTILEWIDTH
                                            cTILEWIDTH;
                                G.moveto (x * cTILEWIDTH + cTILEWIDTH / 2)
                                         (height - cTILEWIDTH - y * cTILEWIDTH + cTILEWIDTH / 2);
                                match t with
                                | Tile n -> G.draw_string (string_of_int n)
                                | EmptyTile -> ();
                               ) rows) tile_map ;;
      
    (* Helper function to delay frames. *)
    let rec delay (sec : float) : unit =
      try ignore (Thread.delay sec)
      with Unix.Unix_error _ -> delay sec ;;
      
    (* Displays a full tile animation on the screen. *)
    let display_tile (dims: int * int) (visited: 'a list) : unit = 
      G.open_graph "";
      let height, width = dims in
      G.resize_window (width * cTILEWIDTH) (width * cTILEWIDTH);
      List.iter (fun m ->
                 G.clear_graph ();
                 draw_tiles m (height * cTILEWIDTH);
                 delay cFRAMEDELAY;
                 G.synchronize ()) visited;
      ignore (G.read_key ()) ;;
      
    let draw (visited: state list) (path: move list) : unit =
      let rec moves_to_states (b: state) (path: move list) : state list =
        let e = find_empty b in 
        let new_board = copy_board b in
        match path with
        | [] -> [b]
        | hd :: tl ->
          let new_empty = move_to_fun hd e in
          let _ = swap_tiles new_board e new_empty in
          b :: moves_to_states (new_board) tl
      in
      display_tile T.dims (moves_to_states initial_state path) 

    let execute_moves (path : move list) : state = 
      let rec move_helper (board : state) (p : move list) : state = 
        let e = find_empty board in 
        let new_board = copy_board board in 
        match p with 
        | [] -> board
        | hd :: tl -> 
           let new_empty = move_to_fun hd e in 
           let _ = swap_tiles new_board e new_empty in 
           move_helper new_board tl 
      in move_helper initial_state path
                     
    let neighbors (s : state) : (state * move) list =
      (* Helper function that calls swap_tiles *)
      let neighbors_helper (bs : tile array array) 
                           (ep : int * int) 
                           ((m, new_empty) : move * (int * int)) =
        let new_bs = copy_board bs in
        let _ = swap_tiles new_bs ep new_empty in
        (new_bs, m) in
      let (w, h) = T.dims in
      let ep = find_empty s in 
      let check_pos (i, j) = i >= 0 && i < w && j >= 0 && j < h in
      let final_board = [Up; Down; Left; Right]
                        |> List.map (fun m -> (m, (move_to_fun m) ep))
                        |> List.filter (fun (_, p) -> check_pos p)
                        |> List.map (neighbors_helper s ep) in
      final_board
        
    (* A function for comparing two states: will be useful in gameplay.ml *)
    let compare_states (s1: state) (s2: state) : int = 
        let b1 = flatten s1 in 
        let b2 = flatten s2 in
        compare b1 b2

    (* A function for printing a state *)
    let print_state (s: state) : unit = 
        Array.iter print s; Printf.printf("\n")
end
  
