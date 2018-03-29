(*                       
                                CS 51
                             Spring 2018
                        Problem Set 5: Search
 
                             Game Solving

This file contains the signature for a GAMESOLVER module as well as a
higher-order functor, MakeGameSolver. A GAMESOLVER module solves the
game by searching for a path from the initial state to the goal state.

The MakeGameSolver functor takes in a collection functor and a
GAMEDESCRIPTION and returns a GAMESOLVER. The collection specified by
the functor is used to store the states that have been reached so
far. Thus, the ordering in which states are delivered by the
collection (with the take function) determines the order in which the
states are searched. A stack regime gives depth-first search, a queue
regime breadth-first search.

At the bottom of the file are definitions for a depth-first search and
breadth-first game solvers, partially applied versions of the
MakeGameSolver functor that use certain collections to engender
different search methods.
 *)

open Set
open Collections
open Gamedescription

(* GAMESOLVER -- a module type that provides for solving games and
   graphically drawing the results *)
       
module type GAMESOLVER = 
  sig
    exception CantReachGoal
    type state
    type move
    val solve : unit -> move list * state list
    val draw : state list -> move list -> unit
    val print_state: state -> unit
  end

(* MakeGameSolver -- a higher-order functor that generates game solvers, with type
 
  (functor(sig type t end -> COLLECTION)) -> GAMEDESCRIPTION -> GAMESOLVER

   A functor that given a functor from a GAMEDESCRIPTION to a
   game description-specific Collection, as well as a GAMEDESCRIPTION,
   returns a full GAMESOLVER module. 
 *)

module MakeGameSolver (DSFunc : functor(Element : sig type t end) -> 
                                       (COLLECTION with type elt = Element.t))
                      (G : GAMEDESCRIPTION)                  
       : (GAMESOLVER with type state = G.state
                      and type move = G.move) =
  struct
    type state = G.state
    type move = G.move
    exception CantReachGoal

    module MyOtherMod = DSFunc(struct type t = (G.state * (G.move list)) end)
    let pending = MyOtherMod.add (G.initial_state, []) MyOtherMod.empty
    module States = Set.Make(struct type t = state let compare = compare end)

    let solve () =
        let rec check_pend pend vis exp =
          if MyOtherMod.is_empty pend then raise CantReachGoal else
          let (s, m), t = MyOtherMod.take pend in
              match States.mem s vis with
              | true -> check_pend t vis exp
              | false -> let exp' = s :: exp in
                         if G.is_goal s then (List.rev m, List.rev exp') 
                         else
                          let rec combine x y =
                            match x with
                            | [] -> y
                            | (s, m1) :: t -> combine t (MyOtherMod.add (s, m1 :: m) y) in
                          check_pend (combine (G.neighbors s) t) (States.add s vis) exp'
        in check_pend pending States.empty [G.initial_state]

    let draw = G.draw
    let print_state = G.print_state


  end ;;
     
(* DFSSolver and BFSSolver: Higher-order Functors that take in a
   GAMEDESCRIPTION, and will return Games that are solved with DFS and
   BFS, respectively. The fast bfs solver uses a better implementation
   of queues for speed. *)
module DFSSolver = MakeGameSolver(MakeStackList) ;;
module BFSSolver = MakeGameSolver(MakeQueueList) ;;
module FastBFSSolver = MakeGameSolver(MakeQueueStack) ;;


(*======================================================================
Time estimate

Please give us an honest (if approximate) estimate of how long (in
minutes) the problem set took you to complete.  We care about your
responses and will use them to help guide us in creating future
assignments.
......................................................................*)

let minutes_spent_gamesolve () : int = failwith "not provided" ;;
