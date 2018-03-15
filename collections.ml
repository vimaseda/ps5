(*                                      
                                CS 51
                             Spring 2018
                        Problem Set 5: Search
 
                             Collections

  The Collection module signature is a generic data structure
  generalizing stacks, queues, and priority queues, allowing adding
  and taking elements. This file provides the signature and several
  functors implementing specific collections (stacks, queues,
  etc.).  
 *)

module type COLLECTION = 
sig

  (* Empty -- Indicates attempt to take from an empty collection *)
  exception Empty
              
  (* elements in the collection *)
  type elt
         
  (* collections themselves *)
  type collection
         
  (* empty -- the empty collection, collection with no elements *)
  val empty : collection 
                
  (* length -- number of elements in the collection *)
  val length : collection -> int
                              
  (* is_empty -- true if and only if the collection is empty *)
  val is_empty : collection -> bool
                                
  (* add -- add an element to a collection *)
  val add : elt -> collection -> collection
                                  
  (* take -- return a pair of an element from the collection and the
     collection of the remaining elements; raise Empty if the
     collection is empty *)
  val take : collection -> elt * collection
                                  
end
  
(*......................................................................
  Some useful collections

  To think about: For each of these implementations, what is the time
  complexity for adding and taking elements in this kind of
  collection? 
 *)

(* Stacks implemented as lists *)
    
module MakeStackList (Element : sig type t end)
       : (COLLECTION with type elt = Element.t) = 
  struct
    exception Empty

    type elt = Element.t
    type collection = elt list

    let empty : collection = []

    let is_empty (d : collection) : bool = 
      d = empty 
            
    let length (d : collection) : int = 
      List.length d
                  
    let add (e : elt) (d : collection) : collection = 
      e :: d;;
      
    let take (d : collection) :  elt * collection = 
      match d with 
      | hd :: tl -> (hd, tl)
      | _ -> raise Empty
end

(* Queues implemented as lists *)
  
module MakeQueueList (Element : sig type t end)
       : (COLLECTION with type elt = Element.t) =
  struct        
    exception Empty
                
    type elt = Element.t
    type collection = elt list
                          
    let empty : collection = []
                               
    let length (d : collection) : int = 
      List.length d
                  
    let is_empty (d : collection) : bool = 
      d = empty 
            
    let add (e : elt) (d : collection) : collection = 
      d @ [e];;
      
    let take (d : collection)  :  elt * collection = 
      match d with 
      | hd :: tl -> (hd, tl)
      | _ -> raise Empty
  end

(* Queues implemented as two stacks
 
   In this implementation, the queue is implemented as a pair of
   stacks (s1, s2) where the elements in the queue from highest to
   lowest priority (first to last to be taken) are given by s1 @
   s2^R. Elements are added (in stack regime) to s2, and taken from
   s1. When s1 is empty, s2 is reversed onto s1.
 *)

module MakeQueueStack (Element : sig type t end) 
       : (COLLECTION with type elt = Element.t) =
  struct
    failwith "MakeQueueStack not implemented"
  end
