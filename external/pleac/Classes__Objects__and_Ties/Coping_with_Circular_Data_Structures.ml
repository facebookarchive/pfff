(* ********************************************************************** *)
(* Coping with Circular Data Structures *)
(* ********************************************************************** *)
let pleac_Coping_with_Circular_Data_Structures () = 
  (* OCaml features a generational garbage collector that can handle
     circular references, so you do not need to do anything special to
     safely dispose of circular data structures. The "DESTROY" method
     has been omitted from this translation since it is unnecessary.
  
     Option types are used heavily due to the imperative style of the
     original recipe, which makes this code somewhat verbose. *)
  
  (* A polymorphic, circular data structure. *)
  class ['a] ring () = object (self)
    val mutable dummy = (None : 'a ring_node option)
    val mutable count = 0
  
    (* Initialize dummy now that a reference to self is available. *)
    initializer
      (let node = new ring_node () in
       node#set_prev (Some node);
       node#set_next (Some node);
       dummy <- Some node)
  
    (* Return the number of values in the ring. *)
    method count = count
  
    (* Insert a value into the ring structure. *)
    method insert value =
      let node = new ring_node () in
      node#set_value (Some value);
      (match dummy with
         | Some ring_dummy ->
             node#set_next ring_dummy#next;
             (match ring_dummy#next with
                | Some ring_dummy_next ->
                    ring_dummy_next#set_prev (Some node)
                | None -> assert false);
             ring_dummy#set_next (Some node);
             node#set_prev (Some ring_dummy);
             count <- count + 1
         | None -> assert false)
  
    (* Find a value in the ring. *)
    method search value =
      match dummy with
        | Some ring_dummy ->
            (match ring_dummy#next with
               | Some ring_dummy_next ->
                   let node = ref ring_dummy_next in
                   while !node != ring_dummy && !node#value <> (Some value)
                   do node :=
                     match !node#next with
                       | Some n -> n
                       | None -> assert false
                   done;
                   !node
               | None -> assert false)
        | None -> assert false
  
    (* Delete a node from the ring structure. *)
    method delete_node node =
      (match node#prev with
         | Some node_prev -> node_prev#set_next node#next
         | None -> assert false);
      (match node#next with
         | Some node_next -> node_next#set_prev node#prev
         | None -> assert false);
      count <- count - 1
  
    (* Delete a node from the ring structure by value. *)
    method delete_value value =
      let node = self#search value in
      match dummy with
        | Some ring_dummy when node != ring_dummy ->
            self#delete_node node
        | _ -> ()
  end
  
  (* A node in the ring structure which contains a polymorphic value. *)
  and ['a] ring_node () = object
    val mutable prev = (None : 'a ring_node option)
    val mutable next = (None : 'a ring_node option)
    val mutable value = (None : 'a option)
    method prev = prev
    method next = next
    method value = value
    method set_prev prev' = prev <- prev'
    method set_next next' = next <- next'
    method set_value value' = value <- value'
  end
  

