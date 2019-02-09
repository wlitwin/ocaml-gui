type mouse_event = MDown of int
                 | MUp of int
                 | MMove of int * int

type keyboard_event = KDown of int
                    | KUp of int

module EvRec = struct
    type ('a, 'b) event_store = {
        tbl : ('a, 'b -> unit) Hashtbl.Poly.t
    }

    let create () = {
        tbl = Hashtbl.Poly.create()
    }

    let register : type a b. (a, b) event_store -> a -> (b -> unit) -> unit =
        fun store key f ->
            Hashtbl.set store.tbl key f

    let unregister : type a b. (a, b) event_store -> a -> unit =
        fun store key ->
            Hashtbl.remove store.tbl key

    let invoke : type a b. (a, b) event_store -> a -> b -> unit =
        fun store key event ->
            match Hashtbl.find store.tbl key with
            | None -> ()
            | Some f -> f event
end

let _ =
    let events = EvRec.create() in
    EvRec.register events `MEDown (function
        | `MDown k -> ()
        | `MUp k -> ()
        | `MMove (x,y) -> ()
        | _ -> ());
    EvRec.register events `KEDown (function
        | `KDown k -> ()
        | `KUp k -> ()
        | _ -> ());
;;

module type EvSig = sig
    type 'a channel
    (** The type of communication channels carrying values of type ['a]. *)

    val new_channel : unit -> 'a channel
    (** Return a new channel. *)

    type +'a event
    (** The type of communication events returning a result of type ['a]. *)

    (** [send ch v] returns the event consisting in sending the value [v]
       over the channel [ch]. The result value of this event is [()]. *)
    val send : 'a channel -> 'a -> unit event

    (** [receive ch] returns the event consisting in receiving a value
       from the channel [ch]. The result value of this event is the
       value received. *)
    val receive : 'a channel -> 'a event

    val always : 'a -> 'a event
    (** [always v] returns an event that is always ready for
       synchronization.  The result value of this event is [v]. *)

    val choose : 'a event list -> 'a event
    (** [choose evl] returns the event that is the alternative of
       all the events in the list [evl]. *)

    val wrap : 'a event -> ('a -> 'b) -> 'b event
    (** [wrap ev fn] returns the event that performs the same communications
       as [ev], then applies the post-processing function [fn]
       on the return value. *)

    val wrap_abort : 'a event -> (unit -> unit) -> 'a event
    (** [wrap_abort ev fn] returns the event that performs
       the same communications as [ev], but if it is not selected
       the function [fn] is called after the synchronization. *)

    val guard : (unit -> 'a event) -> 'a event
    (** [guard fn] returns the event that, when synchronized, computes
       [fn()] and behaves as the resulting event. This allows events with
       side-effects to be computed at the time of the synchronization
       operation. *)

    val sync : 'a event -> 'a
    (** ``Synchronize'' on an event: offer all the communication
       possibilities specified in the event to the outside world,
       and block until one of the communications succeed. The result
       value of that communication is returned. *)

    val select : 'a event list -> 'a
    (** ``Synchronize'' on an alternative of events.
       [select evl] is shorthand for [sync(choose evl)]. *)

    val poll : 'a event -> 'a option
    (** Non-blocking version of {!Event.sync}: offer all the communication
       possibilities specified in the event to the outside world,
       and if one can take place immediately, perform it and return
       [Some r] where [r] is the result value of that communication.
       Otherwise, return [None] without blocking. *)
end

module Ev : EvSig = struct
    type 'a channel = {
        inputs : 'a Queue.t;
        outputs : 'a Queue.t;
    }

    type 'a basic_event = {
        poll : unit -> bool;
        suspend : unit -> unit;
        result : unit -> 'a;
    }

    type condition = {
        good : bool;
    }

    type 'a behavior = int ref -> condition -> int -> 'a basic_event

    type 'a event = {bop : int; v: 'a}

    let new_channel () = {
        inputs = Queue.create();
        outputs = Queue.create();
    }

    let send (chan : 'a channel) (value : 'a) : unit event =
        {bop=10;v=()}

    let receive chan = 
        let v = Queue.dequeue_exn chan.outputs in
        {bop=20; v}

    let always value = {bop=30;v=value}
    let choose evt_list = List.hd_exn evt_list 
    let wrap evt f = {bop=60; v=f evt.v}
    let wrap_abort evt f = evt
    let guard f = f()
    let sync evt = evt.v
    let select evt_list = (List.hd_exn evt_list).v
    let poll evt = Some evt.v
end

