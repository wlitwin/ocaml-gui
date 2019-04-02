type 'a t

val create : unit -> 'a t

val search : 'a t * Rect.t * 'a DynArray.t -> unit

val insert : 'a t * Rect.t * 'a -> unit

val delete : 'a t * Rect.t * ('a -> bool) -> unit

