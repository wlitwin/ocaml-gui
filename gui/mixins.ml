type event = .. 

                  (*
type event_result = Propagate
                  | Stop


class virtual handlesEvent =
object
    val virtual mutable eventHandlers : (event -> event_result) list
    val virtual mutable snoopers : (event -> unit) list

    method addSnoop f =
        snoopers <- f :: snoopers

    method postEvent evt =
        let module Cs = Continue_or_stop in
        List.iter snoopers (fun f -> f evt);
        List.fold_until 
            eventHandlers
            ~init:Propagate
            ~f:(fun accum f -> 
                 match f evt with
                 | Stop -> Cs.Stop Stop 
                 | Propagate -> Cs.Continue Propagate
            )
            ~finish:Fn.id
end
*)

type event += Resize of Rect.t

type event += Paint of Platform.Windowing.Graphics.context

type event += KeyDown of Keys.key
            | KeyUp of Keys.key

