type mouse_button =
    | Left
    | Right
    | Middle

class virtual handlesMouse =
object(self)
    val virtual events : ([>`Click | `Move | `Enter | `Leave], 
                          [>`ClickArg of (mouse_button * Pos.t) | `MoveArg of Pos.t | `EnterArg of Pos.t | `LeaveArg of Pos.t])
        HandlesEvent.event_store

    method onClick (mouse_button, pos) = ()
    method onEnter pos = ()
    method onLeave pos = ()
    method onMove pos = ()

    initializer
        let fn = function
            | `ClickArg args -> self#onClick args
            | `MoveArg pos -> self#onMove pos
            | `EnterArg pos -> self#onEnter pos
            | `LeaveArg pos -> self#onLeave pos
            | _ -> ()
        in
        let obj = object method call evt = fn evt#arg end in
        events#addFn `Click obj;
        events#addFn `Move obj;
        events#addFn `Enter obj;
        events#addFn `Leave obj;
end

