class virtual focusable =
object(self)
    val virtual events : ([>`Focused | `Unfocused], [>`FocusedArg | `UnfocusedArg]) HandlesEvent.event_store

    val mutable isFocused = false

    method onFocused = ()
    method onUnfocused = ()

    initializer
        let fn = function
            | `FocusedArg -> isFocused <- true; self#onFocused
            | `UnfocusedArg -> isFocused <- false; self#onUnfocused
            | _ -> ()
        in
        let obj = object method call evt = fn evt#arg end in
        events#addFn `Focused obj;
        events#addFn `Unfocused obj;
end

type focus_direction = Forward
                     | Backward

let focusedEvent = HandlesEvent.mkEvent `Focused `FocusedArg
let unfocusedEvent = HandlesEvent.mkEvent `Unfocused `UnfocusedArg

class virtual ['a, 'b] focusManager app (children : ('a, 'b) HandlesEvent.handles_event list) =
object(self)
    val mutable focused = 0

    val virtual events : ('a, 'b) HandlesEvent.event_store
    val children : ('a, 'b) HandlesEvent.handles_event array = Array.of_list children

    method private rotateFocusFwd () = 
        focused <- Int.rem (focused + 1) (Array.length children)

    method private rotateFocusBwd () =
        let len = Array.length children in
        focused <- focused - 1;
        if focused < 0 then focused <- len - 1

    method private updateFocus dir =
        let focus_fn = match dir with
               | Forward -> self#rotateFocusFwd
               | Backward -> self#rotateFocusBwd
        in
        let old_widget = self#focused in
        focus_fn();
        let new_widget = self#focused in
        old_widget#events#handle unfocusedEvent;
        new_widget#events#handle focusedEvent;

    method focused = children.(focused)

    initializer 
        let fn evt = 
            match evt#arg with
            | `KeyDownArg Keys.Tab -> self#updateFocus Forward
            | _ -> self#focused#events#handle evt
        in
        events#addFn `Any (object
            method call = fn 
        end);
        self#focused#events#handle focusedEvent;
end
