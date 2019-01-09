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

(*
class virtual focusManager children =
object(self)
    val virtual events : 
        ([>`Paint | `KeyDown | `Focused | `Unfocused], [>`KeyDownArg of Keys.key | `PaintArg of Platform.Windowing.Graphics.context | `FocusedArg | `UnfocusedArg]) HandlesEvent.event_store

    val mutable focused = 0
    val children : <events : ([>`Focused | `Unfocused], [>`FocusedArg | `UnfocusedArg]) HandlesEvent.event_store > array = Array.of_list children

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
        old_widget#events#handle focusedEvent;
        new_widget#events#handle unfocusedEvent;
        (*
        app#redrawWidget new_widget;
        app#redrawWidget old_widget;
        *)

    method private sendToAll evt =
        Array.iter children (fun ch -> ch#events#handle evt)

    method focused = children.(focused)

    initializer 
        self#focused#events#handle focusedEvent;
        let fn evt = 
            match evt#arg with
            | `KeyDownArg Keys.Tab -> self#updateFocus Forward
            | `PaintArg _ -> self#sendToAll evt
            | _ -> self#focused#events#handle evt
        in
        let obj = object method call = fn end in
        events#addFn `KeyDown obj;
        events#addFn `Paint obj;
        (* TODO need an ANY event *)
end
*)
