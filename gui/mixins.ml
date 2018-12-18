class virtual layoutable =
object
    val virtual id : int
    val mutable virtual rect : Rect.t
    
    method virtual preferredSize : Size.t
    method id   = id
    method size = RectSize.size_of_rect rect
    method pos  = RectPos.pos_of_rect rect
    method rect = rect
    method resize r = rect <- r
end

class virtual layout =
object(self)
    inherit layoutable as super
    val mutable rect = Rect.empty

    method virtual addLayoutable : layoutable -> unit
    method virtual removeLayoutable : int -> unit
    method virtual layout : Rect.t -> unit
    method virtual items : layoutable list

    method! resize r =
        Stdio.printf " -- Layout Resizing to %f %f %f %f --\n%!" r.x r.y r.w r.h;
        super#resize r;
        self#layout r
end

type event = .. 

type event_result = Propagate
                  | Stop

class virtual handlesEvent =
object
    val virtual mutable eventHandlers : (event -> event_result) list
    (*val mutable virtual eventFilters*)

    method postEvent evt =
        let module Cs = Continue_or_stop in
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

type mouse_button =
    | Left
    | Right
    | Middle

type event += Click of (mouse_button * Pos.t)
            | Move of Pos.t
            | Enter of Pos.t
            | Leave of Pos.t

class virtual handlesMouse =
object(self)
    inherit handlesEvent

    method onClick (mouse_button, pos) = Propagate
    method onEnter pos = Propagate
    method onLeave pos = Propagate
    method onMove pos = Propagate

    initializer
        eventHandlers <-
            (function
             | Click (btn, pos) -> self#onClick (btn, pos)
             | Move pos -> self#onMove pos
             | Enter pos -> self#onEnter pos
             | Leave pos -> self#onLeave pos
             | _ -> Propagate) :: eventHandlers
end

type event += KeyDown of Keys.key
            | KeyUp of Keys.key

class virtual handlesKeyboard =
object(self)
    inherit handlesEvent

    method onKeyDown (key : Keys.key) : event_result = Propagate
    method onKeyUp (key : Keys.key) : event_result = Propagate

    initializer
        eventHandlers <-
            (function
             | KeyDown key -> self#onKeyDown key
             | KeyUp key -> self#onKeyUp key
             | _ -> Propagate) :: eventHandlers
end

class virtual styleable =
    let open Style in
object
    val virtual style : style

    method style = style
end

type event += Paint of Cairo.context

class virtual drawable =
object(self)
    inherit handlesEvent

    method virtual onDraw : Cairo.context -> unit 

    initializer
        eventHandlers <-
            (function
                | Paint cr -> self#onDraw cr; Propagate
                | _ -> Propagate) :: eventHandlers
end

type event += Focused
            | Unfocused

class virtual focusable =
object(self)
    inherit handlesEvent

    val mutable isFocused = false

    method onFocused = Stop
    method onUnfocused = Stop

    initializer
        eventHandlers <-
            (function
                | Focused -> isFocused <- true; self#onFocused
                | Unfocused -> isFocused <- false; self#onUnfocused
                | _ -> Propagate) :: eventHandlers
end

type focus_direction = Forward
                     | Backward

class virtual focusManager app children =
object(self)
    inherit handlesEvent

    val mutable focused = 0
    val children : handlesEvent array = Array.of_list children

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
        old_widget#postEvent Unfocused |> ignore;
        new_widget#postEvent Focused |> ignore;
        app#redraw;

    method private sendToAll evt =
        Array.iter children (fun ch -> ch#postEvent evt |> ignore)

    method focused = children.(focused)

    initializer 
        self#focused#postEvent Focused |> ignore;
        eventHandlers <-
            (function
                | KeyDown Keys.Tab -> 
                        self#updateFocus Forward;
                        Stop 
                | Paint _ as p -> self#sendToAll p; Propagate
                | e -> self#focused#postEvent e) :: eventHandlers
end
