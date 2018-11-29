open Rect

class virtual layoutable =
object
    val virtual id : int
    val mutable virtual rect : rect
    
    method virtual preferredSize : size
    method id   = id
    method size = size_of_rect rect
    method pos  = pos_of_rect rect
    method rect = rect
    method resize r = rect <- r
end

class virtual layout =
object
    method virtual addLayoutable : layoutable -> unit
    method virtual removeLayoutable : int -> unit
    method virtual layout : rect -> unit
    method virtual items : layoutable list
end

type event = .. 

class virtual handlesEvent =
object
    val virtual mutable eventHandlers : (event -> unit) list
    (*val mutable virtual eventFilters*)

    method postEvent evt =
        List.iter ~f:(fun f -> f evt) eventHandlers
end

type mouse_button =
    | Left
    | Right
    | Middle

type event += Click of (mouse_button * pos)
            | Move of pos
            | Enter of pos
            | Leave of pos

class virtual handlesMouse =
object(self)
    inherit handlesEvent

    method onClick (mouse_button, pos) = ()
    method onEnter pos = ()
    method onLeave pos = ()
    method onMove pos = ()

    initializer
        eventHandlers <-
            (function
             | Click (btn, pos) -> self#onClick (btn, pos)
             | Move pos -> self#onMove pos
             | Enter pos -> self#onEnter pos
             | Leave pos -> self#onLeave pos
             | _ -> ()) :: eventHandlers
end

type event += KeyDown of Keys.key
            | KeyUp of Keys.key

class virtual handlesKeyboard =
object(self)
    inherit handlesEvent

    method onKeyDown (key : Keys.key) : unit = ()
    method onKeyUp (key : Keys.key) : unit = ()

    initializer
        eventHandlers <-
            (function
             | KeyDown key -> self#onKeyDown key
             | KeyUp key -> self#onKeyUp key
             | _ -> ()) :: eventHandlers
end

class virtual styleable =
    let open Style in
object
    val virtual style : style

    method style = style
end

class virtual drawable =
object
    method virtual draw : Cairo.context -> unit 
end
