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
        List.iter (fun f -> f evt) eventHandlers
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
object
    inherit handlesEvent

    val mutable onClick : (mouse_button * pos) -> unit = ignore
    val mutable onEnter : pos -> unit = ignore
    val mutable onLeave : pos -> unit = ignore
    val mutable onMove  : pos -> unit = ignore

    initializer
        eventHandlers <-
            (function
             | Click (btn, pos) -> onClick (btn, pos)
             | Move pos -> onMove pos
             | Enter pos -> onEnter pos
             | Leave pos -> onLeave pos
             | _ -> ()) :: eventHandlers
end

type event += KeyDown of Keys.key
            | KeyUp of Keys.key

class virtual handlesKeyboard =
object
    inherit handlesEvent

    val mutable onKeyDown : Keys.key -> unit = ignore
    val mutable onKeyUp   : Keys.key -> unit = ignore

    initializer
        eventHandlers <-
            (function
             | KeyDown key -> onKeyDown key
             | KeyUp key -> onKeyUp key
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
