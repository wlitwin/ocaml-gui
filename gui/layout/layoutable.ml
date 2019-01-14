class virtual ['a, 'b] layoutable =
object(self)
    inherit Drawable.drawable

    val virtual events : ([>`Resize] as 'a, [>`ResizeArg of Rect.t] as 'b) HandlesEvent.event_store

    val virtual id : int
    val mutable virtual rect : Rect.t
    
    method virtual preferredSize : Size.t
    method id   = id
    method size = RectSize.size_of_rect rect
    method pos  = RectPos.pos_of_rect rect
    method rect = rect

    method events = events

    method resize r =
        events#handle (HandlesEvent.mkEvent `Resize (`ResizeArg r))

    method onResize r =
        rect <- r;

    initializer
        let fn = function
            | `ResizeArg rect -> self#onResize rect
            | _ -> ()
        in
        events#addFn `Resize (object
            method call evt = fn evt#arg
        end)
end

