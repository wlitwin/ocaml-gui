open Mixins

class basicWidget app = object(self)
    val id = 0
    val mutable eventHandlers = []
    val style = new Style.style
    val mutable rect = Rect.empty
    val application = app
    val mutable shouldClip = true

    inherit styleable
    inherit handlesMouse
    inherit handlesKeyboard
    inherit layoutable
    inherit drawable
    inherit focusable

    method invalidate : unit =
        app#redraw

    method preferredSize = self#size

    method calcInnerRect r =
        Rect.inset r style#borderSize

    method clipDrawArea cr =
        if shouldClip then Paint.clip cr rect

    method paint cr = ()

    method onDraw cr =
        Cairo.save cr;
        self#clipDrawArea cr;
        style#fillBgColor cr rect;
        style#drawBorder cr rect;
        Cairo.move_to cr rect.x rect.y;
        self#paint cr;
        Cairo.restore cr;
end
