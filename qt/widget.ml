open Mixins

class basicWidget app = object(self)
    val id = 0
    val mutable eventHandlers = []
    val style = new Style.style
    val mutable rect = Rect.empty
    val application = app

    inherit styleable
    inherit handlesMouse
    inherit handlesKeyboard
    inherit layoutable
    inherit drawable
    inherit focusable

    method invalidate : unit =
        app#redraw

    method preferredSize = self#size

    method fillBgColor cr =
        let bgColor = style#bgColor in
        Cairo.set_source_rgba cr bgColor.r bgColor.g bgColor.b bgColor.a;
        Cairo.rectangle cr rect.x rect.y rect.w rect.h;
        Cairo.fill cr;

    method clipDrawArea cr =
        Cairo.rectangle cr rect.x rect.y rect.w rect.h;
        Cairo.clip cr;

    method paint cr = ()

    method onDraw cr =
        Cairo.save cr;
        self#clipDrawArea cr;
        self#fillBgColor cr;
        Cairo.move_to cr rect.x rect.y;
        self#paint cr;
        Cairo.restore cr;
end
