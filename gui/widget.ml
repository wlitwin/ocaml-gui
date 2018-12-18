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

    method fillBgColor cr =
        let bgColor = style#bgColor in
        Cairo.set_source_rgba cr bgColor.r bgColor.g bgColor.b bgColor.a;
        Cairo.rectangle cr rect.x rect.y rect.w rect.h;
        Cairo.fill cr;

    method clipDrawArea cr =
        if shouldClip then (
            Cairo.rectangle cr rect.x rect.y rect.w rect.h;
            Cairo.clip cr;
        )

    method drawBorder cr =
        let color = style#fgColor in
        Cairo.rectangle cr rect.x rect.y rect.w rect.h;
        Cairo.set_source_rgba cr color.r color.g color.b color.a;
        Cairo.set_line_width cr 10.;
        Cairo.stroke cr;

    method paint cr = ()

    method onDraw cr =
        Cairo.save cr;
        self#clipDrawArea cr;
        self#fillBgColor cr;
        Cairo.move_to cr rect.x rect.y;
        self#paint cr;
        Cairo.restore cr;
end
