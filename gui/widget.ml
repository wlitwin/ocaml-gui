open Mixins

class basicWidget app = object(self)
    val id = 0
    val style = new Style.style
    val application = app
    val mutable eventHandlers = []
    val mutable rect = Rect.empty
    val mutable shouldClip = false
    val mutable layout : Mixins.layout option = None

    inherit styleable
    inherit handlesMouse
    inherit handlesKeyboard
    inherit layoutable
    inherit drawable
    inherit focusable

    method setLayout l =
        layout <- Some l

    method invalidate : unit =
        app#redraw

    method contentSize = self#size

    method private fullRect =
        style#outsetRectByBorder rect

    method clipDrawArea cr =
        if shouldClip then Paint.clip cr self#fullRect

    method private sendEventToLayout event =
        Option.iter layout (fun l -> l#postEvent event |> ignore)

    method paint cr =
        self#sendEventToLayout (Paint cr)

    (* TODO - move this into mixins
     *)
    method onResize r =
        rect <- style#insetRectByBorder r;
        self#sendEventToLayout (Resize rect);
        Mixins.Propagate

    method preferredSize =
        style#outsetSizeByBorder self#contentSize

    method onDraw cr =
        Cairo.save cr;
        self#clipDrawArea cr;
        let fullRect = self#fullRect in
        style#fillBgColor cr fullRect;
        style#drawBorder cr fullRect;
        Cairo.move_to cr rect.x rect.y;
        self#paint cr;
        Cairo.restore cr;
end
