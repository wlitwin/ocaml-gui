open Mixins
module Graphics = Platform.Windowing.Graphics

type 'a layout = 'a constraint 'a = Mixins.layout

class basicWidget app = object(self)
    val id = 0
    val style = new Style.style
    val application = app
    val mutable eventHandlers = []
    val mutable snoopers = []
    val mutable rect = Rect.empty
    val mutable shouldClip = true
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
        app#redrawWidget (self :> Mixins.handlesEvent)

    method contentSize = 
        match layout with
        | Some l -> l#preferredSize
        | None -> self#size

    method fullRect =
        style#borderStyle#outsetRectByBorder rect

    method clipDrawArea cr =
        if shouldClip then Graphics.clip_rect cr self#fullRect

    method private sendEventToLayout event =
        Option.iter layout (fun l -> l#postEvent event |> ignore)

    method paint cr =
        self#sendEventToLayout (Paint cr)

    (* TODO - move this into mixins
     *)
    method onResize r =
        rect <- style#borderStyle#insetRectByBorder r;
        self#sendEventToLayout (Resize rect);
        Mixins.Propagate

    method preferredSize =
        style#borderStyle#outsetSizeByBorder self#contentSize

    method onDraw cr =
        Graphics.save cr;
        self#clipDrawArea cr;
        let fullRect = self#fullRect in
        style#fillBgColor cr fullRect;
        style#borderStyle#drawBorder cr fullRect;
        Graphics.move_to cr rect.x rect.y;
        self#paint cr;
        Graphics.restore cr;

    initializer
        rect <- Rect.{x=0.; y=0.; w=10.; h=10.}
end
