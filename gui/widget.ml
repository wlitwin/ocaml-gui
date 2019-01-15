module Graphics = Platform.Windowing.Graphics

class ['a, 'b] basicWidget app = object(self)
    val id = 0
    val style = new Style.style
    val application = app
    val mutable eventHandlers = []
    val mutable snoopers = []
    val mutable rect = Rect.empty
    val mutable layout : ('a, 'b) Layout.layout option = None
    val table = Hashtbl.Poly.create()
    val rev_table = Hashtbl.Poly.create()

    val bgRect : Rendering.rectObject = app#renderer#createRectObject
    val renderObject = app#renderer#createGroupObject

    inherit Stylable.styleable
    inherit ['a, 'b] Layoutable.layoutable
    inherit HandlesMouse.handlesMouse
    inherit HandlesKeyboard.handlesKeyboard
    inherit Focusable.focusable

    val events = HandlesEvent.create()

    method events : ('a, 'b) HandlesEvent.event_store = events

    method setLayout (l : ('a, 'b) #Layout.layout) =
        layout <- Some l;
        self#renderObject#attach l#renderObject;

    method contentSize = 
        match layout with
        | Some l -> l#preferredSize
        | None -> self#size

    method fullRect =
        style#borderStyle#outsetRectByBorder rect

    method private sendEventToLayout event =
        Option.iter layout (fun l -> l#events#handle event)

    method onResize (r : Rect.t) =
        let open Layoutable in
        rect <- style#borderStyle#insetRectByBorder r;
        bgRect#setRect r;
        self#sendEventToLayout HandlesEvent.(mkEvent `Resize (`ResizeArg rect));
        (*renderObject#setContent (Group [0, [Rendering.fill_rect self#fullRect style#bgColor]])*)

    method preferredSize =
        style#borderStyle#outsetSizeByBorder self#contentSize

    method setBGColor (color : Color.t) =
        style#setBGColor color;
        bgRect#setColor color;
        (*renderObject#setContent (Group [0, [Rendering.fill_rect self#fullRect color]])*)

    initializer
        self#setBGColor style#bgColor;
        renderObject#attach (bgRect :> Rendering.nodeObject);
        rect <- Rect.{x=0.; y=0.; w=10.; h=10.}
end
