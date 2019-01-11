module Graphics = Platform.Windowing.Graphics

(*
let z = object
    method drawList = [
        `Rectangle Color.black;
        `Text "Hello";
    ]
end

(* OR have a composite step 
 *
 * So that widgets draw to an internal buffer
 * then a compositor takes over the rest *)

(*type 'a layout = 'a constraint 'a = Mixins.layout*)

type input =
    [ `Click
    | `Any
    | `Enter
    | `Focused
    | `KeyDown
    | `KeyUp
    | `Leave
    | `Move
    | `Paint
    | `Resize
    | `Unfocused ]

type output =
    [ `ClickArg of HandlesMouse.mouse_button * Pos.t
    | `EnterArg of Pos.t
    | `FocusedArg
    | `KeyDownArg of Keys.key
    | `KeyUpArg of Keys.key
    | `LeaveArg of Pos.t
    | `MoveArg of Pos.t
    | `PaintArg of Drawable.PWG.context
    | `ResizeArg of Rect.t
    | `UnfocusedArg ]
    *)

class ['a, 'b] basicWidget app = object(self)
    val id = 0
    val style = new Style.style
    val application = app
    val mutable eventHandlers = []
    val mutable snoopers = []
    val mutable rect = Rect.empty
    val mutable shouldClip = true
    val mutable layout : ('a, 'b) Layout.layout option = None
    val table = Hashtbl.Poly.create()
    val rev_table = Hashtbl.Poly.create()

    inherit Stylable.styleable
    inherit Drawable.drawable
    inherit ['a, 'b] Layoutable.layoutable
    inherit HandlesMouse.handlesMouse
    inherit HandlesKeyboard.handlesKeyboard
    inherit Focusable.focusable

    val events = HandlesEvent.create()

    method events : ('a, 'b) HandlesEvent.event_store = events

    method shouldClip = shouldClip
    method setShouldClip b = shouldClip <- b

    method setLayout l =
        layout <- Some l

    method invalidate : unit =
        app#redrawWidget (self :> ('a, 'b) basicWidget)

    method contentSize = 
        match layout with
        | Some l -> l#preferredSize
        | None -> self#size

    method fullRect =
        style#borderStyle#outsetRectByBorder rect

    method clipDrawArea cr =
        if shouldClip then Graphics.clip_rect cr self#fullRect

    method private sendEventToLayout event =
        Option.iter layout (fun l -> l#events#handle event)

    method paint cr =
        let open Drawable in
        self#sendEventToLayout HandlesEvent.(mkEvent `Paint (`PaintArg cr))

    (* TODO - move this into mixins
     *)
    method onResize r =
        let open Layoutable in
        rect <- style#borderStyle#insetRectByBorder r;
        self#sendEventToLayout HandlesEvent.(mkEvent `Resize (`ResizeArg rect));

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
