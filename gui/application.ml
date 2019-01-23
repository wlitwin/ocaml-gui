type special_keys_state = {
    mutable ctrlDown : bool;
    mutable shiftDown : bool;
    mutable altDown : bool;
    mutable superDown : bool;
}

class ['a, 'b] application window ?(title="Window") size = 
object(self)
    val mutable viewport : Size.t = size
    val mutable widget : ('a, 'b) Widget.basicWidget option = None
    val mutable title : string = title
    val renderer = new Rendering.renderer
    val special_keys : special_keys_state = {
        ctrlDown=false;
        shiftDown=false;
        altDown=false;
        superDown=false;
    }

    method specialKeys = special_keys
    method renderer = renderer

    method title = title
    method setTitle t =
        title <- t;
        Platform.Windowing.set_title window t

    method graphicsContext =
        Platform.Windowing.graphics_context window

    method widget = Option.value_exn widget
    method setWidget w = 
        widget <- Some w;
        renderer#setRoot w#renderObject

    method resize (size : Size.t) =
         Util.timeit "resize" (fun _ ->
            let open Layoutable in
            renderer#setSize size;
            renderer#ignoreUpdates (fun _ ->
                self#widget#events#handle
                    HandlesEvent.(mkEvent `Resize (`ResizeArg Rect.{x=0.;y=0.;w=size.w;h=size.h}));
            );
            renderer#refreshFull
        );

    method private checkSuperKeys key upDown =
        match key with
        | Keys.LControl -> special_keys.ctrlDown <- upDown
        | Keys.LShift -> special_keys.shiftDown <- upDown
        | Keys.LAlt -> special_keys.altDown <- upDown
        | Keys.LSuper -> special_keys.superDown <- upDown
        | _ -> ()

    method private keyDown key =
        match key with
        | Keys.Enter -> 
            Platform.Windowing.request_redraw window
        | _ ->
            renderer#groupUpdates (fun _ ->
                self#checkSuperKeys key true;
                HandlesKeyboard.(self#widget#events#handle HandlesEvent.(mkEvent `KeyDown (`KeyDownArg key)))
            )

    method private keyUp key =
        self#checkSuperKeys key false;
        renderer#groupUpdates (fun _ ->
            HandlesKeyboard.(self#widget#events#handle HandlesEvent.(mkEvent `KeyUp (`KeyUpArg key)))
        )

    initializer
        renderer#setRequestDraw (fun _ ->
            Platform.Windowing.request_redraw window
        )

    method main =
        Platform.Windowing.init
        window
        ~title:"Window"
        ~size:size
        ~draw:(fun cr -> renderer#render cr)
        ~resize:(fun sz -> self#resize sz)
        ~keyPress:(fun key -> self#keyDown key)
        ~keyRelease:(fun key -> self#keyUp key);
end

