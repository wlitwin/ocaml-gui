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
    val special_keys : special_keys_state = {
        ctrlDown=false;
        shiftDown=false;
        altDown=false;
        superDown=false;
    }

    method specialKeys = special_keys

    method title = title
    method setTitle t =
        title <- t;
        Platform.Windowing.set_title window t

    method graphicsContext =
        Platform.Windowing.graphics_context window

    method widget = Option.value_exn widget
    method setWidget w = widget <- Some w

    val mutable draw_list = []

    method redrawWidget (widget : ('a, 'b) HandlesEvent.handles_event) : unit =
        draw_list <- widget :: draw_list;
        Platform.Windowing.request_redraw window

    method redraw =
        draw_list <- [];
        Platform.Windowing.request_redraw window

    method resize (size : Size.t) =
         Util.timeit "resize" (fun _ ->
            let open Layoutable in
            self#widget#events#handle HandlesEvent.(mkEvent `Resize (`ResizeArg Rect.{x=0.;y=0.;w=size.w;h=size.h}));
            ()
        );
        self#redraw;

    method private checkSuperKeys key upDown =
        match key with
        | Keys.LControl -> special_keys.ctrlDown <- upDown
        | Keys.LShift -> special_keys.shiftDown <- upDown
        | Keys.LAlt -> special_keys.altDown <- upDown
        | Keys.LSuper -> special_keys.superDown <- upDown
        | _ -> ()

    method private keyDown key =
        (match key with
        | Keys.Enter -> self#redraw
        | _ -> 
            self#checkSuperKeys key true;
            HandlesKeyboard.(self#widget#events#handle HandlesEvent.(mkEvent `KeyDown (`KeyDownArg key)))
        );

    method private keyUp key =
        self#checkSuperKeys key false;
        HandlesKeyboard.(self#widget#events#handle HandlesEvent.(mkEvent `KeyUp (`KeyUpArg key)))

    method private draw (cr : Platform.Windowing.Graphics.context) =
        Util.timeit "draw" (fun _ ->
            try
                self#widget#onDraw;
                Util.timeit "sort tree + draw tree" (fun _ ->
                    Rendering.draw cr self#widget#renderObject
                )
                (*
                let event = HandlesEvent.(mkEvent `Paint (`PaintArg cr)) in
                begin match draw_list with
                | [] -> Drawable.(self#widget#events#handle event)
                | lst -> 
                        List.iter lst (fun w -> 
                            w#events#handle event)
                end;
                draw_list <- []
                *)
            with e ->
                (*Stdio.print_endline "==================== EXCEPTION OCCURRED ==================";
                Stdio.print_endline (Exn.to_string e);
                Backtrace.get() |> Backtrace.to_string |> Stdio.printf "%s\n%!"*)
                ()
        );

    method main =
        Platform.Windowing.init
        window
        ~title:"Window"
        ~size:size
        ~draw:(fun cr -> self#draw cr)
        ~resize:(fun sz -> self#resize sz)
        ~keyPress:(fun key -> self#keyDown key)
        ~keyRelease:(fun key -> self#keyUp key);
end

