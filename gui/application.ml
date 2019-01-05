type special_keys_state = {
    mutable ctrlDown : bool;
    mutable shiftDown : bool;
    mutable altDown : bool;
    mutable superDown : bool;
}

class application window ?(title="Window") size = 
object(self)
    val mutable viewport : Size.t = size
    val mutable widget : Widget.basicWidget option = None
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

    method redrawWidget (widget : Mixins.handlesEvent) : unit =
        self#redraw

    method redraw =
        Platform.Windowing.request_redraw window

    method resize (size : Size.t) =
         Util.timeit "resize" (fun _ ->
            self#widget#postEvent (Mixins.Resize Rect.{x=0.;y=0.;w=size.w;h=size.h}) |> ignore;
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
        self#checkSuperKeys key true;
        self#widget#postEvent (Mixins.KeyDown key) |> ignore;

    method private keyUp key =
        self#checkSuperKeys key false;
        self#widget#postEvent (Mixins.KeyUp key) |> ignore;

    method private draw (cr : Platform.Windowing.Graphics.context) =
        Util.timeit "draw" (fun _ ->
            try
                self#widget#postEvent (Mixins.Paint cr) |> ignore
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

