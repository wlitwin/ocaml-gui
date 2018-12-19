type special_keys_state = {
    mutable ctrlDown : bool;
    mutable shiftDown : bool;
    mutable altDown : bool;
    mutable superDown : bool;
}

class application ?(title="Window") size = 
    let _ = ignore(GMain.init()) in
    let gtk_window = GWindow.window 
        ~title
        ~width:(Float.to_int size.Size.w)
        ~height:(Float.to_int size.Size.h)
        ~position:`CENTER 
    () in
    let drawing_area = GMisc.drawing_area ~packing:gtk_window#add () in
object(self)
    val mutable viewport : Size.t = size
    val mutable widget : Widget.basicWidget option = None
    val mutable title : string = title
    val window : GWindow.window = gtk_window
    val drawing_area : GMisc.drawing_area = drawing_area
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
        window#set_title t

    method widget = Option.value_exn widget
    method setWidget w = widget <- Some w

    method redraw =
        GtkBase.Widget.queue_draw window#as_widget;

    method resize (size : Size.t) =
        Stdio.printf "Resizing application %f %f\n%!" size.w size.h;
        self#widget#resize Rect.{x=0.;y=0.;w=size.w;h=size.h};
        self#redraw;
        false

    method private checkSuperKeys key upDown =
        match key with
        | Keys.LControl -> special_keys.ctrlDown <- upDown
        | Keys.LShift -> special_keys.shiftDown <- upDown
        | Keys.LAlt -> special_keys.altDown <- upDown
        | Keys.LSuper -> special_keys.superDown <- upDown
        | _ -> ()

    method private keyDown key =
        let key = self#normalizeKey (GdkEvent.Key.keyval key) in
        Stdio.printf "Key Released: %d (0x%x)\n%!" key key;
        let key = Keys.of_code key in
        self#checkSuperKeys key true;
        self#widget#postEvent (Mixins.KeyDown key) |> ignore;
        true

    method private keyUp key =
        let key = self#normalizeKey (GdkEvent.Key.keyval key) in
        Stdio.printf "Key Released: %d (0x%x)\n%!" key key;
        let key = Keys.of_code key in
        self#checkSuperKeys key false;
        self#widget#postEvent (Mixins.KeyUp key) |> ignore;
        true

    method private expose ev =
        Stdio.printf "Main window got an expose event\n%!";
        let cr = Cairo_gtk.create drawing_area#misc#window in
        (*let allocation = drawing_area#misc#allocation in*)
        Util.timeit "draw" (fun _ ->
            try
                (*let w = Float.of_int allocation.Gtk.width
                and h = Float.of_int allocation.Gtk.height in*)
                self#widget#postEvent (Mixins.Paint cr) |> ignore
            with e ->
                Stdio.print_endline "==================== EXCEPTION OCCURRED ==================";
                Stdio.print_endline (Exn.to_string e);
                Backtrace.get() |> Backtrace.to_string |> Stdio.printf "%s\n%!"
        );
        true

    method private normalizeKey (key : int) =
        let key = if key = 0xffe1 then 0xffe2 else key in
        if special_keys.shiftDown then (
            try key |> Char.of_int_exn |> Char.lowercase |> Char.to_int 
            with  _ -> key
        ) else key

    method main =
        gtk_window#set_resizable true;
        (*gtk_window#set_resize_mode `IMMEDIATE;*)
        ignore(drawing_area#event#connect#expose (fun event -> self#expose event));
        ignore(gtk_window#connect#destroy GMain.quit);
        ignore(gtk_window#event#connect#key_press (fun key -> self#keyDown key));
        ignore(gtk_window#event#connect#key_release (fun key -> self#keyUp key));
        ignore(gtk_window#event#connect#configure (fun evt -> 
            let module GC = GdkEvent.Configure in
            self#resize Rect.{
                (*x=Float.of_int (GC.x evt);
                y=Float.of_int (GC.y evt);*)
                w=Float.of_int (GC.width evt); 
                h=Float.of_int (GC.height evt)})
        );
        gtk_window#show();
        self#redraw;
        GMain.main();
end

