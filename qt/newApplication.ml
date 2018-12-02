open Rect

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
        ~width:(Float.to_int size.w)
        ~height:(Float.to_int size.h)
        ~position:`CENTER 
    () in
object(self)
    val mutable viewport : size = size
    val mutable widget : Widget.basicWidget = new Widget.basicWidget
    val mutable title : string = title
    val window : GWindow.window = gtk_window
    val special_keys : special_keys_state = {
        ctrlDown=false;
        shiftDown=false;
        altDown=false;
        superDown=false;
    }

    method title = title
    method setTitle t =
        title <- t;
        window#set_title t

    method widget = widget
    method setWidget w = widget <- w

    method redraw =
        window#coerce#misc#draw None

    method resize (size : Rect.size) =
        widget#resize Rect.{x=0.;y=0.;w=size.w;h=size.h};
        self#redraw;
        false

    method private keyDown key =
        let key = self#normalizeKey (GdkEvent.Key.keyval key) in
        Stdio.printf "Key Released: %d (0x%x)\n%!" key key;
        widget#onKeyDown (Keys.of_code key);
        true

    method private keyUp key =
        let key = self#normalizeKey (GdkEvent.Key.keyval key) in
        Stdio.printf "Key Released: %d (0x%x)\n%!" key key;
        widget#onKeyUp (Keys.of_code key);
        true

    method private expose drawing_area ev =
        let cr = Cairo_gtk.create drawing_area#misc#window in
        (*let allocation = drawing_area#misc#allocation in*)
        Util.timeit "draw" (fun _ ->
            try
                (*let w = Float.of_int allocation.Gtk.width
                and h = Float.of_int allocation.Gtk.height in*)
                widget#draw cr
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
        let d = GMisc.drawing_area ~packing:gtk_window#add () in
        gtk_window#set_resizable true;
        (*gtk_window#set_resize_mode `IMMEDIATE;*)
        ignore(d#event#connect#expose (fun event -> self#expose d event));
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

