open Rect

let expose app drawing_area ev =
    let cr = Cairo_gtk.create drawing_area#misc#window in
    let allocation = drawing_area#misc#allocation in
    Util.timeit "draw" (fun _ ->
        try
            let w = float allocation.Gtk.width
            and h = float allocation.Gtk.height in
            (*app#resizeViewport cr Rect.{w;h};
            (*app#addEvent (Qt.Resize Rect.{w;h});*)
            app#addEvent (Layout cr);
            app#addEvent (Paint cr);
            app#dispatch;
            *)
            ()
        with e ->
            print_endline "==================== EXCEPTION OCCURRED ==================";
            print_endline (Printexc.to_string e);
            Printexc.print_backtrace stdout;
            flush stdout;
            flush stderr;
    );
    true
;;

let fix_keycode key = (*Util.id key *)
    if key = 0xffe1 then 0xffe2
    else key
;;

let keypress app w eid : bool =
    let key = fix_keycode (GdkEvent.Key.keyval eid) in
    (*w#present();*)
    true
;;

let keyrelease app w eid : bool =
    let key = fix_keycode (GdkEvent.Key.keyval eid) in
    Printf.printf "Key Released: %d (0x%x)\n" key key; flush stdout;
    (*w#present();*)
    true
;;

type special_keys_state = {
    mutable ctrlDown : bool;
    mutable shiftDown : bool;
    mutable altDown : bool;
    mutable superDown : bool;
}

class application = object(self)
    val mutable viewport : size = {w=400.; h=400.}
    val special_keys : special_keys_state = {
        ctrlDown=false;
        shiftDown=false;
        altDown=false;
        superDown=false;
    }

    method private normalizeKey (key : int) =
        match special_keys.shiftDown with
        | true -> key |> Char.chr |> Char.lowercase_ascii |> Char.code
        | false -> key
        | exception _ -> key

    method main =
        ignore(GMain.init());
        let gtk_window = GWindow.window 
            ~title:"Gtk Demo" 
            ~width:viewport.w
            ~height:viewport.h
            ~position:`CENTER 
        () in
        let d = GMisc.drawing_area ~packing:gtk_window#add () in
        ignore(d#event#connect#expose (expose self d));
        ignore(gtk_window#connect#destroy GMain.quit);
        ignore(gtk_window#event#connect#key_press (keypress self gtk_window));
        ignore(gtk_window#event#connect#key_release (keyrelease self gtk_window));
        gtk_window#show();
        GMain.main();

end

