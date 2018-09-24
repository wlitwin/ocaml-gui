let app = new Qt.application 

let expose drawing_area ev =
    let cr = Cairo_gtk.create drawing_area#misc#window in
    let allocation = drawing_area#misc#allocation in
    Util.timeit "draw" (fun _ ->
        try
            let w = float allocation.Gtk.width
            and h = float allocation.Gtk.height in
            app#resizeViewport cr Rect.{w;h};
            (*app#addEvent (Qt.Resize Rect.{w;h});*)
            app#addEvent (Qt.Layout cr);
            app#addEvent (Qt.Paint cr);
            app#dispatch;
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
(*
    if key >= 65 && key <= 90 then
        key + 32
    else
        key
        *)

let keyrelease w eid : bool =
    let key = fix_keycode (GdkEvent.Key.keyval eid) in
    Printf.printf "Key Released: %d (0x%x)\n" key key; flush stdout;
    app#addEvent (Qt.KeyRelease key);
    (*G.key_release gui_state key;*)
    w#present();
    true
;;

let keypress w eid : bool =
    let key = fix_keycode (GdkEvent.Key.keyval eid) in
    (*G.key_press gui_state key; *)
    app#addEvent (Qt.KeyPress key);
    Printf.printf "Key Pressed: %d (0x%x)\n" key key; flush stdout;
    w#present();
    true
;;

let _ =
    let tile = new QtTextBox.textBox app in
    app#openWindow Rect.{x=0.; y=0.; w=400.; h=400.}
                   (tile :> Qt.control)
                   Qt.no_info |> ignore;

    ignore(GMain.init());
    let gtk_window = GWindow.window ~title:"Gtk Demo" ~width:400 ~height:400 ~position:`CENTER () in
    let d = GMisc.drawing_area ~packing:gtk_window#add () in
    ignore(d#event#connect#expose (expose d));
    ignore(gtk_window#connect#destroy GMain.quit);
    ignore(gtk_window#event#connect#key_press (keypress gtk_window));
    ignore(gtk_window#event#connect#key_release (keyrelease gtk_window));
    gtk_window#show();
    GMain.main();
