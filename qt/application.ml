open Rect
open Style
open Simple_queue
open Control
open Event

type anchors = {
    tl : bool;
    tr : bool;
    bl : bool;
    br : bool;
}

let no_anchors = {
    tl = false; tr = false; bl = false; br = false;
}

type window_info = {
    anchors : anchors; (* Probably not right for this *)
    relative : bool;
}

let no_info = {
    anchors = no_anchors;
    relative = true;
}

let expose app drawing_area ev =
    let cr = Cairo_gtk.create drawing_area#misc#window in
    let allocation = drawing_area#misc#allocation in
    Util.timeit "draw" (fun _ ->
        try
            let w = float allocation.Gtk.width
            and h = float allocation.Gtk.height in
            app#resizeViewport cr Rect.{w;h};
            (*app#addEvent (Qt.Resize Rect.{w;h});*)
            app#addEvent (Layout cr);
            app#addEvent (Paint cr);
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

let keypress app w eid : bool =
    let key = fix_keycode (GdkEvent.Key.keyval eid) in
    app#addEvent (KeyPress key);
    Printf.printf "Key Pressed: %d (0x%x)\n" key key; flush stdout;
    w#present();
    true
;;

let keyrelease app w eid : bool =
    let key = fix_keycode (GdkEvent.Key.keyval eid) in
    Printf.printf "Key Released: %d (0x%x)\n" key key; flush stdout;
    app#addEvent (KeyRelease key);
    w#present();
    true
;;

class application = object(self)
    (* concept of z-order - Last window is top-most *)
    val windows : (window_handle, control * window_info) Hashtbl.t = Hashtbl.create 10
    (* Index (len - 1) == topmost control, list is in z-order *)
    val wnds : (control * window_info) DynArray.t = DynArray.create()
    val eventQueue : event queue = new queue
    val mutable viewport : size = {w=400.; h=400.}
    val mutable nextId = -1 
    val mutable ctrlDown = false
    val mutable shiftDown = false
    val mutable altDown = false
    val mutable superDown = false
    val mutable viewportSize = {w=0.; h=0.}

    method ctrlDown = ctrlDown
    method shiftDown = shiftDown
    method altDown = altDown
    method superDown = superDown

    method openWindow (rect : rect) (control : control) (window_info : window_info) : window_handle =
        nextId <- nextId + 1;
        Hashtbl.replace windows nextId (control, window_info);
        control#setId nextId;
        control#setGeometry rect;
        DynArray.add wnds (control, window_info);
        nextId

    method closeWindow (handle : window_handle) : unit =
        Hashtbl.remove windows handle;
        DynArray.filter (fun (ctrl, _) -> ctrl#id <> handle) wnds

    method private normalizeKey (key : int) =
        if shiftDown then
            try key |> Char.chr |> Char.lowercase_ascii |> Char.code
            with _ -> key
        else
            key

    method addEvent event =
        (* Work around GTK's weird key handling *)
        let event = match event with
                  | KeyPress key -> KeyPress (self#normalizeKey key)
                  | KeyRelease key -> KeyRelease (self#normalizeKey key)
                  | e -> e
        in
        eventQueue#enque event

    method printQueue =
        match eventQueue#head with
        | Some head ->
            Printf.printf "QUEUE LENGTH %d\n" eventQueue#length;
            Dllist.iter (fun it ->
                Printf.printf " == %s\n" (str_of_event it)
            ) head
        | None -> ()

    method viewportSize = viewportSize

    method resizeViewport (cr : Cairo.context) (size : size) =
        (* Go through and resize all the windows according to
         * their info *)
        (* TODO - come up with better types of resizing *)
        viewportSize <- size;
        Hashtbl.iter (fun _ (wnd, info : control * window_info) ->
            if info.relative then begin
                let r : rect = wnd#geom in
                let dx = size.w /. viewport.w
                and dy = size.h /. viewport.h in
                wnd#setGeometry {r with w=(r.w *. dx); h=(r.h *. dy)};
                wnd#relayout cr;
            end
        ) windows;
        viewport <- size

    method private checkControlKeys event =
        match event with
        | KeyPress 0xffe3   -> ctrlDown  <- true
        | KeyRelease 0xffe3 -> ctrlDown  <- false
        | KeyPress 0xffe9   -> altDown   <- true
        | KeyRelease 0xffe9 -> altDown   <- false
        | KeyPress 0xffeb   -> superDown <- true
        | KeyRelease 0xffeb -> superDown <- false
        | KeyPress 0xffe2   -> shiftDown <- true
        | KeyRelease 0xffe2 -> shiftDown <- false
        | _ -> ()

    method dispatch : unit =
        (*Printf.printf "DISPATCHING\n";
        self#printQueue;*)
        let rec loop () =
            begin match eventQueue#deque with
            | Some (Paint _ as event) ->
                DynArray.iter (fun (control, _) ->
                    control#event event
                ) wnds
            | Some event ->
                (* Track control characters *)
                self#checkControlKeys event;
                if DynArray.length wnds > 0 then begin
                    (*Printf.printf "Sending %s\n" (str_of_event event);*)
                    (DynArray.last wnds |> fst)#event event;
                end;
                loop()
            | None -> ()
            end
        in
        loop ()

    method main =
        ignore(GMain.init());
        let gtk_window = GWindow.window 
            ~title:"Gtk Demo" 
            ~width:400 
            ~height:400 
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

