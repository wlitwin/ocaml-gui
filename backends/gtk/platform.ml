module CairoGraphics = struct
    type context = Cairo.context

    let save = Cairo.save
    let restore = Cairo.restore

    let set_color cr color =
        let open Color in
        Cairo.set_source_rgba cr color.r color.g color.b color.a

    let set_width = Cairo.set_line_width
    let move_to cr x y = Cairo.move_to cr x y
    let line_to cr x y = Cairo.line_to cr x y
    let stroke = Cairo.stroke
    let fill = Cairo.fill

    let font_weight_to_cairo = function 
        | Font.Normal -> Cairo.Normal
        | Font.Bold -> Cairo.Bold
    ;;

    let measure_text cr (font_info : Font.t) text : Font.metrics =
        let weight = font_weight_to_cairo font_info.weight in
        save cr;
        Cairo.set_font_size cr font_info.size;
        Cairo.select_font_face cr font_info.font ~weight;
        let fe = Cairo.font_extents cr in
        let te = Cairo.text_extents cr text in
        restore cr;
        (*Size.{w=(*te.width +. te.x_bearing +.*) te.x_advance; h=fe.ascent +. fe.descent}*)
        Font.{
            width = te.x_advance;
            x_advance = te.x_advance;
            x_bearing = te.x_bearing;
            ascent = fe.ascent;
            descent = fe.descent;
        }
    ;;

    let draw_text cr (font_info : Font.t) (rect : Rect.t) text =
        let open Rect in
        let open Color in
        save cr;
        Cairo.select_font_face cr font_info.font ~weight:(font_weight_to_cairo font_info.weight);
        Cairo.set_font_size cr font_info.size;
        let hint = measure_text cr font_info text in
        (*let color = style#fgColor in*)
        (*Cairo.set_source_rgba cr color.r color.g color.b color.a;*)
        set_color cr Color.black;
        let offset = Float.(max 0. (hint.width -. rect.w)) in
        let fe = Cairo.font_extents cr in
        (*Cairo.move_to cr (rect.x -. offset) (rect.y +. rect.h -. fe.Cairo.descent);*)
        Cairo.move_to cr (rect.x -. offset) (rect.y +. fe.Cairo.ascent);
        Cairo.show_text cr text;
        restore cr;
    ;;

    let rectangle cr rect =
        let open Rect in
        Cairo.rectangle cr rect.x rect.y rect.w rect.h

    let clip_reset cr =
        Cairo.clip_reset cr

    let clip_rect cr rect =
        rectangle cr rect;
        Cairo.clip cr
end

module Windowing : PlatformSig.WindowingSig = struct
    module Graphics = CairoGraphics
    
    type special_keys_state = {
        mutable ctrlDown : bool;
        mutable shiftDown : bool;
        mutable altDown : bool;
        mutable superDown : bool;
    }

    type context = {
        window : GWindow.window;
        special_keys : special_keys_state;
    }

    let normalize_key context (key : int) =
        let key = if key = 0xffe1 then 0xffe2 else key in
        if context.special_keys.shiftDown then (
            try key |> Char.of_int_exn |> Char.lowercase |> Char.to_int 
            with  _ -> key
        ) else key
    ;;

    let fix_key context (key : GdkEvent.Key.t) = 
        normalize_key context (GdkEvent.Key.keyval key)
        |> Keys.of_code
    ;;

    let create () =
        let _ = ignore(GMain.init()) in
        let gtk_window = GWindow.window 
            ~title:"No Title"
            ~width:100
            ~height:100
            ~position:`CENTER 
        () in
        {
            window=gtk_window;
            special_keys={
                ctrlDown=false;
                shiftDown=false;
                altDown=false;
                superDown=false;
            }
        }
    ;;

    let init
        context
        ~title 
        ~size 
        ~(draw : Graphics.context -> unit) 
        ~(resize : Size.t -> unit)
        ~(keyPress : Keys.key -> unit)
        ~(keyRelease : Keys.key -> unit)
    =
        let gtk_window = context.window in
        gtk_window#set_resizable true;
        gtk_window#misc#set_app_paintable true;
        gtk_window#misc#set_double_buffered true;
        let w, h = Float.(to_int size.Size.w, to_int size.Size.h) in
        gtk_window#misc#set_size_request ~width:w ~height:h ();
        gtk_window#set_title title;
        (*Stdio.printf "SCREEN WIDTH %d\n" (Gdk.Screen.width ());*)
        (*gtk_window#set_resize_mode `IMMEDIATE;*)
        ignore(gtk_window#event#connect#expose (fun event -> 
                let cr = Cairo_gtk.create gtk_window#misc#window in
                draw cr;
                true
            );
        );
        ignore(gtk_window#connect#destroy GMain.quit);
        ignore(gtk_window#event#connect#key_press (fun key -> 
            keyPress (fix_key context key);
            true
            );
        );
        ignore(gtk_window#event#connect#key_release (fun key -> 
            keyRelease (fix_key context key);
            true));
        ignore(gtk_window#event#connect#configure (fun evt -> 
                let module GC = GdkEvent.Configure in
                resize Size.{
                    w=Float.of_int (GC.width evt); 
                    h=Float.of_int (GC.height evt)};
                false
            )
        );
        gtk_window#show()
    ;;

    let graphics_context context = Cairo_gtk.create context.window#misc#window

    let set_title context title =
        context.window#set_title title

    let request_redraw context =
        GtkBase.Widget.queue_draw context.window#as_widget

    let run setup_func =
        let context = create() in
        setup_func context;
        GMain.main()
end
