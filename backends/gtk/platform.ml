module CairoGraphics = struct
    type context = Cairo.context

    let save = Cairo.save
    let restore = Cairo.restore

    let set_color cr color =
        let open Color in
        Cairo.set_source_rgba cr color.r color.g color.b color.a

    type image = {
        size : Size.t;
        image : Cairo.Surface.t;
    }

    let create_image w h =
        let module CI = Cairo.Image in
        { image = CI.create CI.ARGB32 w h;
          size = Size.(Float.{w=of_int w; h=of_int h}) }

    let set_width = Cairo.set_line_width
    let move_to cr x y = Cairo.move_to cr x y
    let line_to cr x y = Cairo.line_to cr x y
    let stroke = Cairo.stroke
    let fill = Cairo.fill

    let font_weight_to_cairo = function 
        | Font.Normal -> Cairo.Normal
        | Font.Bold -> Cairo.Bold
    ;;

    let translate cr x y =
        Cairo.translate cr x y

    let identity_transform cr =
        Cairo.identity_matrix cr

    let measure_text cr (font_info : Font.t) text : Font.metrics =
        let weight = font_weight_to_cairo font_info.weight in
        (*save cr;*)
        Cairo.set_font_size cr font_info.size;
        Cairo.select_font_face cr font_info.font ~weight;
        let fe = Cairo.font_extents cr in
        let te = Cairo.text_extents cr text in
        (*restore cr;*)
        (*Size.{w=(*te.width +. te.x_bearing +.*) te.x_advance; h=fe.ascent +. fe.descent}*)
        Font.{
            width = te.x_advance;
            x_advance = te.x_advance;
            x_bearing = te.x_bearing;
            ascent = fe.ascent;
            descent = fe.descent;
        }
    ;;

    let dummy_ctx = 
        let module CI = Cairo.Image in
        let img = CI.create CI.ARGB32 1 1 in
        Cairo.create img
    ;;

    let measure_text_no_context (font, text) =
        measure_text dummy_ctx font text


    let font_extents_no_context (font_info : Font.t) : Font.font_extents = 
        let cr = dummy_ctx in
        Cairo.select_font_face cr font_info.font ~weight:(font_weight_to_cairo font_info.weight);
        Cairo.set_font_size cr font_info.size;
        let fe = Cairo.font_extents cr in
        {ascent=fe.ascent;
         descent=fe.descent;
         baseline=fe.baseline;
         max_x_advance=fe.max_x_advance;
         max_y_advance=fe.max_y_advance}

    let set_font_info cr (font_info : Font.t) =
        Cairo.select_font_face cr font_info.font ~weight:(font_weight_to_cairo font_info.weight);
        Cairo.set_font_size cr font_info.size;
    ;;

    let draw_text_ cr (pos : Pos.t) text =
        Cairo.move_to cr pos.x pos.y;
        Cairo.show_text cr text;
    ;;

    let draw_text cr (font_info : Font.t) (rect : Rect.t) text =
        let open Rect in
        let open Color in
        (*save cr;*)
        Cairo.select_font_face cr font_info.font ~weight:(font_weight_to_cairo font_info.weight);
        Cairo.set_font_size cr font_info.size;
        (*let hint = measure_text cr font_info text in*)
        (*let color = style#fgColor in*)
        (*Cairo.set_source_rgba cr color.r color.g color.b color.a;*)
        set_color cr Color.black;
        (*let offset = Float.(max 0. (hint.width -. rect.w)) in*)
        let fe = Cairo.font_extents cr in
        (*Cairo.move_to cr (rect.x -. offset) (rect.y +. rect.h -. fe.Cairo.descent);*)
        (*Cairo.move_to cr (rect.x -. offset) (rect.y +. fe.Cairo.ascent);*)
        Cairo.move_to cr rect.x (rect.y +. fe.Cairo.ascent);
        Cairo.show_text cr text;
        (*restore cr;*)
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
        mutable backing_image : Graphics.image;
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
            };
            backing_image=Graphics.create_image 100 100
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
                let img = context.backing_image.image in
                let cr = Cairo.create img in
                draw cr;
                let cr = Cairo_gtk.create gtk_window#misc#window in
                Cairo.set_source_surface cr img ~x:0. ~y:0.;
                Cairo.paint cr;
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
                let w = GC.width evt
                and h = GC.height evt in
                context.backing_image <- Graphics.create_image w h;
                resize Size.{
                    w=Float.of_int w; 
                    h=Float.of_int h};
                false
            )
        );
        gtk_window#show()
    ;;

    let dummy_ctx = 
        let module CI = Cairo.Image in
        let img = CI.create CI.ARGB32 1 1 in
        Cairo.create img
    ;;

    let graphics_context context = dummy_ctx

    let set_title context title =
        context.window#set_title title

    let request_redraw context =
        GtkBase.Widget.queue_draw context.window#as_widget

    let run setup_func =
        let context = create() in
        setup_func context;
        GMain.main()
end
