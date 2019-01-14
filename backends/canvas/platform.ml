module CanvasGraphics = struct
    open Js_of_ocaml
    module Html = Dom_html

    type context = {
        canvas : Html.canvasRenderingContext2D Js.t;
        canvas_elem : Html.canvasElement Js.t;
    }

    let create_div () =
        let doc = Html.window##.document in
        let div = Html.createDiv doc in
        div##.style##.width := Js.string "100%";
        div##.style##.height := Js.string "100%";
        div

    let create () = 
       let doc = Html.window##.document in
       let body = doc##.body in
       let canvas = Dom_html.createCanvas doc in
       canvas##.id := Js.string "canvas";
       canvas##.width  := body##.offsetWidth;
       canvas##.height := body##.offsetHeight;
       Dom.appendChild body canvas;
       let c = canvas##getContext Html._2d_ in
       c##.strokeStyle := Js.string "rgb(0, 0, 0, 0)";
       c##.lineWidth := 0.;
       Js.export "cvar" c;
       { canvas=c; canvas_elem=canvas; }

    let translate context x y =
        context.canvas##translate x y

     let identity_transform context =
         Js.Unsafe.(coerce context.canvas)##resetTransform

    let save context =
        context.canvas##save

    let restore context =
        context.canvas##restore

    let set_color context (color : Color.t) =
        let color = Color.scale_by color 255.0 in
        let style = Printf.sprintf "rgb(%f, %f, %f, %f)" color.r color.g color.b color.a in
        context.canvas##.fillStyle := (Js.string style);
        context.canvas##.strokeStyle := (Js.string style);
    ;;

    let setup_font_info context font_info =
        let open Font in
        let style = match font_info.weight with
                    | Normal -> "400"
                    | Bold -> "700"
        in
        let size = Printf.sprintf "%dpx" Float.(to_int font_info.size) in
        let font_string = Printf.sprintf "%s %s '%s'" style size font_info.font in
        context.canvas##.font := Js.string font_string
    ;;

    let measure_text context font_info text =
        save context;
        setup_font_info context font_info;
        let metrics = context.canvas##measureText (Js.string text) in
        restore context;
        let width = (Js.Unsafe.coerce metrics)##.width in
        let x_bearing = width in
        let x_advance = width in
        (* HTML+Javascript suck balls, apparently there's no text/font metrics
         * beyond the width... *)
        let ascent = font_info.size*.0.9 in
        let descent = font_info.size*.0.1 in
        Font.{
            width;
            x_bearing;
            x_advance;
            ascent;
            descent;
        }
    ;;

    let set_font_info cr font_info =
        setup_font_info cr font_info

    let draw_text_ context (pos : Pos.t) text =
        context.canvas##fillText (Js.string text) pos.x pos.y;
    ;;

    let draw_text context font_info (rect : Rect.t) text =
        save context;
        setup_font_info context font_info;
        let metrics = measure_text context font_info text in
        (*Caml.print_endline Printf.(sprintf "Metrics width %f bearing %f advance %f ascent %f descent %f\nRECT %f %f\n"
            metrics.width
            metrics.x_bearing
            metrics.x_advance
            metrics.ascent
            metrics.descent
            rect.x
            rect.y);*)
        context.canvas##.fillStyle := Js.string "black";
        let offset = Float.(max 0. (metrics.width -. rect.w)) in
        context.canvas##fillText (Js.string text) (rect.x -. offset) (rect.y +. metrics.ascent);
        restore context;
    ;;

    let set_width context width =
        context.canvas##.lineWidth := width

    let move_to context x y =
        context.canvas##moveTo x y

    let line_to context x y =
        context.canvas##lineTo x y

    let stroke context = 
        context.canvas##stroke

    let fill context =
        context.canvas##fill

    let rectangle context (rect : Rect.t) =
        context.canvas##beginPath;
        context.canvas##rect rect.x rect.y rect.w rect.h

    let clip_rect context (rect : Rect.t) =
        (*context.canvas##rect rect.x rect.y rect.w rect.h;*)
        rectangle context rect;
        context.canvas##stroke;
        context.canvas##clip

    let clip_reset context =
        let w = Float.of_int context.canvas_elem##.width in
        let h = Float.of_int context.canvas_elem##.height in
        clip_rect context Rect.{x=0.; y=0.; w; h};
end

module Windowing : PlatformSig.WindowingSig = struct
    module Graphics = CanvasGraphics

    type context = {
        mutable canvas : Graphics.context;
        mutable draw       : Graphics.context -> unit;
        mutable resize     : Size.t -> unit;
        mutable keyPress   : Keys.key -> unit;
        mutable keyRelease : Keys.key -> unit;
        mutable draw_callback : (float -> unit) Js.callback;
    }

    let create () = {
        canvas = Graphics.create();
        draw = ignore;
        resize = ignore;
        keyPress = ignore;
        keyRelease = ignore;
        draw_callback = Js.wrap_callback ignore
    }

    let wrapped_draw context =
        Dom_html.window##requestAnimationFrame 
            context.draw_callback
            |> ignore
    ;;

    let init
        (context : context)
        ~(title : string)
        ~(size : Size.t)
        ~(draw : Graphics.context -> unit) 
        ~(resize : Size.t -> unit)
        ~(keyPress : Keys.key -> unit)
        ~(keyRelease : Keys.key -> unit)
    =
        context.draw <- draw;
        context.resize <- resize;
        context.keyPress <- keyPress;
        context.keyRelease <- keyRelease;
        Dom_html.window##.document##.title := (Js.string title);
        context.draw_callback <- Js.wrap_callback (fun _ ->
            context.draw context.canvas
        );
    ;;

    let graphics_context context = context.canvas

    let set_title context (new_title : string) : unit =
        Dom_html.window##.document##.title := (Js.string new_title)

    let request_redraw context =
        wrapped_draw context
    ;;

    let body () = Dom_html.window##.document##.body

    let run setup_func =
        Dom_html.window##.onload := Dom_html.handler (fun _ ->
            let context = create() in
            setup_func context;
            let window = Dom_html.window in 
            window##.onresize := Dom_html.handler (fun evt -> 
                (* Resize the canvas *)
                let doc = Dom_html.window##.document in
                let body = doc##.body in
                let w, h = body##.offsetWidth, body##.offsetHeight in
                context.canvas.canvas_elem##.width := w;
                context.canvas.canvas_elem##.height := h;
                context.resize Size.{w=Float.of_int w; h=Float.of_int h};
                wrapped_draw context;
                Js._true
            );
            window##.onkeydown := Dom_html.handler (fun evt ->
                let code : Js.js_string Js.t Js.optdef = evt##.code in
                context.keyPress (KeyConverter.convert_code_to_key code);
                Dom.preventDefault evt;
                Dom_html.stopPropagation evt;
                Js._false;
            );
            window##.onkeyup := Dom_html.handler (fun evt ->
                context.keyRelease (KeyConverter.convert_code_to_key evt##.code);
                Dom.preventDefault evt;
                Dom_html.stopPropagation evt;
                Js._false;
            );
            let body = body() in
            let w, h = body##.offsetWidth, body##.offsetHeight in
            context.resize Size.{w=Float.of_int w; h=Float.of_int h};
            wrapped_draw context;
            Js._false
        )
    ;;
end
