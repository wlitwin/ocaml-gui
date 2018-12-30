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
       c##.lineWidth := 0.;
       Js.export "cvar" c;
       { canvas=c; canvas_elem=canvas; }

    let save context =
        context.canvas##save

    let restore context =
        context.canvas##restore

    let set_color context (color : Color.t) =
        let color = Color.scale_by color 255.0 in
        let style = Printf.sprintf "rgb(%f, %f, %f, %f)" color.r color.g color.b color.a in
        context.canvas##.fillStyle := (Js.string style)

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

    let clip_rect context (rect : Rect.t) =
        context.canvas##rect rect.x rect.y rect.w rect.h;
        context.canvas##stroke;
        context.canvas##clip

    let clip_reset context =
        (* TODO !? *)
        ()

    let measure_text context text =
        ()

    let draw_text context text =
        ()

    let rectangle context (rect : Rect.t) =
        context.canvas##beginPath;
        context.canvas##rect rect.x rect.y rect.w rect.h
end

module Windowing : PlatformSig.WindowingSig = struct
    module Graphics = CanvasGraphics

    type context = {
        mutable canvas : Graphics.context;
        mutable draw       : Graphics.context -> unit;
        mutable resize     : Size.t -> unit;
        mutable keyPress   : Keys.key -> unit;
        mutable keyRelease : Keys.key -> unit;
    }

    let create () = {
        canvas = Graphics.create();
        draw = ignore;
        resize = ignore;
        keyPress = ignore;
        keyRelease = ignore;
    }

    let wrapped_draw context =
        Graphics.save context.canvas;
        context.draw context.canvas;
        Graphics.restore context.canvas;
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
    ;;

    let set_title context (new_title : string) : unit =
        Dom_html.window##.document##.title := (Js.string new_title)

    let request_redraw context =
        wrapped_draw context
    ;;

    let body () = Dom_html.window##.document##.body

    let run context =
        let window = Dom_html.window in 
        window##.onresize := Dom_html.handler (fun evt -> 
            (* Resize the canvas *)
            let doc = Dom_html.window##.document in
            let body = doc##.body in
            let w, h = body##.offsetWidth, body##.offsetHeight in
            Caml.print_endline Printf.(sprintf "Window resized %d %d\n" w h);
            context.canvas.canvas_elem##.width := w;
            context.canvas.canvas_elem##.height := h;
            context.resize Size.{w=Float.of_int w; h=Float.of_int h};
            wrapped_draw context;
            Js._true
        );
        let body = body() in
        let w, h = body##.offsetWidth, body##.offsetHeight in
        context.resize Size.{w=Float.of_int w; h=Float.of_int h};
        wrapped_draw context
    ;;
end
