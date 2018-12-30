type font_weight = Normal
                 | Bold

type font_info = {
    mutable fontSize : float;
    mutable font : string;
    mutable weight : font_weight;
}

let default_font = {
    fontSize = 24.0;
    font = "Ubuntu Mono";
    weight = Normal;
}

let measure_text cr font_info text =
    (*
    let open Cairo in
    Cairo.save cr;
    Cairo.set_font_size cr font_info.fontSize;
    Cairo.select_font_face cr font_info.font ~weight:font_info.weight;
    let fe = font_extents cr in
    let te = text_extents cr text in
    Cairo.restore cr;
    Size.{w=(*te.width +. te.x_bearing +.*) te.x_advance; h=fe.ascent +. fe.descent}
    *)
    Size.{w=10.; h=10.}
;;

let draw_text cr rect style text = 
    (*
    let open Rect in
    let open Color in
    Cairo.save cr;
    let font_info = style#fontInfo in
    Cairo.select_font_face cr font_info.font ~weight:font_info.weight;
    Cairo.set_font_size cr font_info.fontSize;
    let hint = measure_text ~cr font_info text in
    let color = style#fgColor in
    Cairo.set_source_rgba cr color.r color.g color.b color.a;
    let offset = Float.(max 0. (hint.w -. rect.w)) in
    let fe = Cairo.font_extents cr in
    (*Cairo.move_to cr (rect.x -. offset) (rect.y +. rect.h -. fe.Cairo.descent);*)
    Cairo.move_to cr (rect.x -. offset) (rect.y +. fe.Cairo.ascent);
    Cairo.show_text cr text;
    Cairo.restore cr;
    *)
    ()
;;
