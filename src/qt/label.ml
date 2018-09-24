open Def
open Rect

class label app text = object(self)
    inherit control app as super

    val mutable text : string = text
    val mutable fontSize : float = 32.0
    val mutable font : string = "Ubuntu Mono"
    val mutable weight : Cairo.weight = Cairo.Normal

    method measureText cr =
        let open Cairo in
        Cairo.save cr;
        Cairo.set_font_size cr fontSize;
        Cairo.select_font_face cr font ~weight;
        let fe = font_extents cr in
        let te = text_extents cr text in
        Cairo.restore cr;
        {w=te.width +. te.x_bearing; h=fe.ascent +. fe.descent}

    method setText new_text =
        text <- new_text

    method text = text

    method setFont name = font <- name
    method setFontSize size = fontSize <- size
    method setWeight new_weight = weight <- new_weight

    method sizeHint cr = self#measureText cr

    method paint cr =
        Cairo.select_font_face cr font ~weight;
        Cairo.set_font_size cr fontSize;
        (*let hint = self#measureText cr in*)
        let fe = Cairo.font_extents cr in
        (*let te = Cairo.text_extents cr text in*)
        Cairo.set_source_rgba cr 0. 0. 0. 1.;
        (*Cairo.move_to cr rect.x (rect.y +. hint.h);*)
        Cairo.move_to cr rect.x (rect.y +. fe.Cairo.ascent);
        Cairo.show_text cr text;
end

