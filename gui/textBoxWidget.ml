open Widget

type font_info = {
    mutable fontSize : float;
    mutable font : string;
    mutable weight : Cairo.weight;
}

class textBoxWidget app = object(self)
    inherit basicWidget app as super

    val mutable text : string = ""
    val font_info : font_info = {
        fontSize = 32.0;
        font = "Ubuntu Mono";
        weight = Cairo.Normal;
    }

    method setText new_text =
        text <- new_text

    method text = text

    method preferredSize =
        self#measureText Util.dummy_ctx 
        (if String.(=) text "" then "default_size" else text)

    method onKeyDown key =
        (match key with
        | Keys.Backspace -> text <- Util.strLeft text
        | key when Keys.is_printable key -> text <- text ^ Keys.to_string key
        | _ -> ());
        self#invalidate;
        Mixins.Propagate

    method measureText cr text =
        let open Cairo in
        Cairo.save cr;
        Cairo.set_font_size cr font_info.fontSize;
        Cairo.select_font_face cr font_info.font ~weight:font_info.weight;
        let fe = font_extents cr in
        let te = text_extents cr text in
        Cairo.restore cr;
        Size.{w=(*te.width +. te.x_bearing +.*) te.x_advance; h=fe.ascent +. fe.descent}

    method drawText cr =
        Cairo.select_font_face cr font_info.font ~weight:font_info.weight;
        Cairo.set_font_size cr font_info.fontSize;
        let hint = self#measureText cr text in
        let color = self#style#fgColor in
        Cairo.set_source_rgba cr color.r color.g color.b color.a;
        let offset = Float.(max 0. (hint.w -. rect.w)) in
        let fe = Cairo.font_extents cr in
        Cairo.move_to cr (rect.x -. offset) (rect.y +. fe.Cairo.ascent);
        Cairo.show_text cr text;

    method paint cr =
        begin if isFocused then
            Cairo.rectangle cr rect.x rect.y rect.w rect.h;
            Cairo.set_source_rgba cr 1. 0. 0. 1.;
            Cairo.set_line_width cr 10.;
            Cairo.stroke cr;
        end;
        self#drawText cr;
end
