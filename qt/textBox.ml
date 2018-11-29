open Rect
open Control

class textBox app = object(self)
    inherit control app as super

    val mutable text : string = ""
    val mutable fontSize : float = 32.0
    val mutable font : string = "Ubuntu Mono"
    val mutable weight : Cairo.weight = Cairo.Normal
    val mutable keyFilter : int -> bool = (fun _ -> true)
    val mutable controlDown = false

    method setKeyFilter f = keyFilter <- f

    method keyPress (key : int) =
        match key with
        | 0xffe3 (* left control *) -> controlDown <- true
        | 0xff08 (*backspace*) -> text <- Util.strLeft text
        | ch -> try
            match Char.of_int ch with
            | Some (' ' .. '~' as ch) when keyFilter key -> text <- text ^ String.make 1 ch
            | _ -> ()
        with _ -> ()

    method keyRelease (key : int) =
        match key with
        | 0xffe3 (* left control *) -> controlDown <- false
        | _ -> ()

    method measureText cr text =
        let open Cairo in
        Cairo.save cr;
        Cairo.set_font_size cr fontSize;
        Cairo.select_font_face cr font;
        let fe = font_extents cr in
        let te = text_extents cr text in
        Cairo.restore cr;
        {w=(*te.width +. te.x_bearing +.*) te.x_advance; h=fe.ascent +. fe.descent}

    method setText new_text =
        text <- new_text

    method text = text

    method sizeHint cr =
        if String.(=) text "" then
            self#measureText cr "default_size"
        else
            self#measureText cr text

    method paint cr =
        let open Float in
        (*Printf.printf "PAINTIN TEXT %f %f %f %f %s\n" rect.x rect.y rect.w rect.h text;*)
        Cairo.select_font_face cr font ~weight;
        Cairo.set_font_size cr fontSize;
        let hint = self#measureText cr text in
        let color = self#style#fgColor in
        Cairo.set_source_rgba cr color.r color.g color.b color.a;
        let offset = max 0. (hint.w -. rect.w +. if self#isFocused then 4. else 0.) in
        let fe = Cairo.font_extents cr in
        Cairo.move_to cr (rect.x -. offset) (rect.y +. fe.Cairo.ascent);
        Cairo.show_text cr text;
        if self#isFocused then begin
            let open Cairo in
            let te = text_extents cr text in
            Cairo.set_line_width cr 14.;
            Cairo.move_to cr (rect.x +. te.x_advance -. offset +. 7.)
                             (rect.y);
            Cairo.rel_line_to cr 0. hint.h;
            Cairo.stroke cr;
        end
    end

