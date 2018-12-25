class textBoxWidget app = object(self)
    inherit Widget.basicWidget app as super

    val mutable text : string = ""

    method setText new_text =
        Stdio.printf "Setting text %s\n" new_text;
        text <- new_text

    method text = text

    method contentSize =
        self#measureText Util.dummy_ctx 
        (if String.(=) text "" then "default_size" else text)

    method measureText cr text =
        Text.measure_text ~cr style#fontInfo text

    method drawText cr =
        Text.draw_text cr rect style text

        (*
    method paint cr =
        let full = self#fullRect in
        if isFocused then begin
            style#borderStyle#setColor Color.red;
            style#borderStyle#setSize 5.0;
            style#borderStyle#setStyle Rectangle;
            self#resize full
            (*
            Cairo.rectangle cr rect.x rect.y rect.w rect.h;
            Cairo.set_source_rgba cr 1. 0. 0. 1.;
            Cairo.set_line_width cr 10.;
            Cairo.stroke cr;
            *)
        end else begin
            style#borderStyle#setStyle NoBorder;
            self#resize full
        end;
        self#drawText cr;
        *)
    method onKeyDown key =
        (match key with
        | Keys.Backspace -> text <- Util.strLeft text
        | key when Keys.is_printable key -> text <- text ^ Keys.to_string key
        | _ -> ());
        self#invalidate;
        Mixins.Propagate

    method! paint cr =
        self#drawText cr

    initializer
        style#setBGColor Color.white;
        style#borderStyle#setStyle Rectangle;
        style#borderStyle#setSize 2.0;
        style#borderStyle#setColor Color.black;
end
