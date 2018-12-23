open Widget
open Text

class label ?(text="") app = object(self)
    inherit basicWidget app as super

    val mutable text : string = text

    method setText new_text =
        text <- new_text

    method text = text

    method contentSize =
        self#measureText Util.dummy_ctx 
        (if String.(=) text "" then "default_size" else text)

    method measureText cr text =
        Text.measure_text ~cr style#fontInfo text

    method drawText cr =
        Text.draw_text cr rect style text

    initializer
        style#setBorderStyle Rectangle

    method paint cr =
        let full = self#fullRect in
        if isFocused then begin
            style#setBorderColor Color.red;
            style#setBorderSize 5.0;
            self#resize full
            (*
            Cairo.rectangle cr rect.x rect.y rect.w rect.h;
            Cairo.set_source_rgba cr 1. 0. 0. 1.;
            Cairo.set_line_width cr 10.;
            Cairo.stroke cr;
            *)
        end else begin
            style#setBorderColor Color.black;
            style#setBorderSize 1.0;
            self#resize full
        end;
        self#drawText cr;
end
