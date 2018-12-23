type border_style = NoBorder
                  | Rectangle
                  | Rounded of float (* corner radius *)

class style = object(self)
    val mutable bgColor = Color.white
    val mutable fgColor = Color.black;
    val mutable borderColor = Color.black;
    val mutable borderSize = 1.0;
    val mutable fontInfo = Text.default_font;
    val mutable borderStyle = Rectangle

    method bgColor = bgColor
    method fgColor = fgColor
    method borderColor = borderColor
    method borderSize = borderSize
    method fontInfo = fontInfo
    method borderStyle = borderStyle

    method setBGColor color = bgColor <- color
    method setFGColor color = fgColor <- color
    method setBorderColor color = borderColor <- color
    method setBorderSize size = borderSize <- size
    method setFontInfo info = fontInfo <- info
    method setBorderStyle style = borderStyle <- style

    method hasBorder = 
        match borderStyle with
        | NoBorder -> false
        | _ -> true

    method outsetSizeByBorder size =
        if self#hasBorder then Size.{
            w = size.w +. borderSize *. 2.0;
            h = size.h +. borderSize *. 2.0;
        } else size

    method outsetRectByBorder rect =
        if self#hasBorder then Rect.{
            x = rect.x -. borderSize;
            y = rect.y -. borderSize;
            w = rect.w +. borderSize*.2.0;
            h = rect.h +. borderSize*.2.0;
        }
        else rect

    method insetRectByBorder rect =
        if self#hasBorder then Rect.{
            x = rect.x +. borderSize;
            y = rect.y +. borderSize;
            w = rect.w -. borderSize*.2.0;
            h = rect.h -. borderSize*.2.0;
        }
        else rect

    method fillBgColor cr (rect : Rect.t) =
        Cairo.set_source_rgba cr bgColor.r bgColor.g bgColor.b bgColor.a;
        begin match borderStyle with
        | NoBorder
        | Rectangle -> Cairo.rectangle cr rect.x rect.y rect.w rect.h
        | Rounded radius -> Paint.rounded_rect cr rect radius
        end;
        Cairo.fill cr

    method private setupBorderStyle cr =
        Cairo.set_source_rgba cr borderColor.r borderColor.g borderColor.b borderColor.a;
        Cairo.set_line_width cr borderSize;

    method private drawRectangleBorder cr (rect : Rect.t) =
        let rect = Rect.{
            x = rect.x +. borderSize*.0.5;
            y = rect.y +. borderSize*.0.5;
            w = rect.w -. borderSize;
            h = rect.h -. borderSize;
        } in
        Cairo.rectangle cr rect.x rect.y rect.w rect.h;
        self#setupBorderStyle cr;
        Cairo.stroke cr;

    method private drawRoundedBorder cr (rect : Rect.t) radius =
        Paint.rounded_rect cr rect radius;
        self#setupBorderStyle cr;
        Cairo.stroke cr;

    method drawBorder cr (rect : Rect.t) =
        match borderStyle with
        | NoBorder -> ()
        | Rectangle -> self#drawRectangleBorder cr rect
        | Rounded radius -> self#drawRoundedBorder cr rect radius

end

