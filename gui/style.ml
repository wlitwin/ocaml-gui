class style = object(self)
    val mutable bgColor = Color.white
    val mutable fgColor = Color.black;
    val mutable fontInfo = Text.default_font;
    val borderStyle = new BorderStyle.borderStyle

    method bgColor = bgColor
    method fgColor = fgColor
    method fontInfo = fontInfo

    method setBGColor color = bgColor <- color
    method setFGColor color = fgColor <- color
    method setFontInfo info = fontInfo <- info

    method borderStyle = borderStyle

    method fillBgColor cr (rect : Rect.t) =
        Cairo.set_source_rgba cr bgColor.r bgColor.g bgColor.b bgColor.a;
        begin match borderStyle#style with
        | NoBorder
        | Rectangle -> Cairo.rectangle cr rect.x rect.y rect.w rect.h
        | Rounded radius -> Paint.rounded_rect cr rect radius
        end;
        Cairo.fill cr
end

