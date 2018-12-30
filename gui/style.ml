module Graphics = Platform.Windowing.Graphics

class style = object(self)
    val mutable bgColor = Color.gray
    val mutable fgColor = Color.black
    val mutable fontInfo = Font.default_font
    val borderStyle = new BorderStyle.borderStyle

    method bgColor = bgColor
    method fgColor = fgColor
    method fontInfo = fontInfo

    method setBGColor color = bgColor <- color
    method setFGColor color = fgColor <- color
    method setFontInfo info = fontInfo <- info

    method borderStyle = borderStyle

    method fillBgColor cr (rect : Rect.t) =
        Graphics.set_color cr bgColor;
        begin match borderStyle#style with
        | NoBorder
        | Rectangle -> Graphics.rectangle cr rect
        | Rounded radius -> (*Paint.rounded_rect cr rect radius*) ()
        end;
        Graphics.fill cr
end

