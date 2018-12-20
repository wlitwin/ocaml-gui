type border_style = NoBorder
                  | Rectangle
                  | Rounded of float (* corner radius *)

class style = object(self)
    val mutable bgColor = Color.white
    val mutable fgColor = Color.black;
    val mutable borderColor = Color.none;
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
end

