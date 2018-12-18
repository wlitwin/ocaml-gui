class style = object(self)
    val mutable bgColor = Color.white
    val mutable fgColor = Color.black;
    val mutable borderColor = Color.none;
    val mutable borderSize = 1.0;
    val mutable fontInfo = Text.default_font;

    method bgColor = bgColor
    method fgColor = fgColor
    method borderColor = borderColor
    method borderSize = borderSize
    method fontInfo = fontInfo

    method setBGColor color = bgColor <- color
    method setFGColor color = fgColor <- color
    method setBorderColor color = borderColor <- color
    method setBorderSize size = borderSize <- size
    method setFontInfo info = fontInfo <- info
end

