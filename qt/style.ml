class style = object(self)
    val mutable bgColor = Rect.white
    val mutable fgColor = Rect.black;
    val mutable borderColor = Rect.none;
    val mutable borderSize = 1.0;

    method bgColor = bgColor
    method fgColor = fgColor
    method borderColor = borderColor
    method borderSize = borderSize

    method setBGColor color = bgColor <- color
    method setFGColor color = fgColor <- color
    method setBorderColor color = borderColor <- color
    method setBorderSize size = borderSize <- size
end

