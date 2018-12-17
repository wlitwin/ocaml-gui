class style = object(self)
    val mutable bgColor = Color.white
    val mutable fgColor = Color.black;
    val mutable borderColor = Color.none;
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

