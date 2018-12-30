module Graphics = Platform.Windowing.Graphics

type border_type = NoBorder
                 | Rectangle
                 | Rounded of float (* corner radius *)

class borderStyle = object(self)
    val mutable color = Color.black;
    val mutable size = 1.0;
    val mutable style = NoBorder

    method setStyle _style = style <- _style
    method setColor _color = color <- _color
    method setSize _size = size <- _size

    method style = style
    method size = size
    method color = color

    method hasBorder = Poly.(<>) style NoBorder

    method outsetSizeByBorder _size =
        if self#hasBorder then Size.{
            w = _size.w +. size *. 2.0;
            h = _size.h +. size *. 2.0;
        } else _size

    method outsetRectByBorder rect =
        if self#hasBorder then Rect.{
            x = rect.x -. size;
            y = rect.y -. size;
            w = rect.w +. size*.2.0;
            h = rect.h +. size*.2.0;
        }
        else rect

    method insetRectByBorder rect =
        if self#hasBorder then Rect.{
            x = rect.x +. size;
            y = rect.y +. size;
            w = rect.w -. size*.2.0;
            h = rect.h -. size*.2.0;
        }
        else rect

    method private setupBorderStyle cr =
        Graphics.set_color cr color;
        Graphics.set_width cr size;

    method private drawRectangleBorder cr (rect : Rect.t) =
        let rect = Rect.{
            x = rect.x +. size*.0.5;
            y = rect.y +. size*.0.5;
            w = rect.w -. size;
            h = rect.h -. size;
        } in
        Graphics.rectangle cr rect;
        self#setupBorderStyle cr;
        Graphics.stroke cr;

    method private drawRoundedBorder cr (rect : Rect.t) radius =
        (*Paint.rounded_rect cr rect radius;
        self#setupBorderStyle cr;
        Cairo.stroke cr;*)
        ()

    method drawBorder cr (rect : Rect.t) =
        match style with
        | NoBorder -> ()
        | Rectangle -> self#drawRectangleBorder cr rect
        | Rounded radius -> self#drawRoundedBorder cr rect radius

end
