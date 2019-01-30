class ['a, 'b] label ?(text="") app = 
    let start_text = text in
object(self)
    inherit ['a, 'b] Widget.basicWidget app as super

    val textObject = app#renderer#createTextObject

    method setText (text : string) : unit = textObject#setText text
    method text : string = textObject#text
    method! contentSize = textObject#size

    method! onResize r =
        super#onResize r;
        textObject#setPos Pos.{x=rect.x; y=rect.y};

    initializer
        textObject#setZIndex 1;
        textObject#setText start_text;
        renderObject#removeChild bgRect#obj;
        renderObject#addChild textObject#obj;
end
