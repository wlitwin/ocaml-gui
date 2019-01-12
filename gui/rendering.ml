type draw_type = Stroke
               | Fill

type text_data = {
    font : Font.t;
    text : string;
}

type shape_type = Rectangle
                | Text of text_data

type primitive = {
    draw_type : draw_type;
    shape_type : shape_type;
    color : Color.t;
}

type layer = {
    z_index : int;
    primitives : primitive list;
}
