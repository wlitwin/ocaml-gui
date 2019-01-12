open Widget

class ['a, 'b] label ?(text="") app = 
    let start_text = text in
object(self)
    inherit ['a, 'b] TextBox.textBoxWidget app as super

    method! onKeyDown _ = 
        ()

    initializer
        showCursor <- false;
        self#setText start_text;
        style#borderStyle#setStyle NoBorder;
        style#setBGColor Color.gray;
end
