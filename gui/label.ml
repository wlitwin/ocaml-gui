open Widget
open Text

class label ?(text="") app = 
    let start_text = text in
object(self)
    inherit TextBox.textBoxWidget app as super

    method! onKeyDown _ = 
        Mixins.Propagate

    initializer
        self#setText start_text;
        style#borderStyle#setStyle NoBorder;
end
