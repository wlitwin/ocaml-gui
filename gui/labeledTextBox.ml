class labeledTextBox app labelText =
object(self)
    inherit Widget.basicWidget app as super

    val textBox = new TextBox.textBoxWidget app
    val label = new Label.label app ~text:labelText

    method! onKeyDown key =
        textBox#onKeyDown key

    initializer
        let layout = new Layout.horizontalLayout in
        layout#addLayoutable (label :> Mixins.layoutable);
        layout#addLayoutable (textBox :> Mixins.layoutable);
        self#setLayout (layout :> Mixins.layout)
end
