class loginWidget app = 
    let name = new LabeledTextBox.labeledTextBox app "Name" in
    let password = new LabeledTextBox.labeledTextBox app "Password" in
object(self)
    inherit Widget.basicWidget app as super

    initializer
        let vert = new Layout.verticalLayout in
        vert#addLayoutable (name :> Mixins.layoutable);
        vert#addLayoutable (password :> Mixins.layoutable);
        self#setLayout vert

    inherit Mixins.focusManager app [(name :> Mixins.handlesEvent);
                                     (password :> Mixins.handlesEvent)]
end
