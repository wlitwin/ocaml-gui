class loginWidget app = 
    let name = new LabeledTextBox.labeledTextBox app "Name" in
    let password = new LabeledTextBox.labeledTextBox app "Password" in
object(self)
    inherit Widget.basicWidget app as super

    initializer
        let open AnchorLayout in
        let lName = (name :> Mixins.layoutable) in
        let lPass = (password :> Mixins.layoutable) in
        let rules = [
            For (lName, [Loc (WLeft 10., WTop 10.); Anchors [Top; Left; Right]]);
            For (lPass, [Loc (iLeft lName 0., iBot lName 10.); Anchors [Top; Left; Right]]);
        ] in
        self#setLayout (new anchorLayout rules)

    inherit Mixins.focusManager app [(name :> Mixins.handlesEvent);
                                     (password :> Mixins.handlesEvent)]
end
