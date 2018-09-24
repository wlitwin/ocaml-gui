open Def
open Label
open TextBox
open Layout
open Spacer

class textBoxWithLabel app labelName =
    let tbox = new textBox app in
    let label = new label app labelName in
object(self)
    inherit control app as super

    method! event (event : event) =
        let handled = super#sendEventToFilters event in
        if not handled then
            tbox#event event

    method tbox = tbox

    initializer begin
        let widget = new control app in
        let spacer = new control app in
        let hspacer = new spacer app {w=20.; h=10.} in
        let hbox = new hboxLayout (self :> control) in
        let vbox = new vboxLayout (self :> control) in
        widget#setLayout (hbox :> layout);
        self#setLayout (vbox :> layout);
        hbox#addControlWith (label :> control) Preferred;
        hbox#addControlWith (hspacer :> control) Preferred;
        hbox#addControl (tbox :> control);
        vbox#addControlWith (widget :> control) Preferred;
        vbox#addControl spacer;
        tbox#style#setBorderColor Rect.black;
        tbox#setFocus true;
    end
end

