open Label
open Scroll
open Layout
open Control

class listBox app strs =
    let ctrl = new control app in
    let vbox = new vboxLayout ctrl in
    let _ = 
        List.iter ~f:(fun str ->
            vbox#addControlWith (new label app str :> control) Preferred;
        ) strs;
        ctrl#setLayout (vbox :> layout);
    in
object(self)
    inherit scrollArea app ctrl as super
end

