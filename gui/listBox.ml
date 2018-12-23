class listBox app (items : Widget.basicWidget list) =
object(self)
    inherit Widget.basicWidget app as super
    (* inherit Mixins.scroller super as scroll *)
    (* inherit Mixins.layouter scroll as layout *)

    initializer
        let coerce (item : 'a) = (item :> Mixins.layoutable) in
        let v_layout = new Layout.verticalLayout in
        List.iter items (Fn.compose v_layout#addLayoutable coerce);
        self#setLayout v_layout
end
