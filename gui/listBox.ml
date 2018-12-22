class listBox app (items : Widget.basicWidget list) =
object(self)
    inherit Widget.basicWidget app as super
    (* inherit Mixins.scroller super as scroll *)
    (* inherit Mixins.layouter scroll as layout *)

    val layout = new Layout.verticalLayout

    method resize r =
        super#resize r;
        layout#resize r

    method onDraw cr =
        List.iter items (fun item -> item#onDraw cr);

    initializer
        let coerce (item : 'a) = (item :> Mixins.layoutable) in
        List.iter items (Fn.compose layout#addLayoutable coerce)
end
