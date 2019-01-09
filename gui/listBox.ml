class listBox ?(items=[]) app =
object(self)
    inherit Widget.basicWidget app as super
    (* inherit Mixins.scroller super as scroll *)
    (* inherit Mixins.layouter scroll as layout *)
    val v_layout = new VerticalLayout.verticalLayout

    method setContents (lst : Layoutable.layoutable list) : unit =
        v_layout#clear;
        let coerce (item : #Layoutable.layoutable) = (item :> Layoutable.layoutable) in
        List.iter lst (Fn.compose v_layout#addLayoutable coerce);
        self#invalidate;

    initializer
        v_layout#setDistributeEvenly false;
        self#setContents items;
        self#setLayout (v_layout :> Layout.layout)
end
