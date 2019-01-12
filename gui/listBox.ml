class ['a, 'b] listBox ?(items=[]) app =
object(self)
    inherit ['a, 'b] Widget.basicWidget app as super
    (* inherit Mixins.scroller super as scroll *)
    (* inherit Mixins.layouter scroll as layout *)
    val v_layout = new VerticalLayout.verticalLayout

    method setContents (lst : ('a, 'b) Layoutable.layoutable list) : unit =
        v_layout#clear;
        List.iter lst v_layout#addLayoutable;

    initializer
        v_layout#setDistributeEvenly false;
        self#setContents items;
        self#setLayout (v_layout :> ('a, 'b) Layout.layout)
end
