class listBox ?(items=[]) app =
object(self)
    inherit Widget.basicWidget app as super
    (* inherit Mixins.scroller super as scroll *)
    (* inherit Mixins.layouter scroll as layout *)
    val v_layout = new Layout.verticalLayout

    method setContents (lst : Mixins.layoutable list) : unit =
        v_layout#clear;
        let coerce (item : #Mixins.layoutable) = (item :> Mixins.layoutable) in
        List.iter lst (Fn.compose v_layout#addLayoutable coerce);
        self#invalidate;

    initializer
        v_layout#setDistributeEvenly false;
        self#setContents items;
        self#setLayout (v_layout :> Mixins.layout)
end
