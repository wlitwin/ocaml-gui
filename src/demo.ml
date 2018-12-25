let mk_lbl app text =
    new Label.label ~text app
;;

type 'a layoutable = 'a constraint 'a = #Mixins.layoutable
type 'a handlesEvent = 'a constraint 'a = #Mixins.handlesEvent

let coerce (l : _ layoutable) = (l :> Mixins.layoutable) 
let coerceEvent (e : _ handlesEvent) = (e :> Mixins.handlesEvent)

class main app = 
    let tbox1 = new TextBox.textBoxWidget app in
    let tbox2 = new TextBox.textBoxWidget app in
    let tbox3 = new TextBox.textBoxWidget app in
    let tbox4 = new TextBox.textBoxWidget app in
    let lbl1  = new Label.label ~text:"Awesome!" app in
    let lbl2  = new Label.label ~text:"Another label with really really really really long text!" app in
    let scroller = new Scroll.scrollArea app (lbl2 :> Widget.basicWidget) in
    let dd    = DropDown.(new dropDown app (Simple ["Option 1"; "Option 2"; "Option 3"; "Option 4"])) in
    let img   = new Image.image app in
    let img2 = new Image.image app in
    let img_data = ByteArray.ByteArray.of_array [|
        0; 0; 255; 255; (**) 0; 255; 0; 255;
        255; 0; 0; 255; (**) 0; 255; 255; 255;
    |] in
    let _ = img#setImage img_data 2 2; img#setScale 10. in
    let _ = img2#setImage img_data 2 2; img2#setScale 15. in
    let img_data2 = ByteArray.ByteArray.of_array [|
        0; 0; 255; 255; (**) 0; 128; 255; 255;
        0; 255; 255; 255; (**) 0; 255; 0; 255;
    |] in
    let img3 = new Image.image app in
    let _ = img3#setImage img_data2 2 2; img3#setScale 50.; img3#setKeepAspectRatio false in
    let lbl3 = new Label.label ~text:"Cell (0,0)!" app in
    let lbl4 = new Label.label ~text:"Cell (0,1)!" app in
    let lbl5 = new Label.label ~text:"Cell (2,0)!" app in
    let lbl6 = new Label.label ~text:"Cell (2,1)!" app in
    let lbl7 = new Label.label ~text:"All three columns!" app in
    let _ = lbl7#style#setBGColor Color.{r=0.7; g=0.7; b=0.7; a=1.} in
    let flow_labels = [
        "Label 1";
        "Label Amazing";
        "Label Super Duper!";
        "Label 4";
        "Another Label";
        "Wrapped Yet?";
        "What about now?";
        "Some really long strings in here";
        "Word";
        "Cool";
    ] |> List.map ~f:(mk_lbl app) in
object(self)
    inherit Widget.basicWidget app as super

    val v_layout = new Layout.verticalLayout
    val h_layout_1 = new Layout.horizontalLayout
    val h_layout_2 = new Layout.horizontalLayout
    val h_layout_3 = new Layout.horizontalLayout
    val h_layout_4 = new Layout.horizontalLayout
    val flow_layout = new Layout.flowLayout
    val grid_layout = new Layout.gridLayout 3 3

    initializer
        h_layout_1#addLayoutable dd;
        h_layout_1#addLayoutable img;
        h_layout_2#addLayoutable tbox1;
        h_layout_2#addLayoutable tbox2;
        h_layout_3#addLayoutable tbox3;
        h_layout_3#addLayoutable tbox4;
        h_layout_4#addLayoutable lbl1;
        h_layout_4#addLayoutable scroller;
        let add_lbl = (Fn.compose flow_layout#addLayoutable coerce) in
        List.take flow_labels 5 |> List.iter ~f:add_lbl;
        flow_layout#addLayoutable img2;
        List.drop flow_labels 5 |> List.iter ~f:add_lbl;
        v_layout#addLayoutable h_layout_1;
        v_layout#addLayoutable h_layout_2;
        v_layout#addLayoutable h_layout_3;
        v_layout#addLayoutable h_layout_4;
        v_layout#addLayoutable grid_layout;
        v_layout#addLayoutable flow_layout;

        grid_layout#addToCell 0 0 (coerce lbl3);
        grid_layout#addToCell 0 1 (coerce lbl4);
        grid_layout#addToCell 1 0 ~rows:2 (coerce img3);
        img3#style#borderStyle#setSize 20.;
        img3#style#borderStyle#setColor Color.{r=0.4; g=0.4; b=0.4; a=1.};
        grid_layout#addToCell 2 0 (coerce lbl5);
        grid_layout#addToCell 2 1 (coerce lbl6);
        grid_layout#addToCell 0 2 ~cols:3 (coerce lbl7);

        self#setLayout (v_layout :> Mixins.layout)

    inherit Mixins.focusManager app [(coerceEvent dd); 
                                     (coerceEvent tbox1);
                                     (coerceEvent tbox2);
                                     (coerceEvent tbox4);
                                     (coerceEvent tbox3);
                                     (coerceEvent scroller)]

end

class main2 app =
    let coerce = List.map ~f:(fun item -> (item :> Widget.basicWidget)) in
    let list_labels = [
        "Item 1"; "Item 2"; "Item 3"; "Item 4";
    ] |> List.map ~f:(mk_lbl app) |> coerce in
object(self)
    inherit Widget.basicWidget app as super

    val lbox = new ListBox.listBox app list_labels
    val lbl = new Label.label ~text:"Hello!" app

    initializer
        (*style#setBorderStyle NoBorder; (* Top-level widget shouldn't have a border *)*)
        let vbox = new Layout.verticalLayout in
        (*let layout = new Layout.fullLayout (lbl :> Mixins.layoutable) in*)
        self#style#setBGColor Color.green;
        lbl#style#borderStyle#setColor Color.yellow;
        lbl#style#setBGColor (Color.red);
        vbox#addLayoutable (lbl :> Mixins.layoutable);
        vbox#addLayoutable (lbox :> Mixins.layoutable);
        self#setLayout (vbox :> Mixins.layout)
end
