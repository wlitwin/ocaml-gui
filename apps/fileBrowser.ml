let strs = [|
    "1";
    "2";
    "3";
    "4";
|]

let build_str () =
    let i1 = Random.int 4 in
    let i2 = Random.int 4 in
    strs.(i1) ^ strs.(i2)

let create_strings =
    let rec f acc n = 
        if n <= 0 then acc
        else f (build_str() :: acc) (n - 1)
    in
    f [] 200
;;

let readDir app path =
    try
        (*let dir = "." :: ".." :: Core.Sys.ls_dir path in
        dir*)
        create_strings
        |> List.sort ~compare:String.compare
        |> List.map ~f:(fun s -> new Label.label app ~text:s)
    with _ -> 
        [new Label.label app ~text:("Invalid path: " ^ path)]
;;

class ['a, 'b] fileBrowser app =
    let lblPath = new Label.label app ~text:"Path" in
    let txtPath = new TextBox.textBoxWidget app in
    let fileList = new ListBox.listBox app in
    let scroll = new Scroll.scrollArea app (fileList :> ('a, 'b) Widget.basicWidget) in
object(self)
    inherit ['a, 'b] Widget.basicWidget app as super

    method private setPath p =
        txtPath#setText p;
        self#updatePath;

    method private updatePath =
        let lbls = readDir app txtPath#text in
        fileList#setContents (List.map lbls (fun i -> 
            (i :> ('a, 'b) Layoutable.layoutable)));
        scroll#resize scroll#fullRect;

    initializer
        self#setPath "/";
        txtPath#events#addFn `KeyDown (object
            method call evt = match evt#arg with
            | `KeyDownArg Keys.Enter -> self#updatePath
            | _ -> ()
        end);
        let cl = fun w -> (w :> ('a, 'b) Layoutable.layoutable) in
        let lPath = cl lblPath in
        let tPath = cl txtPath in
        let lScroll = cl scroll in
        let rules = ConstraintLayout.(Constraint.([
            {input_item=lPath; 
             input_loc={top=centerTop lPath tPath; left=wLeft 10.; bottom=preferredH lblPath; right=preferredW lblPath}};
            {input_item=tPath; input_loc={top=wTop 10.; left=rightOf lPath 10.; right=wRight ~-.10.; bottom=preferredH tPath}};
            {input_item=lScroll; input_loc={top=bottomOf tPath 10.; left=wLeft 10.; right=wRight ~-.10.; bottom=wBottom ~-.10.}};
        ])) in
        let layout = new ConstraintLayout.constraintLayout rules in
        self#setLayout (layout :> ('a, 'b) Layout.layout)

    inherit ['a, 'b] Focusable.focusManager app [(txtPath :> ('a, 'b) HandlesEvent.handles_event);
                                                 (scroll :> ('a, 'b) HandlesEvent.handles_event)]
end
