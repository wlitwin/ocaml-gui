type 'a layoutable = 'a constraint 'a = #Layoutable.layoutable
let cl (l : _ layoutable) = (l :> Layoutable.layoutable) 

let strs = [|
    "Hello";
    "World";
    "Cool";
    "Awesome";
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
    f [] 1000
;;

let readDir app path =
    try
        let dir = "." :: ".." :: Core.Sys.ls_dir path in
        dir
        (*create_strings*)
        |> List.sort ~compare:String.compare
        |> List.map ~f:(fun s -> new Label.label app ~text:s)
    with _ -> 
        [new Label.label app ~text:("Invalid path: " ^ path)]
;;

(*
class fileBrowser app =
    let lblPath = new Label.label app ~text:"Path" in
    let txtPath = new TextBox.textBoxWidget app in
    let fileList = new ListBox.listBox app in
    let scroll = new Scroll.scrollArea app (fileList :> Widget.basicWidget) in
object(self)
    inherit Widget.basicWidget app as super

    method private setPath p =
        txtPath#setText p;
        self#updatePath;

    method private updatePath =
        let lbls = readDir app txtPath#text in
        fileList#setContents (List.map lbls (fun i -> 
            i#setShouldClip false;
            (i :> Layoutable.layoutable)));
        scroll#resize scroll#fullRect

    initializer
        self#setPath "/";
        HandlesKeyboard.(HandlesEvent.repeat txtPath KeyDown (function
            | KeyDownArg Keys.Enter -> self#updatePath
            | _ -> ()
        ))#attach;
        let open ConstraintLayout in
        let open ConstraintLayout.Constraint in
        let centerTop toCenter ref = FunDep ([(ref, DTop); (ref, DBottom)], fun fields ->
            let lookup f = Hashtbl.Poly.find_exn fields f in
            let top = lookup (ref, DTop)
            and bot = lookup (ref, DBottom) in
            let prefH = toCenter#preferredSize.Size.h in
            top +. ((bot -. top) -. prefH)*.0.5
        ) in
        let lPath = cl lblPath in
        let tPath = cl txtPath in
        let lScroll = cl scroll in
        let rules = [
            {item=lPath; loc={top=centerTop lPath tPath; left=wLeft 10.; bottom=PreferredH lPath; right=PreferredW lPath}};
            {item=tPath; loc={top=wTop 10.; left=rightOf lPath 10.; right=wRight ~-.10.; bottom=PreferredH tPath}};
            {item=lScroll; loc={top=bottomOf tPath 10.; left=wLeft 10.; right=wRight ~-.10.; bottom=wBottom ~-.10.}};
        ] in
        let layout = new constraintLayout rules in
        self#setLayout (layout :> Layout.layout)

    inherit Focusable.focusManager app [(txtPath :> HandlesEvent.handlesEvent);
                                        (scroll :> HandlesEvent.handlesEvent)]
end
*)
