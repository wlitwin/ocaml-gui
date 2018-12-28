type 'a layoutable = 'a constraint 'a = #Mixins.layoutable
let cl (l : _ layoutable) = (l :> Mixins.layoutable) 

let readDir app path =
    try
        let dir = "." :: ".." :: Core.Sys.ls_dir path in
        dir
        |> List.sort ~compare:String.compare
        |> List.map ~f:(fun s -> new Label.label app ~text:s)
    with _ -> 
        [new Label.label app ~text:("Invalid path: " ^ path)]
;;

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
        fileList#setContents (List.map lbls (fun i -> (i :> Mixins.layoutable)));
        scroll#resize scroll#fullRect

    initializer
        self#setPath "/";
        txtPath#addSnoop (function
            | Mixins.KeyDown Keys.Enter -> self#updatePath
            | _ -> ()
        );
        let open AnchorLayout in
        let top amt = Add [WTop; Const amt] in
        let left amt = Add [WLeft; Const amt] in
        let right amt = Add [WRight; Const ~-.amt] in
        let bottom amt = Add [WBottom; Const ~-.amt] in
        let rightOf i amt = Add [IRight i; Const amt] in
        let bottomOf i amt = Add [IBottom i; Const amt] in
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
            {item=lPath; loc={top=centerTop lPath tPath; left=left 10.; bottom=PreferredH lPath; right=PreferredW lPath}};
            {item=tPath; loc={top=top 10.; left=rightOf lPath 10.; right=right 10.; bottom=PreferredH tPath}};
            {item=lScroll; loc={top=bottomOf tPath 10.; left=left 10.; right=right 10.; bottom=bottom 10.}};
        ] in
        let layout = new AnchorLayout.anchorLayout rules in
        self#setLayout (layout :> Mixins.layout)

    inherit Mixins.focusManager app [(txtPath :> Mixins.handlesEvent);
                                     (scroll :> Mixins.handlesEvent)]
end
