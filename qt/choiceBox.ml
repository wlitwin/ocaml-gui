open TextBox
open Layout
open Label
open Scroll
open Control

let cbIdxToKey, cbKeyToIdx, cbStrings = 
    let open Util in
    let keys = [
        0x75(*u*); 0x69(*i*); 0x6f(*o*); 0x70(*p*); 0x68(*h*);
        0x6a(*j*); 0x6b(*k*); 0x6c(*l*); 0x6e(*n*); 0x6d(*m*);
    ] in
    let assoc = keys |> List.mapi ~f:Util.pair in
    let assoc_flip = List.map ~f:(fun (a, b) -> (b, a)) assoc in
    Util.hash_of_list assoc,
    Util.hash_of_list assoc_flip,
    keys |> Array.of_list |> Array.map ~f:(String.make 1 % Char.of_int_exn)
;;

(* TODO - display something when there's no choices *)
class choiceBox app choices ?title vboxType =
    let tbox = new textBox app in
    let ctrl = new control app in
    let vbox = new vboxLayout ctrl in
    let scroll = 
        List.iter ~f:(fun str ->
            vbox#addControlWith (new label app str :> control) vboxType;
        ) choices;
        ctrl#setLayout (vbox :> layout);
        new scrollArea app ctrl
    in
object(self)
    inherit control app as super

    val text = tbox
    val choiceCtrl = ctrl
    val scroll = scroll
    val mutable title = title
    val mutable choices = choices
    val mutable choiceCallback : (int * string) -> unit = (fun _ -> ())
    val mutable cancelCallback : unit -> unit = (fun _ -> ())
    val mutable currentChoices : (int * string) list = []
    val mutable currentChoicesLen : int = 0
    val mutable initialSizeHint = Rect.zero_size
    val mutable maxItemsSizeHint = 10 (* Easier if max size existed *)

    method reset = tbox#setText ""

    method setAcceptCallback f = choiceCallback <- f
    method setCancelCallback f = cancelCallback <- f

    method setChoices new_choices =
        choices <- new_choices;
        self#reset;
        self#updateChoices;
        let sz = (layout |> Util.some)#sizeHintNoCr in
        Stdio.printf "\n\n SIZE HINT %f %f \n" sz.w sz.h;
        initialSizeHint <- sz

        (*
    method setTitle new_title =
        title <- Some new_title;
        initialSizeHint <- vbox#sizeHintNoCr;
        *)

    method private updateChoices =
        let terms = String.split ~on:' ' tbox#text in
        vbox#removeAllChildren;
        choices
        |> List.mapi ~f:(fun i item -> (i, item))
        |> List.filter ~f:(fun (_, item) -> Util.strContainsAllCI item terms)
        |> Util.tee (fun lst ->
                currentChoices <- lst;
                currentChoicesLen <- List.length lst
        )
        |> List.iter ~f:(fun (_, item) -> vbox#addControlWith (new label app item :> control) vboxType)

    method private keyToIndex = Hashtbl.find_exn cbKeyToIdx
    method private indexToKey = Hashtbl.find_exn cbIdxToKey
    method private isInKeyMap key = Hashtbl.mem cbKeyToIdx key
    method private indexToStr idx = cbStrings.(idx)

    method keyPress (key : int) =
        match key with
        | 0xff0d -> (* enter pressed *)
                (* If there's only one choice, fire the callback *)
                begin match currentChoices with
                | [idx, str] -> choiceCallback (idx, str)
                | _ -> ()
                end
        | 0xff1b -> (* escape hit *) cancelCallback()
        | key when (app#ctrlDown && self#isInKeyMap key) ->
                let idx = self#keyToIndex key in
                if idx < currentChoicesLen then
                    choiceCallback (List.nth_exn currentChoices idx)
        | key ->
                tbox#keyPress key;
                (* Filter the list by the text box *)
                vbox#removeAllChildren;
                self#updateChoices;

    (* Paint a number into the corner of each choice
     * To allow quick-selection
     *)
    method! beginPaint cr =
        super#beginPaint cr;
        (* Paint numbers in the top corner if CTRL is down *)
        let len = currentChoicesLen in
        if app#ctrlDown && len > 0 then begin
            let vsz = vbox#sizeHint cr in
            let divH = vsz.h /. (Float.of_int len) in
            let r = ctrl#geom in
            Cairo.set_font_size cr (divH *. 0.9);
            Cairo.select_font_face cr "Ubuntu mono";
            let fe = Cairo.font_extents cr in
            let w = fe.Cairo.max_x_advance *. 1.1 in
            let h = Float.(min (fe.Cairo.ascent +. fe.Cairo.descent) divH) in
            let ox = r.x +. scroll#geom.x
            and oy = r.y +. scroll#geom.y in
            let maxIdx = (min len (Array.length cbStrings)) - 1 in
            for i=0 to maxIdx do
                let oy = oy +. (divH *. Float.of_int i) in
                Cairo.set_source_rgba cr 0. 0. 0. 1.;
                Cairo.rectangle cr (ox +. r.w -. w) oy w h;
                Cairo.fill cr;
                Cairo.set_source_rgba cr 1. 1. 1. 1.;
                Cairo.move_to cr (ox +. r.w -. w +. (w -. fe.Cairo.max_x_advance)*.0.5) (oy +. fe.Cairo.ascent);
                Cairo.show_text cr (self#indexToStr i);
            done
        end

    method! sizeHint cr =
        initialSizeHint

    initializer begin
        (* Constructor code here *)
        tbox#setText "";
        let vbox = new vboxLayout (self :> control) in
        self#style#setBorderColor Rect.black;
        self#style#setBorderSize 4.0;
        ctrl#style#setBorderColor Rect.black;
        begin match title with
        | Some str ->
            let title = new label app str in
            title#style#setBorderColor Rect.black;
            title#setWeight Cairo.Bold;
            vbox#addControlWith (title :> control) Preferred;
        | None -> ()
        end;
        vbox#addControlWith (text :> control) Preferred;
        vbox#addControlWith (scroll :> control) Equal;
        self#setLayout (vbox :> layout);
        tbox#setFocus true;
        let module CI = Cairo.Image in
        let img = CI.create CI.ARGB32 1 1 in
        let cr = Cairo.create img in
        initialSizeHint <- vbox#sizeHint cr;
        self#updateChoices;
    end
end

