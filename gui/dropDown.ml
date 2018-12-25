open Widget

type choices = Simple of string list

type popup_result = Selected of int
                  | Canceled

let coerce (a : #basicWidget) : #Mixins.handlesEvent = (a :> Mixins.handlesEvent)

class dropDownPopup app choices callback =
    let choices = Array.map choices (fun text -> new Label.label ~text app) in
object(self)
    inherit basicWidget app as super

    val vbox = new Layout.verticalLayout

    method private selected =
        Option.value_exn
            (Array.findi 
                choices 
                (fun _ a -> phys_equal (a :> Mixins.handlesEvent) self#focused))
        |> fst

    method onKeyDown k =
        begin match k with
        | Keys.Enter
        | Keys.Space -> callback (Selected self#selected)
        | Keys.Esc -> callback Canceled
        | Keys.J -> self#updateFocus Mixins.Forward;
        | Keys.K -> self#updateFocus Mixins.Backward;
        | _ -> ()
        end;
        Mixins.Propagate

    initializer
        Array.iter choices
            (fun ch -> vbox#addLayoutable ch);
        self#setLayout (vbox :> Mixins.layout);
        style#setBGColor Color.orange;

    inherit Mixins.focusManager app 
        (Array.to_list (Array.map choices coerce))
end

class dropDown app choices = 
    let choices = match choices with
                | Simple lst -> Array.of_list lst
    in
object(self)
    inherit basicWidget app as super

    val mutable index = 0
    val mutable popUp = None

    method selected = choices.(index)

    method private popUpCallback res =
        begin match res with
        | Selected i -> index <- i
        | Canceled -> ()
        end;
        popUp <- None;
        self#invalidate

    method private createPopup =
        let ddp = new dropDownPopup app choices self#popUpCallback in
        let dd_rect = (*{rect with y=rect.y+.rect.h} in*) rect in
        Stdio.printf "DD RECT %f %f %f %f\n" dd_rect.x dd_rect.y dd_rect.w dd_rect.h;
        ddp#resize dd_rect;
        popUp <- Some ddp

    method! postEvent evt =
        match popUp with
        | Some p -> p#postEvent evt
        | None -> super#postEvent evt

    method onKeyDown k =
        begin match k with
        | Keys.Space when Option.is_none popUp -> 
                self#createPopup;
                self#invalidate
        | _ -> ()
        end;
        Mixins.Propagate

    method! contentSize =
        Text.measure_text style#fontInfo self#selected

    method private drawDropDown cr =
        match popUp with
        | Some popUp -> popUp#postEvent (Mixins.Paint cr) |> ignore
        | None -> ()

    method! paint cr =
        Text.draw_text cr rect style self#selected;

    method! onDraw cr =
        super#onDraw cr;
        self#drawDropDown cr;

end
