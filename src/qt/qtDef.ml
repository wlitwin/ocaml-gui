open Rect

type event = KeyPress of int
           | KeyRelease of int
           | Resize of size
           | Paint of Cairo.context
           | Layout of Cairo.context

let str_of_event = function
    | KeyPress _ -> "KeyPress"
    | KeyRelease _ -> "KeyRelease"
    | Resize _ -> "Resize"
    | Paint _ -> "Paint"
    | Layout _ -> "Layout"

class style = object(self)
    val mutable bgColor = Rect.white
    val mutable fgColor = Rect.black;
    val mutable borderColor = Rect.none;
    val mutable borderSize = 1.0;

    method bgColor = bgColor
    method fgColor = fgColor
    method borderColor = borderColor
    method borderSize = borderSize

    method setBGColor color = bgColor <- color
    method setFGColor color = fgColor <- color
    method setBorderColor color = borderColor <- color
    method setBorderSize size = borderSize <- size
end

class ['a] queue = object(self)
    val mutable head : 'a Dllist.node_t option = None
    val mutable tail : 'a Dllist.node_t option = None
    val mutable length = 0

    method length = length

    method enque (obj : 'a) =
        length <- length + 1;
        match tail with
        | None ->
            let node = Dllist.create obj in
            head <- Some node;
            tail <- Some node;
        | Some tl -> tail <- Some (Dllist.append tl obj)

    method front =
        match head with
        | Some head -> Some (Dllist.get head)
        | None -> None

    method head = head

    method deque =
        if length <= 0 then None
        else if length = 1 then begin
            length <- length - 1;
            match head with
            | Some hd ->
                head <- None;
                tail <- None;
                Some (Dllist.get hd)
            | None -> None
        end else begin
            length <- length - 1;
            match head with
            | Some hd ->
                head <- Some (Dllist.drop hd);
                Some (Dllist.get hd) 
            | None -> None
        end
end

type anchors = {
    tl : bool;
    tr : bool;
    bl : bool;
    br : bool;
}

let no_anchors = {
    tl = false; tr = false; bl = false; br = false;
}

type window_info = {
    anchors : anchors; (* Probably not right for this *)
    relative : bool;
}

let no_info = {
    anchors = no_anchors;
    relative = true;
}

type window_handle = int

class application = object(self)
    (* concept of z-order - Last window is top-most *)
    val windows : (window_handle, control * window_info) Hashtbl.t = Hashtbl.create 10
    (* Index (len - 1) == topmost control, list is in z-order *)
    val wnds : (control * window_info) DynArray.t = DynArray.create()
    val eventQueue : event queue = new queue
    val mutable viewport : size = {w=400.; h=400.}
    val mutable nextId = -1 
    val mutable ctrlDown = false
    val mutable shiftDown = false
    val mutable altDown = false
    val mutable superDown = false
    val mutable viewportSize = {w=0.; h=0.}

    method ctrlDown = ctrlDown
    method shiftDown = shiftDown
    method altDown = altDown
    method superDown = superDown

    method openWindow (rect : rect) (control : control) (window_info : window_info) : window_handle =
        nextId <- nextId + 1;
        Hashtbl.replace windows nextId (control, window_info);
        control#setId nextId;
        control#setGeometry rect;
        DynArray.add wnds (control, window_info);
        nextId

    method closeWindow (handle : window_handle) : unit =
        Hashtbl.remove windows handle;
        DynArray.filter (fun (ctrl, _) -> ctrl#id <> handle) wnds

    method private normalizeKey (key : int) =
        if shiftDown then
            try key |> Char.chr |> Char.lowercase_ascii |> Char.code
            with _ -> key
        else
            key

    method addEvent event =
        (* Work around GTK's weird key handling *)
        let event = match event with
                  | KeyPress key -> KeyPress (self#normalizeKey key)
                  | KeyRelease key -> KeyRelease (self#normalizeKey key)
                  | e -> e
        in
        eventQueue#enque event

    method printQueue =
        match eventQueue#head with
        | Some head ->
            Printf.printf "QUEUE LENGTH %d\n" eventQueue#length;
            Dllist.iter (fun it ->
                Printf.printf " == %s\n" (str_of_event it)
            ) head
        | None -> ()

    method viewportSize = viewportSize

    method resizeViewport (cr : Cairo.context) (size : size) =
        (* Go through and resize all the windows according to
         * their info *)
        (* TODO - come up with better types of resizing *)
        viewportSize <- size;
        Hashtbl.iter (fun _ (wnd, info : control * window_info) ->
            if info.relative then begin
                let r : rect = wnd#geom in
                let dx = size.w /. viewport.w
                and dy = size.h /. viewport.h in
                wnd#setGeometry {r with w=(r.w *. dx); h=(r.h *. dy)};
                wnd#relayout cr;
            end
        ) windows;
        viewport <- size

    method private checkControlKeys event =
        match event with
        | KeyPress 0xffe3   -> ctrlDown  <- true
        | KeyRelease 0xffe3 -> ctrlDown  <- false
        | KeyPress 0xffe9   -> altDown   <- true
        | KeyRelease 0xffe9 -> altDown   <- false
        | KeyPress 0xffeb   -> superDown <- true
        | KeyRelease 0xffeb -> superDown <- false
        | KeyPress 0xffe2   -> shiftDown <- true
        | KeyRelease 0xffe2 -> shiftDown <- false
        | _ -> ()

    method dispatch : unit =
        (*Printf.printf "DISPATCHING\n";
        self#printQueue;*)
        let rec loop () =
            begin match eventQueue#deque with
            | Some (Paint _ as event) ->
                DynArray.iter (fun (control, _) ->
                    control#event event
                ) wnds
            | Some event ->
                (* Track control characters *)
                self#checkControlKeys event;
                if DynArray.length wnds > 0 then begin
                    (*Printf.printf "Sending %s\n" (str_of_event event);*)
                    (DynArray.last wnds |> fst)#event event;
                end;
                loop()
            | None -> ()
            end
        in
        loop ()
end

and virtual layout parent = object(self)
    val mutable parent = parent

    method setParent (control : control) =
        parent <- control 

    method virtual addControl : control -> unit

    method virtual sizeHint : Cairo.context -> size

    method sizeHintNoCr =
        let module CI = Cairo.Image in
        let img = CI.create CI.ARGB32 1 1 in
        let cr = Cairo.create img in
        self#sizeHint cr

    method virtual children : control DynArray.t

    (* Can add parent back if need be *)
    method virtual layout : Cairo.context -> unit
end

and control app =
    object(self)
        val mutable key_state = []
        val mutable rect = empty_rect
        val mutable layout = None
        val mutable is_focused = false
        val mutable id : window_handle = -1
        val mutable event_filters : (int * (event -> bool)) list = []
        val mutable event_filter_id = 0
        val style = new style
        val app = app

        method id = id
        method setId new_id = id <- new_id

        method addEventFilter f : int =
            event_filter_id <- event_filter_id + 1;
            event_filters <- (event_filter_id, f) :: event_filters;
            event_filter_id

        method removeEventFilter id =
            event_filters <- List.filter (fun (eid, _) -> eid <> id) event_filters

        method shortcut (keys : int list) = ()

        method style = style

        method keyPress (key : int) : unit =
            if not (List.mem key key_state) then
                key_state <- key :: key_state;
            self#shortcut key_state

        method keyRelease (key : int) : unit =
            key_state <- List.filter (fun k -> k <> key) key_state

        method isFocused = is_focused
        method setFocus focused = is_focused <- focused

        method resize size =
            self#setGeometry {rect with w=size.w; h=size.h}

        method move pos =
            self#setGeometry {rect with x=pos.x; y=pos.y}

        method sendEventToFilters (event : event) =
            List.exists (fun (_, f) -> f event) event_filters

        method event (event : event) =
            let handled = self#sendEventToFilters event in
            if not handled then
                match event with
                | KeyPress key -> self#keyPress key
                | KeyRelease key -> self#keyRelease key
                | Resize size -> self#resize size
                | Paint cr -> self#beginPaint cr
                | Layout cr -> self#relayout cr

        method geom : rect = rect

        method setLayout (new_layout : layout) =
            new_layout#setParent (self :> control);
            layout <- Some new_layout

        method relayout (cr : Cairo.context) : unit =
            match layout with
            | Some layout -> layout#layout cr
            | None -> ()

        method relayoutNoCr : unit =
            let module CI = Cairo.Image in
            let img = CI.create CI.ARGB32 1 1 in
            let cr = Cairo.create img in
            match layout with
            | Some layout -> 
                layout#layout cr
            | None -> self#relayout cr

        method sizeHint (cr : Cairo.context) : size = 
            match layout with
            | Some l -> l#sizeHint cr
            | None -> {w=0.; h=0.;}

        method sizeHintNoCr : size =
            let module CI = Cairo.Image in
            let img = CI.create CI.ARGB32 1 1 in
            let cr = Cairo.create img in
            self#sizeHint cr

        method size : size = size_of_rect rect

        method pos : pos = pos_of_rect rect

        method setGeometry (new_rect : rect) : unit =
            rect <- new_rect

        method clearShortcut =
            key_state <- []

        method beginPaint (cr : Cairo.context) =
            (*Printf.printf "STARTING PAINT FOR %s\n" (string_of_int id);*)
            Cairo.save cr;
            Cairo.rectangle cr rect.x rect.y rect.w rect.h;
            Cairo.clip cr;
            Cairo.move_to cr rect.x rect.y;
            Cairo.set_source_rgba cr 1. 1. 1. 1.;
            Cairo.rectangle cr rect.x rect.y rect.w rect.h;
            Cairo.fill cr;
            self#paint cr;
            begin match layout with
            | Some l -> 
                DynArray.iter (fun it ->
                    it#beginPaint cr
                ) l#children
            | None -> ()
            end;
            (*Printf.printf "PAINTIN TEXT %f %f %f %f\n" rect.x rect.y rect.w rect.h;*)
            if self#style#borderColor <> Rect.none then begin
                let bc = self#style#borderColor in
                Cairo.set_source_rgba cr bc.r bc.g bc.b bc.a;
                Cairo.set_line_width cr self#style#borderSize;
                Cairo.rectangle cr rect.x rect.y rect.w rect.h;
                Cairo.stroke cr;
            end;
            Cairo.restore cr;

        method paint (cr : Cairo.context) = ()
    end
