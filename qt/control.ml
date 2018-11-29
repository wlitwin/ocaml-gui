open Rect
open Events
open Style

type window_handle = int

class control app =
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
            event_filters <- List.filter ~f:(fun (eid, _) -> eid <> id) event_filters

        method shortcut (keys : int list) = ()

        method style = style

        method keyPress (key : int) : unit =
            if not (List.mem ~equal:Poly.(=) key_state key) then
                key_state <- key :: key_state;
            self#shortcut key_state

        method keyRelease (key : int) : unit =
            key_state <- List.filter ~f:(fun k -> k <> key) key_state

        method isFocused = is_focused
        method setFocus focused = is_focused <- focused

        method resize size =
            self#setGeometry {rect with w=size.w; h=size.h}

        method move pos =
            self#setGeometry {rect with x=pos.x; y=pos.y}

        method sendEventToFilters (event : event) =
            List.exists ~f:(fun (_, f) -> f event) event_filters

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
            let bgColor = style#bgColor in
            Cairo.set_source_rgba cr bgColor.r bgColor.g bgColor.b bgColor.a;
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
            if Poly.(<>) style#borderColor Rect.none then begin
                let bc = style#borderColor in
                Cairo.set_source_rgba cr bc.r bc.g bc.b bc.a;
                Cairo.set_line_width cr style#borderSize;
                Cairo.rectangle cr rect.x rect.y rect.w rect.h;
                Cairo.stroke cr;
            end;
            Cairo.restore cr;

        method paint (cr : Cairo.context) = ()
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
