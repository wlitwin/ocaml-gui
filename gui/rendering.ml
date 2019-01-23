(* TODO figure out how to use JaneStreet's Incremental library for this *)

module R = Test_rtree
module Graphics = Platform.Windowing.Graphics
module HP = Hashtbl.Poly

type draw_type = Stroke
               | Fill

type text_data = {
    font : Font.t;
    text : string;
    size : Size.t;
}

type shape_type = Rectangle of Size.t
                | Text of text_data

type primitive = {
    pos : Pos.t;
    draw_type : draw_type;
    shape_type : shape_type;
    color : Color.t;
}

type content = Translate of Pos.t
             | Group of (int * primitive list) list
             | Primitive of primitive

type node = {
    parent : node option;
    content : content;
    children : node list;
}

let mk_rect (rect, color, draw_type : Rect.t * Color.t * draw_type) = {
    pos = Pos.{x=rect.x; y=rect.y};
    draw_type;
    shape_type = Rectangle Size.{w=rect.w; h=rect.h};
    color;
}

let fill_rect (rect : Rect.t) color =
    mk_rect(rect, color, Fill)
;;

let stroke_rect (rect : Rect.t) color =
    mk_rect(rect, color, Stroke)
;;

let measure_text (font, text) =
    let metrics = Platform.Windowing.Graphics.measure_text_no_context(font, text) in
    Size.{w=metrics.width; h=metrics.ascent+.metrics.descent}
;;

let fill_text text font color pos =
    {
        pos;
        draw_type = Fill;
        shape_type = Text {
            font;
            text;
            size=measure_text(font, text);
        };
        color;
    }
;;

type rectangle_list = {
    draw_type : draw_type;
    color : Color.t;
    rects : Rect.t list;
}

type text_list = {
    draw_type : draw_type;
    font : Font.t;
    color : Color.t;
    text : (Pos.t * string) list;
}


type obj = < >

class nodeObject renderer = object(self)
    val mutable content = Group [0, []]
    val mutable children : nodeObject DynArray.t = DynArray.create ~capacity:1 ()
    val renderer = renderer
    val mutable z_index = 0
    val mutable parent : nodeObject option = None
    val mutable identifier : string = ""

    method rect = Rect.empty

    method id = identifier
    method setId id = identifier <- id

    method zIndex = z_index
    method setZIndex z = z_index <- z

    method fullZIndex =
        let rec loop idx = function
            | None -> idx
            | Some p -> loop (idx + p#zIndex) p#parent
        in
        loop z_index parent

    method parent = parent
    method setParent p = parent <- p

    method content = content
    method setContent c : unit = 
        let old_rect = self#rect in
        content <- c;
        renderer#removeObject (self :> nodeObject);
        renderer#addObject (self :> nodeObject);
        renderer#refreshChanged ((self :> nodeObject), old_rect);

    method toString = identifier

    method attach (obj : nodeObject) : unit  =
        renderer#groupUpdates (fun _ ->
            match obj#parent with
            | None -> 
                obj#setParent (Some (self :> nodeObject));
                DynArray.add children obj;
                renderer#addObject obj;
                renderer#refreshChanged ((obj :> nodeObject), obj#rect)
            | Some p when phys_equal p (self :> nodeObject) -> ()
            | Some p -> 
                p#detach obj;
                obj#setParent (Some (self :> nodeObject));
                renderer#addObject obj;
        );

    method draw (cr : Platform.Windowing.Graphics.context) : unit =
        DynArray.iter (fun child ->
            child#draw cr
        ) children

    method detach (obj : nodeObject) : unit =
        obj#setParent None;
        DynArray.filter (fun o -> not (phys_equal obj o)) children;
        renderer#refreshChanged ((obj :> nodeObject), obj#rect);
        renderer#removeObject obj;
end

module SpatialIndex = struct
    module HP = Hashtbl.Poly

    type t = {
        rtree : nodeObject Rtree.t;
        object_table : (nodeObject, Rect.t) HP.t;
    }

    let create () = {
        rtree = Rtree.create();
        object_table = HP.create();
    }

    let add (t, obj, rect) =
        if not (HP.mem t.object_table obj) then (
            HP.set t.object_table obj rect;
            Rtree.insert (t.rtree, rect, obj);
        )

    let search (t, rect, results) =
        Rtree.search (t.rtree, rect, results)

    let remove (t, obj) =
        if HP.mem t.object_table obj then (
            let rect = HP.find_exn t.object_table obj in
            HP.remove t.object_table obj;
            Rtree.delete (t.rtree, rect, (fun o -> phys_equal o obj))
        )
end

module Drawable = struct
    type prim = {
        mutable bounds : Rect.t;
        mutable color : Color.t;
        mutable fill_type : draw_type;
    }

    type text_rec = {
        mutable text : string;
        mutable font : Font.t;
        prim : prim;
    }

    type view_rec = {
        mutable outer_bounds : Rect.t;
        mutable inner_bounds : Rect.t;
        index : SpatialIndex.t;
        children : any_object DynArray.t;
    }

    and any_object = Ex : 'a t -> any_object

    and _ t =
        | Text : text_rec -> text_rec t
        | Rect : prim -> prim t
        | Viewport : view_rec -> view_rec t

    module Prim = struct
        let create ?(bounds=Rect.empty) ?(color=Color.black) ?(fill_type=Fill) () = {
            bounds; color; fill_type
        }

        let set_pos (prim, pos : prim * Pos.t) =
            prim.bounds <- {prim.bounds with x=pos.x; y=pos.y}
    end

    module Viewport = struct
        let create ?(outer_bounds=Rect.empty) ?(inner_bounds=Rect.empty) () = Viewport {
            outer_bounds;    
            inner_bounds;
            index = SpatialIndex.create();
            children = DynArray.create();
        }

        let set_inner_bounds : view_rec t * Rect.t -> unit = function
            | Viewport v, rect -> v.inner_bounds <- rect

        let set_outer_bounds : view_rec t * Rect.t -> unit = function
            | Viewport v, rect -> v.inner_bounds <- rect

        let add_child : view_rec t * 'a t -> unit = function
            | Viewport v, child -> DynArray.add v.children (Ex child)

        let iter : (view_rec t * (any_object -> unit)) -> unit = function
            | Viewport v, f -> DynArray.iter f v.children 
    end

    module Text = struct
        let create ?(text="") ?(font=Font.default_font) () = Text {
            text;
            font;
            prim=Prim.create();
        }

        let set_text : text_rec t * string -> unit = function
            | Text t, str -> t.text <- str

        let set_font : text_rec t * Font.t -> unit = function
            | Text t, font -> t.font <- font

        let set_pos : text_rec t * Pos.t -> unit = function
            | Text t, pos -> Prim.set_pos (t.prim, pos)
    end

    module Rect = struct
        let create ?(bounds=Rect.empty) ?(color=Color.red) ?(fill_type=Fill) () =
            Rect (Prim.create ~bounds ~color ~fill_type ())

        let set_rect : prim t * Rect.t -> unit = function
            | Rect r, rect -> r.bounds <- rect

        let set_pos : prim t * Pos.t -> unit = function
            | Rect r, pos -> Prim.set_pos (r, pos)
    end

    let test () =
        let v = Viewport.create() in
        let t = Text.create() in
        let r = Rect.create() in
        Viewport.add_child (v, r);
        Viewport.add_child (v, t);
        Viewport.iter (v, function
            | Ex (Text _) -> Stdio.printf "TEXT\n"
            | Ex (Rect _) -> Stdio.printf "RECT\n"
            | Ex (Viewport _) -> Stdio.printf "VIEWPORT\n")

end

module Layer = struct

let calc_z obj =
    let rec loop (obj, z_index) =
        match obj#parent with
        | None -> z_index
        | Some p -> loop (p, z_index + p#zIndex)
    in
    loop (obj, 0)
;;

let calc_rect (obj, rect) =
    let rec loop (obj, rect) =
        match obj#parent with
        | None -> rect
        | Some p ->
            match p#content with
            | Translate pos -> loop (p, Rect.({rect with x=rect.x+.pos.x; y=rect.y+.pos.y}))
            | _ -> loop (p, rect)
    in
    loop (obj, rect)
;;

type layer = {
    z_index : int;
    prims : nodeObject DynArray.t;
}

type render_state = {
    layers : layer DynArray.t;
}

end

class groupObject renderer = object(self)
    inherit nodeObject renderer
end

class primObject renderer = object(self)
    inherit nodeObject renderer

    method private prim =
        match self#content with
        | Primitive prim -> prim
        | _ -> failwith "Invalid prim"

    method setColor color =
        self#setContent (Primitive {self#prim with color})

    method setMode draw_type = 
        self#setContent (Primitive {self#prim with draw_type})

    method pos : Pos.t = self#prim.pos

    method setPos pos = 
        self#setContent (Primitive {self#prim with pos})
end

class textObject renderer = object(self)
    inherit primObject renderer

    method private text_info =
        match self#prim.shape_type with
        | Text ti -> ti
        | _ -> failwith "Invalid text object"

    method text = self#text_info.text
    method font = self#text_info.font

    method! toString = "T[" ^ identifier ^ "][" ^ self#text ^ "]"

    method fontExtents : Font.font_extents =
        renderer#fontExtents self#font

    method size = self#text_info.size

    method! rect =
        let prim = self#prim in
        let ascent = self#fontExtents.ascent in
        let size = self#text_info.size in
        Rect.{x=prim.pos.x; y=prim.pos.y-.ascent; w=size.w; h=size.h}

    method setText text =
        let ti = self#text_info in
        let size = measure_text(ti.font, text) in
        self#setContent (Primitive {self#prim with shape_type=Text {ti with text; size}});

    method setFont font =
        self#setContent (Primitive {self#prim with shape_type=Text {self#text_info with font}})

    method! draw cr =
        let rect = Layer.calc_rect ((self :> nodeObject), self#rect) in
        let ti = self#text_info in
        let prim = self#prim in
        Graphics.set_color cr prim.color;
        Graphics.draw_text cr ti.font rect ti.text

    initializer
        content <- (Primitive {
            pos=Pos.zero;
            draw_type=Fill;
            shape_type=Text {text=""; font=Font.default_font; size=Size.zero};
            color=Color.black;
        })
end

class rectObject renderer = object(self)
    inherit primObject renderer

    method setSize (size : Size.t) =
        self#setContent (Primitive {self#prim with
            shape_type=Rectangle size
        })

    method size =
        match self#prim.shape_type with
        | Rectangle size -> size
        | _ -> failwith "Should be rectangle"

    method! rect =
        let prim = self#prim in
        let size = self#size in
        Rect.{x=prim.pos.x; y=prim.pos.y; w=size.w; h=size.h}

    method setRect (r : Rect.t) =
        self#setContent (Primitive {self#prim with 
            pos=Pos.{x=r.x; y=r.y}; 
            shape_type=Rectangle Size.{w=r.w; h=r.h}
        })

    method! toString = 
        let color = self#prim.color in
        Printf.sprintf "R[%s][%.2f %.2f %.2f]" identifier color.r color.g color.b

    method! draw cr =
        let rect = Layer.calc_rect ((self :> nodeObject), self#rect) in
        let prim = self#prim in
        Graphics.set_color cr prim.color;
        Graphics.rectangle cr rect;
        match prim.draw_type with
        | Fill -> Graphics.fill cr
        | Stroke -> Graphics.stroke cr

    initializer
        content <- (Primitive {
            pos=Pos.zero;
            draw_type=Fill;
            shape_type=Rectangle Size.{w=100.; h=100.};
            color=Color.red;
        })
end

class translateObject renderer = object(self)
    inherit nodeObject renderer

    method setTranslation (x, y : float * float) : unit =
        (*
        self#setContent (Translate Pos.{x; y});
        renderer#refreshSingle (self :> nodeObject)
        *)
        ()

    method! attach obj =
        ()

    method! detach obj =
        ()

    method! draw (cr : Platform.Windowing.Graphics.context) : unit =
        ()
end

module Dirty = struct
type t = NotDirty
       | SingleUpdate of nodeObject * Rect.t
       | RenderAll

let is_dirty = function
    | NotDirty -> false
    | _ -> true
end

type update_type = 
    | Refresh of Rect.t
    | Changed of Rect.t * Rect.t
    | FullRefresh

class renderer = object(self)
    val mutable root = new nodeObject (object 
        method groupUpdates _ = ()
        method refreshChanged _ = ()
        method refreshFull _ = ()
        method addObject _ = ()
        method removeObject _ = ()
    end)
    val mutable requestDraw : unit -> unit = fun _ -> ()
    val mutable updates : update_type DynArray.t = DynArray.create ~capacity:10 ()

    val index : SpatialIndex.t = SpatialIndex.create()
    val searchResults : nodeObject DynArray.t = DynArray.create ~capacity:100 ()

    method root = root
    method setRoot r = 
        root <- r;

    method fontExtents font : Font.font_extents =
        Graphics.font_extents_no_context font

    method createTextObject = new textObject self
    method createRectObject = new rectObject self
    method createGroupObject = new groupObject self
    method createTranslateObject = new translateObject self

    val mutable drawEnabled = true
    val mutable shouldUpdate = true

    method addObject (obj : nodeObject) : unit =
        let rect = obj#rect in
        if not (Rect.is_empty rect) && Option.is_some obj#parent then (
            let rect = Layer.calc_rect (obj, obj#rect) in
            SpatialIndex.add (index, obj, rect);
        )

    method removeObject (obj : nodeObject) : unit =
        SpatialIndex.remove (index, obj);

    method ignoreUpdates (f : unit -> unit) : unit =
        let before = shouldUpdate in
        shouldUpdate <- false;
        f();
        shouldUpdate <- before;

    method groupUpdates f =
        let before = drawEnabled in
        drawEnabled <- false;
        f();
        drawEnabled <- before;
        self#requestDraw

    val mutable size = Size.zero
    method setSize s = size <- s

    method refreshFull =
        if shouldUpdate then (
            DynArray.clear updates;
            DynArray.add updates FullRefresh;
            self#requestDraw
        )

    method refreshChanged (obj, old_rect) =
        if shouldUpdate then (
            (*Stdio.printf "ADDING REFRESH CHANGED %s\n%!" obj#toString;*)
            let rect = Layer.calc_rect (obj, obj#rect)
            and old_rect = Layer.calc_rect (obj, old_rect) in
            DynArray.add updates (Changed (rect, old_rect));
        )

    method refreshSingle (obj : nodeObject) =
        if shouldUpdate then (
            (*Stdio.printf "ADDING REFRESH SINGLE %s\n%!" obj#toString;*)
            let rect = Layer.calc_rect (obj, obj#rect) in
            DynArray.add updates (Refresh rect)
        )

    method setRequestDraw f =
        requestDraw <- f

    method private requestDraw =
        if drawEnabled then
            requestDraw()

    val stat_font = Font.{
        size = 20.;
        font = "Ubuntu mono";
        weight = Bold;
    }

    method private drawStats (cr, t1, t2) =
        Graphics.set_color cr Color.gray;
        Graphics.rectangle cr Rect.{x=0.; y=size.h-.17.; w=200.; h=size.h};
        Graphics.fill cr;
        let text = Printf.sprintf "S %.3fms D %.3fms" t1 t2 in
        Graphics.set_color cr Color.black;
        Graphics.set_font_info cr stat_font;
        Graphics.draw_text_ cr Pos.{x=0.; y=size.h -. 2.} text;

    method renderRefresh (cr, rect) =
        let open Rect in
        (*Stdio.printf "REFRESH RECT %.2f %.2f %.2f %.2f\n%!" rect.x rect.y rect.w rect.h;*)
        Graphics.clip_rect cr rect;
        let _, s1 = Util.time (fun _ ->
            SpatialIndex.search (index , rect, searchResults);
            Util.dynarray_sort (searchResults, fun (obj1, obj2) ->
                obj1#fullZIndex - obj2#fullZIndex
            );
        ) in
        (*Stdio.printf "SEARCH FOUND %d\n%!" (DynArray.length searchResults);*)
        DynArray.iter (fun obj ->
            (*Stdio.printf "DRAWING %s\n%!" obj#toString;*)
            obj#draw cr
        ) searchResults;
        Graphics.clip_reset cr;
        s1

    method renderChanged (cr, rect, old_rect) =
        (* Draw over old rect *)
        let s1 = self#renderRefresh (cr, old_rect)
        and s2 = self#renderRefresh (cr, rect) in
        s1+.s2

    method render cr =
        (*let draw_debug_rects (cr, rect, old_rect) =
            Graphics.set_color cr Color.{r=Random.float 1.; g=Random.float 1.; b=Random.float 1.; a=1.};
            Graphics.rectangle cr old_rect;
            Graphics.fill cr;
            Graphics.set_color cr Color.{r=Random.float 1.; g=Random.float 1.; b=Random.float 1.; a=1.};
            Graphics.rectangle cr rect;
            Graphics.fill cr;
        in*)
        if drawEnabled && DynArray.length updates > 0 then begin
            (*
            let print_tree () = 
                Stdio.printf "%s\n%!" (Test_rtree.str_tree rtree
                    (fun n -> n#toString)
                    (fun obj -> Layer.calc_rect (obj, obj#rect))
                )
            in
            print_tree();
            *)
            let searchTime, drawTime = Util.time (fun _ ->
                (* Check if there is a FullRefresh, if so, ignore everything else *)
                if DynArray.exists (fun u -> Poly.(u = FullRefresh)) updates then (
                    DynArray.clear updates;
                    DynArray.add updates FullRefresh;
                );
                Stdio.printf "Number of updates %d\n%!" (DynArray.length updates);
                DynArray.fold_left (fun acc update ->
                    let search_time =  
                        match update with
                        | Refresh rect ->
                            self#renderRefresh (cr, rect)
                        | Changed (rect, old_rect) ->
                            self#renderChanged (cr, rect, old_rect)
                        | FullRefresh ->
                            self#renderRefresh (cr, Rect.{x=0.; y=0.; w=size.w; h=size.h})
                    in
                    search_time+.acc
                ) 0. updates
            ) in
            self#drawStats (cr, searchTime, drawTime-.searchTime);
            DynArray.clear updates;
        end
end
