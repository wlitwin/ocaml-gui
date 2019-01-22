(* TODO figure out how to use JaneStreet's Incremental library for this *)

module R = Test_rtree

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

type render_list = RectangleList of rectangle_list
                 | TextList of text_list

type text_key = (draw_type * Color.t * Font.t)
type rect_key = (draw_type * Color.t)

module HP = Hashtbl.Poly
let group_prims prims =
    let text_tbl : (text_key, (Pos.t * string) list) HP.t = HP.create()
    and rect_tbl : (rect_key, Rect.t list) HP.t = HP.create() in
    let add (key, value) tbl =
        match HP.find tbl key with
        | None -> HP.set tbl key [value]
        | Some lst -> HP.set tbl key (value :: lst)
    in
    List.iter prims (function
        | {shape_type=Rectangle sz; pos; draw_type; color} -> 
            let key = (draw_type, color) in
            add (key, Rect.{x=pos.x; y=pos.y; w=sz.w; h=sz.h}) rect_tbl
        | {shape_type=Text ti; pos; draw_type; color} ->
            let key = (draw_type, color, ti.font) in
            add (key, (pos, ti.text)) text_tbl
    );
    let lst =
        Hashtbl.fold ~init:[] ~f:(fun ~key:(draw_type, color, font) ~data:text acc ->
            TextList {
                draw_type; color; font; text
            } :: acc
      ) text_tbl
    in
    Hashtbl.fold ~init:lst ~f:(fun ~key:(draw_type, color) ~data:rects acc ->
        RectangleList {
            draw_type; color; rects
        } :: acc
    ) rect_tbl
;;

type obj = < >

class nodeObject renderer = object(self)
    val mutable content = Group [0, []]
    val mutable children : nodeObject DynArray.t = DynArray.create ~capacity:1 ()
    val renderer = renderer
    val mutable z_index = 0
    val mutable parent : nodeObject option = None

    method rect = Rect.empty

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
        renderer#refreshChanged ((self :> nodeObject), old_rect);
        renderer#addObject (self :> nodeObject);

    method children = children

    method toString = ""

    method attach (obj : nodeObject) : unit  =
        renderer#groupUpdates (fun _ ->
            match obj#parent with
            | None -> 
                obj#setParent (Some (self :> nodeObject));
                DynArray.add children obj;
                renderer#addObject (obj :> nodeObject);
                renderer#refreshChanged ((obj :> nodeObject), obj#rect)
            | Some p when phys_equal p (self :> nodeObject) -> ()
            | Some p -> 
                p#detach obj;
                renderer#removeObject (obj :> nodeObject);
                obj#setParent (Some (self :> nodeObject));
        );

    method draw (cr : Platform.Windowing.Graphics.context) : unit =
        DynArray.iter (fun child ->
            child#draw cr
        ) children

    method detach (obj : nodeObject) : unit =
        obj#setParent None;
        DynArray.filter (fun o -> not (phys_equal obj o)) children;
        renderer#refreshChanged ((obj :> nodeObject), obj#rect)
end

let sort_tree root =
    let z_tbl : (int, primitive list) HP.t = HP.create() in
    let add_prims_by_z (z_index, pos, prims) =
        List.iter prims (function
            | {pos=loc} as prim -> 
                let prim = {prim with pos=Pos.add pos loc} in
                match HP.find z_tbl z_index with
                | None -> HP.set z_tbl z_index [prim]
                | Some lst -> HP.set z_tbl z_index (prim :: lst)
        )
    in
    let rec traverse (z_index, pos : int * Pos.t) (node : nodeObject) =
        let z_index = z_index + node#zIndex in
        match node#content with
        | Translate amt -> DynArray.iter (traverse (z_index, Pos.add pos amt)) node#children 
        | Group groups ->
            List.iter groups (fun (idx, prims) -> 
                let z_index = z_index + idx in
                add_prims_by_z (z_index, pos, prims);
            );
            DynArray.iter (traverse (z_index, pos)) node#children 
        | Primitive primitive -> add_prims_by_z (z_index, pos, [primitive])
    in
    traverse (0, Pos.zero) root;
    let keys = 
        HP.keys z_tbl 
        |> List.sort ~compare
    in
    let count = List.length keys in
    let sorted : render_list list list = List.fold keys ~init:[] ~f:(fun lst z_index ->
        let prims = HP.find_exn z_tbl z_index in
        (group_prims prims) :: lst
    ) in
    let z_sorted = Array.create ~len:count [] in
    List.iteri sorted (fun idx lst ->
        z_sorted.(count - idx - 1) <- lst
    );
    z_sorted
;;

module Graphics = Platform.Windowing.Graphics

let draw_tree (cr, tree) =
    (*Stdio.printf "RENDER LIST IS %d IN LENGTH\n" (List.length tree);*)
    Array.iter tree (fun lst ->
        List.iter lst (function
            | TextList {draw_type=Stroke} -> failwith "Unsupported stroke text"
            | TextList {draw_type=Fill; font; color; text} ->
                (*Util.timeit "TEXT LIST TIME" (fun _ ->*)
                    (*Stdio.printf "TEXT LIST color %.2f %.2f %.2f %d\n" color.r color.g color.b (List.length text);*)
                    Graphics.set_color cr color;
                    Graphics.set_font_info cr font;
                    List.iter text (fun (pos, text) ->
                        Graphics.draw_text_ cr pos text
                        (*Graphics.draw_text cr font Rect.{x=pos.x; y=pos.y; w=0.; h=0.} text*)
                    )
                (* ) *)
            | RectangleList {draw_type=Fill; color; rects} ->
                (*Util.timeit "RECT LIST [F] TIME" (fun _ ->*)
                    (*Stdio.printf "RECT LIST (F) %.2f %.2f %.2f %d\n" color.r color.g color.b (List.length rects);*)
                    Graphics.set_color cr color;
                    List.iter rects (fun r ->
                        Graphics.rectangle cr r;
                        Graphics.fill cr;
                    )
                (* ) *)
            | RectangleList {draw_type=Stroke; color; rects} ->
                (*Util.timeit "RECT LIST [S] TIME" (fun _ ->*)
                    (*Stdio.printf "RECT LIST (S) %.2f %.2f %.2f %d\n" color.r color.g color.b (List.length rects);*)
                    Graphics.set_color cr color;
                    List.iter rects (fun r ->
                        Graphics.rectangle cr r;
                        Graphics.stroke cr;
                    )
                (* ) *)
    ))
;;

let print_tree tree =
    let rec print idnt (node : nodeObject) =
        let pad = String.make idnt ' ' in
        begin match node#content with
        | Group lst -> 
            let items = List.fold lst ~init:0 ~f:(fun acc (_, lst) ->
                List.length lst + acc
            ) in
            Stdio.printf "%sGROUP [%d items]" pad items
        | Translate _ -> Stdio.printf "%sTranslate" pad
        | Primitive {shape_type=Rectangle _} -> Stdio.printf "%sPrim[Rect]" pad
        | Primitive {shape_type=Text _} -> Stdio.printf "%sPrim[Text]" pad
        end;
        Stdio.printf " -- with children %d\n" (DynArray.length node#children);
        DynArray.iter (print (idnt+2)) node#children 
    in
    print 0 tree;
;;

let draw (cr, tree : Graphics.context * nodeObject) =
    print_tree tree;
    let tree, tree_time = Util.time (fun _ ->
        sort_tree tree
    ) in
    let _, draw_time = Util.time (fun _ ->
        draw_tree (cr, tree)
    ) in
    tree_time, draw_time
;;

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

    method! toString = "T[" ^ self#text ^ "]"

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
        Printf.sprintf "R[%.2f %.2f %.2f]" color.r color.g color.b

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

    method setTranslation (x, y) : unit =
        self#setContent (Translate Pos.{x; y});
        renderer#refreshSingle (self :> nodeObject)
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

    val rtree : nodeObject Rtree.t = Rtree.create()
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
        if not (Rect.is_empty rect) then (
            let rect = Layer.calc_rect (obj, obj#rect) in
            Rtree.insert (rtree, rect, obj)
        )

    method removeObject (obj : nodeObject) : unit =
        let rect = Layer.calc_rect (obj, obj#rect) in
        Rtree.delete (rtree, rect, fun o ->
            phys_equal o obj)

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
            let rect = Layer.calc_rect (obj, obj#rect)
            and old_rect = Layer.calc_rect (obj, old_rect) in
            DynArray.add updates (Changed (rect, old_rect));
        )

    method refreshSingle (obj : nodeObject) =
        if shouldUpdate then (
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
        Graphics.clip_rect cr rect;
        let _, s1 = Util.time (fun _ ->
            Rtree.search (rtree, rect, searchResults);
            Util.dynarray_sort (searchResults, fun (obj1, obj2) ->
                obj1#fullZIndex - obj2#fullZIndex
            );
            DynArray.iter (fun obj ->
                obj#draw cr
            ) searchResults
        ) in
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
            let print_tree () = 
                Stdio.printf "%s\n%!" (Test_rtree.str_tree rtree
                    (fun n -> n#toString)
                    (fun obj -> Layer.calc_rect (obj, obj#rect))
                )
            in
            print_tree();
            let searchTime, drawTime = Util.time (fun _ ->
                (* Check if there is a FullRefresh, if so, ignore everything else *)
                if DynArray.exists (fun u -> Poly.(u = FullRefresh)) updates then (
                    DynArray.clear updates;
                    DynArray.add updates FullRefresh;
                );
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
