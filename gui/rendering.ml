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

module Rtree = Rtree2

module SpatialIndex = struct
    module HP = Hashtbl.Poly

    type ('a, 'b) t = {
        rtree : ('a * 'b) Rtree.t;
        object_table : ('a, Rect.t) HP.t;
        cmp : 'a -> ('a * 'b) -> bool;
    }

    let create cmp = {
        rtree = Rtree.create();
        object_table = HP.create();
        cmp;
    }

    let add (t, (key, obj), rect) =
        if not (Rect.is_empty rect) && not (HP.mem t.object_table key) then (
            HP.set t.object_table key rect;
            Rtree.insert (t.rtree, rect, (key, obj));
        )

    let search (t, rect, results) =
        Rtree.search (t.rtree, rect, results)

    let remove (t, key) =
        if HP.mem t.object_table key then (
            let rect = HP.find_exn t.object_table key in
            HP.remove t.object_table key;
            Rtree.delete (t.rtree, rect, t.cmp key)
        )
end

module Drawable = struct
    type notifiable = <
        changed : Rect.t -> unit
    >

    type id = < >

    type common = {
        mutable parent : parent_object option;
        mutable bounds : Rect.t;
        mutable relative_z_index : int;
        mutable absolute_z_index : int;
        id : id;
    }

    and prim_rec = {
        mutable color : Color.t;
        mutable fill_type : draw_type;
        p_common : common;
    }

    and text_rec = {
        mutable text : string;
        mutable font : Font.t;
        mutable t_color : Color.t;
        mutable t_fill_type : draw_type;
        t_common : common;
    }

    and view_rec = {
        mutable inner_bounds : Rect.t;
        index : (id, any_object) SpatialIndex.t;
        search_results : (id * any_object) DynArray.t;
        view_children : any_object DynArray.t;
        v_common : common;
    }

    and group_rec = {
        g_common : common;
        group_children : any_object DynArray.t;
    }

    and any_object = Ex : 'a t -> any_object [@@ocaml.boxed]

    and parent_object = P : (unit * 'a) t -> parent_object [@@ocaml.boxed]

    and group = group_rec
    and view = view_rec
    and text = text_rec
    and prim = prim_rec

    and _ t =
        | Text : text -> text t
        | Rect : prim -> prim t
        | Group : group -> (unit * group) t
        | Viewport : view -> (unit * view) t

    let get_rect : type a. a t -> Rect.t = function
        | Text t -> t.t_common.bounds
        | Rect r -> r.p_common.bounds
        | Group g -> Rect.empty
        | Viewport v -> v.v_common.bounds

    let get_common : type a. a t -> common = function
        | Text t -> t.t_common
        | Rect r -> r.p_common
        | Group g -> g.g_common
        | Viewport v -> v.v_common

    let get_id v = (get_common v).id

    let delete_from_nearest_viewport : type a. a t -> unit = 
        let rec loop : type a. a t * common -> unit = function
            | _, {parent=None} -> ()
            | Group g, parent ->
                (* Need to delete all children as well *)
                DynArray.iter (function
                    | Ex (Text _ as text) -> loop (text, parent)
                    | Ex (Rect _ as rect) -> loop (rect, parent)
                    | Ex (Group _ as group) -> loop (group, parent)
                    | Ex (Viewport _ as view) -> loop (view, parent)
                ) g.group_children;
            | c, {parent=Some (P (Viewport v))} ->
                    SpatialIndex.remove (v.index, get_id c)
            | c, {parent=Some (P (Group g))} -> loop (c, g.g_common)
        in
        function
        | child -> loop (child, get_common child)
    ;;

    let calc_z_index : type a. a t -> int = fun obj ->
        let rec loop : type a. int * a t -> int = function 
            | z, obj ->
                let common = get_common obj in
                let z = z + common.relative_z_index in
                match common.parent with
                | None -> z
                | Some (P p) -> loop (z, p)
        in
        loop (0, obj)
    ;; 

    let rec update_z_index : type a. (unit * a) t -> unit = 
        let rec loop : type a. int * a t -> unit = function
            | z, Text t ->
                t.t_common.absolute_z_index <- t.t_common.relative_z_index + z
            | z, Rect r ->
                r.p_common.absolute_z_index <- r.p_common.relative_z_index + z
            | z, Viewport v ->
                let z = v.v_common.relative_z_index + z in
                v.v_common.absolute_z_index <- z;
                DynArray.iter (fun (Ex child) -> loop (z, child)) v.view_children
            | z, Group g ->
                let z = g.g_common.relative_z_index + z in
                g.g_common.absolute_z_index <- z;
                DynArray.iter (fun (Ex child) -> loop (z, child)) g.group_children
        in
        function
        | Group g as group ->
            let z_index = calc_z_index group in
            g.g_common.absolute_z_index <- z_index;
            DynArray.iter (fun (Ex child) -> loop (z_index, child)) g.group_children
        | Viewport v as view ->
            let z_index = calc_z_index view in
            v.v_common.absolute_z_index <- z_index;
            DynArray.iter (fun (Ex child) -> loop (z_index, child)) v.view_children
    ;;

    let add_to_nearest_viewport : type a. a t -> unit = 
        let rec loop : type a. a t * common -> unit = function
            | Group g, parent ->
                (* Need to add each child individually *)
                DynArray.iter (function
                    | Ex (Text _ as text) -> loop (text, parent)
                    | Ex (Rect _ as rect) -> loop (rect, parent)
                    | Ex (Group _ as group) -> loop (group, parent)
                    | Ex (Viewport _ as view) -> loop (view, parent)
                ) g.group_children;
                (* And need to update this group's index *)
            | child, {parent=Some (P (Viewport v))} ->
                let rect = get_rect child in
                SpatialIndex.add (v.index, (get_id child, Ex child), rect);
            | child, {parent=Some (P (Group g))} ->
                loop (child, g.g_common)
            | _, {parent=None} -> ()
        in
        function
        | child -> loop (child, get_common child)
    ;;

    let update : type a. a t -> unit = 
        let rec loop : type a. a t * common -> unit = function
            | _, {parent=None} -> ()
            | Viewport _ as child, {parent=Some (P (Viewport v))} ->
                let rect = get_rect child in
                let id = get_id child in
                (*Stdio.printf "UPDATEING VIEWPROTING %f %f %f %f\n%!" rect.x rect.y rect.w rect.h;*)
                SpatialIndex.remove (v.index, id);
                SpatialIndex.add (v.index, (id, Ex child), rect);
            | child, {parent=Some (P (Viewport v))} ->
                let rect = get_rect child in
                let id = get_id child in
                SpatialIndex.remove (v.index, id);
                SpatialIndex.add (v.index, (id, Ex child), rect);
            | child, {parent=Some (P (Group g))} ->
                loop (child, g.g_common)
        in
        function
        | child -> loop (child, get_common child) 
    ;;

    let get_children : type a. (unit * a) t -> any_object DynArray.t = function
        | Group g -> g.group_children
        | Viewport v -> v.view_children
    ;;

    let remove_child : type a b. (unit * a) t * b t -> unit = 
        let filter c = 
            let common = get_common c in
            DynArray.filter (fun (Ex v) -> 
            if phys_equal (get_id v) common.id then (
                common.parent <- None;
                false
            ) else true
        ) in
        function
        | Group g, c ->
                (* Search up until the viewport is found *)
                delete_from_nearest_viewport c;
                filter c g.group_children
        | Viewport v, c ->
                SpatialIndex.remove (v.index, get_id c);
                filter c v.view_children

    let set_parent : type a b. a t * (unit * b) t -> unit = 
        let add_child (common, child, p) =
            common.parent <- Some (P p);
            DynArray.add (get_children p) (Ex child);
        in
        function
        | c, p -> 
            let common = get_common c in
            begin match common.parent, p with
            | Some (P ex_p), p when phys_equal (get_id ex_p) (get_id p) -> ()
            | Some (P ex_p), p -> 
                    remove_child (ex_p, c);
                    add_child (common, c, p);
                    add_to_nearest_viewport c;
            | None, p -> 
                    add_child (common, c, p);
                    add_to_nearest_viewport c;
            end;
            (* When the child is an object with children
             * we need to go through and update all their
             * z-indices *)
            match c with
            | Group _ as group -> update_z_index group
            | Viewport _ as view -> update_z_index view
            | Text t -> t.t_common.absolute_z_index <- calc_z_index c
            | Rect r -> r.p_common.absolute_z_index <- calc_z_index c
    ;;

    let unparent : type a. a t -> unit = function
        | child ->
            let common = get_common child in
            match common.parent with
            | Some (P p) -> remove_child (p, child)
            | None -> ()
    ;;

    let common () = {
        parent=None;
        id=object end;
        bounds=Rect.empty;
        relative_z_index=0;
        absolute_z_index=0;
    }

    let print_tree : type a. (unit * view) t -> unit = function
        | Viewport tree ->
            (*jStdio.printf "%s\n%!" (Test_rtree.str_tree tree.index.rtree
                (fun (id, Ex obj) -> 
                    Printf.sprintf "%s %d" 
                    (match obj with
                    | Rect _ -> "rect"
                    | Text t -> "text[" ^ t.text ^ "]"
                    | Group _ -> "group"
                    | Viewport _ -> "view"
                    )
                    (get_common obj).absolute_z_index
                )
                (fun (id, Ex obj) -> (get_common obj).bounds)
            )*)
            ()
    ;;

    module Rectangle = Rect

    let draw_text : Graphics.context * text -> unit = function
        | cr, text ->
            Graphics.set_color cr text.t_color;
            Graphics.draw_text cr text.font text.t_common.bounds text.text

    let draw_rect : Graphics.context * prim -> unit = function
        | cr, rect ->
            Graphics.set_color cr rect.color;
            Graphics.rectangle cr rect.p_common.bounds;
            match rect.fill_type with
            | Fill -> Graphics.fill cr
            | Stroke -> Graphics.stroke cr

    let rec draw : type a. Graphics.context * Rectangle.t * a t -> unit = 
        function
        | cr, _, Text t -> draw_text (cr, t)
        | cr, _, Rect r -> draw_rect (cr, r)
        | cr, rect, Group g ->
            DynArray.iter (fun (Ex v) -> draw (cr, rect, v)) g.group_children
        | cr, rect, Viewport v -> 
            Graphics.save cr;
            Graphics.clip_rect cr v.v_common.bounds;
            (*let outer = v.v_common.bounds in
            let inner = v.inner_bounds in*)
            let translate_x = Float.round (v.v_common.bounds.x -. v.inner_bounds.x) in
            let translate_y = Float.round (v.v_common.bounds.y -. v.inner_bounds.y) in
            (*Stdio.printf "OFFSET %f %f\n" translate_x translate_y;
            Stdio.printf "OUTER %f %f %f %f\n" outer.x outer.y outer.w outer.h;
            Stdio.printf "INNER %f %f %f %f\n%!" inner.x inner.y inner.w inner.h;*)
            Graphics.translate cr translate_x translate_y;
            let search_rect = Rectangle.{rect with x=rect.x -. translate_x; y=rect.y-.translate_y} in
            (*print_tree (Viewport v);*)
            SpatialIndex.search (v.index, search_rect, v.search_results);
            Util.dynarray_sort (v.search_results, (fun ((_, Ex item1), (_, Ex item2)) ->
                (get_common item1).absolute_z_index - (get_common item2).absolute_z_index
            ));
            DynArray.iter (fun (_, Ex v) -> draw (cr, rect, v)) v.search_results;
            Graphics.restore cr;

    module Common = struct
        let set_pos (c, p : common * Pos.t) =
            c.bounds <- {c.bounds with x=p.x; y=p.y}
    end

    let rec count_parents : type a. a t -> int = function
        | obj -> 
            match (get_common obj).parent with
            | None -> 0
            | Some (P p) -> count_parents p + 1
    ;;

    let rec str_tree : type a. a t -> string = function
        | obj ->
            let pad idnt = String.make idnt ' ' in
            let rec loop : type a. a t * int -> string = function
                | Rect r, idnt -> Printf.sprintf "%srect %d (%d) [%d]\n" (pad idnt) r.p_common.absolute_z_index r.p_common.relative_z_index (calc_z_index (Rect r))
                | Text t, idnt -> Printf.sprintf "%stext %s %d (%d) [%d]\n" (pad idnt) t.text t.t_common.absolute_z_index t.t_common.relative_z_index (calc_z_index (Text t))
                | Group g, idnt ->
                    Printf.sprintf "%sgroup %d (%d) [%d]\n%s" (pad idnt) g.g_common.absolute_z_index g.g_common.relative_z_index (calc_z_index (Group g))
                    (DynArray.fold_left (fun acc (Ex obj) ->
                        acc ^ loop (obj, idnt+2)
                    ) "" g.group_children)
                | Viewport v, idnt ->
                    Printf.sprintf "%sview %d (%d) [%d]\n%s" (pad idnt) v.v_common.absolute_z_index v.v_common.relative_z_index (calc_z_index (Viewport v))
                    (DynArray.fold_left (fun acc (Ex obj) ->
                        acc ^ loop (obj, idnt+2)
                    ) "" v.view_children)
            in
            loop (obj, 0)
    ;;

    module Viewport = struct
        let create ?(outer_bounds=Rect.empty) ?(inner_bounds=Rect.empty) () = Viewport {
            inner_bounds;
            index = SpatialIndex.create (fun key (id, _) -> phys_equal key id);
            search_results = DynArray.create();
            view_children = DynArray.create();
            v_common = common();
        }

        let set_inner_bounds : (unit * view) t * Rect.t -> unit = function
            | Viewport v, rect -> v.inner_bounds <- rect

        let set_outer_bounds : (unit * view) t * Rect.t -> unit = function
            | Viewport v as view, rect ->
                    v.v_common.bounds <- rect;
                    update view
        ;;

        let set_z_index : (unit * view) t * int -> unit = function
            | Viewport v as view, z_index ->
                v.v_common.relative_z_index <- z_index;
                update_z_index view;
                (*Stdio.printf "UPDATE Z INDEX (VIEW)\n%s%!"
                (str_tree view);*)
        ;;

        let get_index : (unit * view) t -> (id, any_object) SpatialIndex.t = function
            | Viewport v -> v.index

        let get_inner_bounds : (unit * view) t -> Rect.t = function
            | Viewport v -> v.inner_bounds

        let get_outer_bounds : (unit * view) t -> Rect.t = function
            | Viewport v -> v.v_common.bounds

        let iter : ((unit * view) t * (any_object -> unit)) -> unit = function
            | Viewport v, f -> DynArray.iter f v.view_children 
    end

    module Group = struct
        let create ?(children=[]) () = Group {
            group_children=DynArray.of_list children; g_common=common();
        }

        let set_z_index : (unit * group) t * int -> unit = function
            | Group g as group, z_index ->
                g.g_common.relative_z_index <- z_index;
                update_z_index group;
                (*Stdio.printf "UPDATE Z INDEX (GROUP)\n%s%!"
                (str_tree group);*)
        ;;

        let iter : (unit * group) t * (any_object -> unit) -> unit = function
            | Group g, f -> DynArray.iter f g.group_children
    end

    module Text = struct
        let create ?(text="") ?(font=Font.default_font) () = Text {
            text;
            font;
            t_color=Color.black;
            t_fill_type=Fill;
            t_common=common();
        }

        let get_font : text t -> Font.t = function
            | Text t -> t.font

        let update_text : text t -> unit = function
            | Text t as text ->
                let size = measure_text (t.font, t.text) in
                t.t_common.bounds <- Rect.{
                    t.t_common.bounds with
                    w=size.w; h=size.h;
                };
                update text
        ;;

        let get_text : text t -> string = function
            | Text t -> t.text

        let get_bounds : text t -> Rectangle.t = function
            | Text t -> t.t_common.bounds

        let set_text : text t * string -> unit = function
            | Text t as text, str -> 
                t.text <- str;
                update_text text;
        ;;

        let set_font : text t * Font.t -> unit = function
            | Text t as text, font -> 
                t.font <- font;
                update_text text;
        ;;

        let set_pos : text t * Pos.t -> unit = function
            | Text t as text, pos -> 
                Common.set_pos (t.t_common, pos);
                update_text text;
        ;;

        let set_z_index : text t * int -> unit = function
            | Text t as text, z_index ->
                t.t_common.relative_z_index <- z_index;
                t.t_common.absolute_z_index <- calc_z_index text;
        ;;
    end

    module Rect = struct
        let create ?(bounds=Rect.empty) ?(color=Color.red) ?(fill_type=Fill) () =
            Rect {
                color=Color.red;
                fill_type=Fill;
                p_common=common();
            }

        let get_rect : prim t -> Rectangle.t = function
            | Rect r -> r.p_common.bounds

        let set_rect : prim t * Rect.t -> unit = function
            | Rect r as r_tag, rect -> 
                r.p_common.bounds <- rect;
                update r_tag;
        ;;

        let set_z_index : prim t * int -> unit = function
            | Rect r as rect, z_index ->
                r.p_common.relative_z_index <- z_index;
                r.p_common.absolute_z_index <- calc_z_index rect;
        ;;

        let set_mode : prim t * draw_type -> unit = function
            | Rect r, fill_type -> r.fill_type <- fill_type
        ;;

        let set_color : prim t * Color.t -> unit = function
            | Rect r, color ->
                r.color <- color;
        ;;

        let get_bounds : prim t -> Rectangle.t = function
            | Rect r -> r.p_common.bounds

        let set_pos : prim t * Pos.t -> unit = function
            | Rect r as r_tag, pos -> 
                Common.set_pos (r.p_common, pos);
                update r_tag;
        ;;
    end
end

module Dirty = struct
type t = NotDirty
       | SingleUpdate of Rect.t
       | RenderAll

let is_dirty = function
    | NotDirty -> false
    | _ -> true
end

type update_type = 
    | Refresh of Rect.t
    | Changed of Rect.t * Rect.t
    | FullRefresh

class groupObject render = object(self)
    val group = Drawable.Group.create()
    val mutable id = ""

    method obj = Drawable.Ex group

    method setId i = id <- i
    method addChild : Drawable.any_object -> unit = fun (Ex c) ->
        Drawable.set_parent (c, group);
        let rect = Drawable.get_rect c in
        render#refreshSingle rect

    method removeChild : Drawable.any_object -> unit = fun (Ex c) ->
        let rect = Drawable.get_rect c in
        Drawable.unparent c;
        render#refreshSingle rect

    method setZIndex (z : int) : unit =
        Drawable.Group.set_z_index (group, z);
end

class viewportObject render = object
    val view = Drawable.Viewport.create()
    val mutable id = ""

    method addChild : Drawable.any_object -> unit = fun (Ex c) ->
        Drawable.set_parent (c, view);
        let rect = Drawable.get_rect c in
        render#refreshSingle rect

    method removeChild : Drawable.any_object -> unit = fun (Ex c) ->
        let rect = Drawable.get_rect c in
        Drawable.unparent c;
        render#refreshSingle rect

    method setZIndex (z : int) : unit =
        Drawable.Viewport.set_z_index (view, z);
        render#refreshSingle (Drawable.Viewport.get_outer_bounds view)

    method setOuterBounds (r : Rect.t) : unit =
        let before = Drawable.Viewport.get_outer_bounds view in
        if not (Rect.equal(before, r)) then (
            Drawable.Viewport.set_outer_bounds (view, r);
            render#refreshChanged (before, r)
        )

    method setInnerBounds (r : Rect.t) : unit =
        let inner_bounds = Drawable.Viewport.get_inner_bounds view in
        if not (Rect.equal(inner_bounds, r)) then (
            Drawable.Viewport.set_inner_bounds (view, r);
            render#refreshSingle (Drawable.Viewport.get_outer_bounds view)
        )

    method obj = Drawable.Ex view
    method setTranslation (x, y : float * float) : unit =
        let bounds = Drawable.Viewport.get_inner_bounds view in
        if not (Pos.equal(Pos.{x;y}, Pos.{x=bounds.x;y=bounds.y})) then (
            Drawable.Viewport.set_inner_bounds (view, Rect.{bounds with x; y});
            render#refreshSingle (Drawable.Viewport.get_outer_bounds view);
        )
end

class rectObject render = object
    val rect = Drawable.Rect.create()
    val mutable id = ""

    method obj = Drawable.Ex rect

    method setId i = id <- i
    method setRect (r : Rect.t) : unit =
        (* TODO - when stroke draw type, create 8 updates 
         * that are just the outlines
         * *)
        let before = Drawable.Rect.get_rect rect in
        if not (Rect.equal(before, r)) then (
            Drawable.Rect.set_rect (rect, r);
            let after = Drawable.Rect.get_rect rect in
            render#refreshChanged (before, after)
        )

    method setColor (color : Color.t) : unit =
        Drawable.Rect.set_color (rect, color);
        render#refreshSingle (Drawable.get_rect rect);

    method setZIndex (z : int) : unit =
        Drawable.Rect.set_z_index (rect, z);
        render#refreshSingle (Drawable.Rect.get_bounds rect);

    method setMode (m : draw_type) : unit =
        Drawable.Rect.set_mode (rect, m);
        render#refreshSingle (Drawable.get_rect rect);
end

class textObject render = object
    val text = Drawable.Text.create()
    val mutable id = ""

    method obj = Drawable.Ex text

    method setPos (pos : Pos.t) : unit =
        let before = Drawable.Text.get_bounds text in
        if not (Pos.equal (pos, Pos.{x=before.x; y=before.y})) then (
            Drawable.Text.set_pos (text, pos);
            let after = Drawable.Text.get_bounds text in
            render#refreshChanged (before, after)
        )

    method setText (str : string) : unit =
        let before = Drawable.Text.get_bounds text in
        Drawable.Text.set_text (text, str);
        let after = Drawable.Text.get_bounds text in
        render#refreshSingle (Rect.union before after)

    method size : Size.t = 
        let r = Drawable.Text.get_bounds text in
        Size.{w=r.w; h=r.h}

    method text = Drawable.Text.get_text text
    method fontExtents : Font.font_extents = render#fontExtents Font.default_font
    method font = Drawable.Text.get_font text
    method setId i = id <- i
    method setZIndex (z : int) : unit =
        Drawable.Text.set_z_index (text, z);
        render#refreshSingle (Drawable.Text.get_bounds text);
end

class renderer = object(self)
    val mutable requestDraw : unit -> unit = fun _ -> ()
    val mutable updates : update_type DynArray.t = DynArray.create ~capacity:10 ()
    val root = Drawable.Viewport.create()

    method setRoot : Drawable.any_object -> unit = fun (Ex obj) ->
        Drawable.set_parent (obj, root)

    method createGroupObject = new groupObject self
    method createRectObject = new rectObject self
    method createTextObject = new textObject self
    method createViewportObject = new viewportObject self 

    method fontExtents (font : Font.t) : Font.font_extents =
        Graphics.font_extents_no_context font

    val mutable drawEnabled = true
    val mutable shouldUpdate = true

    method ignoreUpdates (f : unit -> unit) : unit =
        let before = shouldUpdate in
        shouldUpdate <- false;
        f();
        shouldUpdate <- before;

    method groupUpdates (f : unit -> unit) : unit =
        let before = drawEnabled in
        drawEnabled <- false;
        f();
        drawEnabled <- before;
        self#requestDraw

    val mutable size = Size.zero
    method setSize (s : Size.t) : unit =
        Drawable.Viewport.set_outer_bounds (root, Rect.{x=0.; y=0.; w=s.w; h=s.h});
        size <- s

    method updates = updates

    method refreshFull =
        if shouldUpdate then (
            DynArray.clear updates;
            DynArray.add updates FullRefresh;
            self#requestDraw
        )

    method refreshChanged (old, new_) =
        if shouldUpdate then (
            (*Stdio.printf "ADDING REFRESH CHANGED %s\n%!" obj#toString;*)
            DynArray.add updates (Changed (old, new_));
        )

    method refreshSingle rect =
        if shouldUpdate then (
            (*Stdio.printf "ADDING REFRESH SINGLE %s\n%!" obj#toString;*)
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
        (*Stdio.printf "REFRESH RECT %.2f %.2f %.2f %.2f\n%!" rect.x rect.y rect.w rect.h;*)
        Graphics.clip_rect cr rect;
        let _, s1 = Util.time (fun _ ->
            Drawable.draw (cr, rect, root)
        ) in
        Graphics.clip_reset cr;
        s1

    method renderChanged (cr, old_rect, rect) =
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
            (*Drawable.print_tree root;*)
            (*Caml.print_endline (Drawable.str_tree root);*)
            let searchTime, drawTime = Util.time (fun _ ->
                (* Check if there is a FullRefresh, if so, ignore everything else *)
                if  DynArray.length updates > 50
                    || DynArray.exists (fun u -> Poly.(u = FullRefresh)) updates || true then (
                    DynArray.clear updates;
                    DynArray.add updates FullRefresh;
                );
                (*Stdio.printf "Number of updates %d\n%!" (DynArray.length updates);*)
                DynArray.fold_left (fun acc update ->
                    let search_time =  
                        match update with
                        | Refresh rect ->
                            self#renderRefresh (cr, rect)
                        | Changed (old_rect, new_rect) ->
                            self#renderChanged (cr, old_rect, new_rect)
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
