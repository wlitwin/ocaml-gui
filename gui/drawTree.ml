module Graphics = Platform.Windowing.Graphics

open GadtUtils

type draw_type = Stroke
               | Fill

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

and user_rec = {
    u_common : common;
    mutable draw_func : Graphics.context -> unit;
}

and any_object = Ex : 'a t -> any_object [@@ocaml.boxed]

and parent_object = P : (unit * 'a) t -> parent_object [@@ocaml.boxed]

and group = group_rec
and view = view_rec
and text = text_rec
and prim = prim_rec
and user = user_rec

and _ t =
    | Text : text -> text t
    | Rect : prim -> prim t
    | Group : group -> (unit * group) t
    | Viewport : view -> (unit * view) t
    | User : user -> user t

let get_rect : type a. a t -> Rect.t = function
    | Text t -> t.t_common.bounds
    | Rect r -> r.p_common.bounds
    | Group g -> Rect.empty
    | Viewport v -> v.v_common.bounds
    | User u -> u.u_common.bounds

let get_common : type a. a t -> common = function
    | Text t -> t.t_common
    | Rect r -> r.p_common
    | Group g -> g.g_common
    | Viewport v -> v.v_common
    | User u -> u.u_common

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
                | Ex (User _ as user) -> loop (user, parent)
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
        | z, User u ->
            u.u_common.absolute_z_index <- u.u_common.relative_z_index + z
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
                | Ex (User _ as user) -> loop (user, parent)
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
        | User u -> u.u_common.absolute_z_index <- calc_z_index c
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
        Caml.print_endline (Printf.sprintf "%s\n%!" (Test_rtree.str_tree tree.index.rtree
            (fun (id, Ex obj) -> 
                Printf.sprintf "%s %d" 
                (match obj with
                | Rect _ -> "rect"
                | Text t -> "text[" ^ t.text ^ "]"
                | Group _ -> "group"
                | User _ -> "user"
                | Viewport _ -> "view"
                )
                (get_common obj).absolute_z_index
            )
            (fun (id, Ex obj) -> (get_common obj).bounds)
        ))
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

let draw (cr, rect, obj) =
    let search_time = ref 0. in
    let rec draw : type a. Graphics.context * Rectangle.t * a t -> unit = 
    function
    | cr, _, Text t -> draw_text (cr, t)
    | cr, _, Rect r -> draw_rect (cr, r)
    | cr, _, User u -> u.draw_func cr
    | cr, rect, Group g ->
        DynArray.iter (fun (Ex v) -> draw (cr, rect, v)) g.group_children
    | cr, rect, Viewport v -> 
        Graphics.save cr;
        Graphics.clip_rect cr v.v_common.bounds;
        let translate_x = Float.round (v.v_common.bounds.x -. v.inner_bounds.x) in
        let translate_y = Float.round (v.v_common.bounds.y -. v.inner_bounds.y) in
        Graphics.translate cr translate_x translate_y;
        let search_rect = Rectangle.{rect with x=rect.x -. translate_x; y=rect.y-.translate_y} in
        (*print_tree (Viewport v);*)
        let _, time = Util.time (fun _ ->
            SpatialIndex.search (v.index, search_rect, v.search_results);
            Util.dynarray_sort (v.search_results, (fun ((_, Ex item1), (_, Ex item2)) ->
                (get_common item1).absolute_z_index - (get_common item2).absolute_z_index
            ));
        ) in
        search_time := !search_time +. time;
        DynArray.iter (fun (_, Ex v) -> draw (cr, rect, v)) v.search_results;
        Graphics.restore cr;
    in
    draw (cr, rect, obj);
    !search_time

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
            | User u, idnt -> Printf.sprintf "%suser %d (%d) [%d]\n" (pad idnt) u.u_common.absolute_z_index u.u_common.relative_z_index (calc_z_index (User u))
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
            let size = RenderUtils.measure_text (t.font, t.text) in
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

module User = struct
    let create () = User {
        u_common=common();
        draw_func=ignore;
    }

    let set_func : user t * (Graphics.context -> unit) -> unit = function
        | User u, f -> u.draw_func <- f;
    ;;

    let update : user t -> unit = function
        | User _ as user -> update user
    ;;

    let get_bounds : user t -> Rectangle.t = function
        | User u -> u.u_common.bounds
    ;;

    let set_pos : user t * Pos.t -> unit = function
        | User u as u_tag, pos -> 
            Common.set_pos (u.u_common, pos);
            update u_tag;
    ;;

    let set_bounds : user t * Rectangle.t -> unit = function
        | User u as u_tag, r -> 
            u.u_common.bounds <- r;
            update u_tag;
    ;;

    let set_z_index : user t * int -> unit = function
        | User u as user, z_index ->
            u.u_common.relative_z_index <- z_index;
            u.u_common.absolute_z_index <- calc_z_index user;
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
