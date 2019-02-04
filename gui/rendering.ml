(* TODO figure out how to use JaneStreet's Incremental library for this *)

module R = Test_rtree
module Graphics = Platform.Windowing.Graphics
module HP = Hashtbl.Poly

type update_type = 
    | Refresh of Rect.t
    | Changed of Rect.t * Rect.t
    | FullRefresh

class groupObject render = object(self)
    val group = DrawTree.Group.create()
    val mutable id = ""

    method obj = DrawTree.Ex group

    method setId i = id <- i
    method addChild : DrawTree.any_object -> unit = fun (Ex c) ->
        DrawTree.set_parent (c, group);
        let rect = DrawTree.get_rect c in
        render#refreshSingle rect

    method removeChild : DrawTree.any_object -> unit = fun (Ex c) ->
        let rect = DrawTree.get_rect c in
        DrawTree.unparent c;
        render#refreshSingle rect

    method setZIndex (z : int) : unit =
        DrawTree.Group.set_z_index (group, z);
end

class viewportObject render = object
    val view = DrawTree.Viewport.create()
    val mutable id = ""

    method addChild : DrawTree.any_object -> unit = fun (Ex c) ->
        DrawTree.set_parent (c, view);
        let rect = DrawTree.get_rect c in
        render#refreshSingle rect

    method removeChild : DrawTree.any_object -> unit = fun (Ex c) ->
        let rect = DrawTree.get_rect c in
        DrawTree.unparent c;
        render#refreshSingle rect

    method setZIndex (z : int) : unit =
        DrawTree.Viewport.set_z_index (view, z);
        render#refreshSingle (DrawTree.Viewport.get_outer_bounds view)

    method setOuterBounds (r : Rect.t) : unit =
        let before = DrawTree.Viewport.get_outer_bounds view in
        if not (Rect.equal(before, r)) then (
            DrawTree.Viewport.set_outer_bounds (view, r);
            render#refreshChanged (before, r)
        )

    method setInnerBounds (r : Rect.t) : unit =
        let inner_bounds = DrawTree.Viewport.get_inner_bounds view in
        if not (Rect.equal(inner_bounds, r)) then (
            DrawTree.Viewport.set_inner_bounds (view, r);
            render#refreshSingle (DrawTree.Viewport.get_outer_bounds view)
        )

    method obj = DrawTree.Ex view
    method setTranslation (x, y : float * float) : unit =
        let bounds = DrawTree.Viewport.get_inner_bounds view in
        if not (Pos.equal(Pos.{x;y}, Pos.{x=bounds.x;y=bounds.y})) then (
            DrawTree.Viewport.set_inner_bounds (view, Rect.{bounds with x; y});
            render#refreshSingle (DrawTree.Viewport.get_outer_bounds view);
        )
end

class userObject render = object
    val user = DrawTree.User.create()
    val mutable id = ""

    method obj = DrawTree.Ex user

    method setId i = id <- i

    method setPos (pos : Pos.t) : unit =
        let before = DrawTree.User.get_bounds user in
        if not (Pos.equal (pos, Pos.{x=before.x; y=before.y})) then (
            DrawTree.User.set_pos (user, pos);
            let after = DrawTree.User.get_bounds user in
            render#refreshChanged (before, after)
        )

    method setZIndex (z : int) : unit =
        DrawTree.User.set_z_index (user, z);
        render#refreshSingle (DrawTree.User.get_bounds user);

    method setFunc (fn : Graphics.context -> unit) =
        DrawTree.User.set_func (user, fn)

    method setBounds (r : Rect.t) : unit =
        let before = DrawTree.User.get_bounds user in
        if not (Rect.equal(before, r)) then (
            DrawTree.User.set_bounds (user, r);
            let after = DrawTree.User.get_bounds user in
            render#refreshChanged (before, after);
        )
end

class rectObject render = object
    val rect = DrawTree.Rect.create()
    val mutable id = ""

    method obj = DrawTree.Ex rect

    method setId i = id <- i
    method setRect (r : Rect.t) : unit =
        (* TODO - when stroke draw type, create 8 updates 
         * that are just the outlines
         * *)
        let before = DrawTree.Rect.get_rect rect in
        if not (Rect.equal(before, r)) then (
            DrawTree.Rect.set_rect (rect, r);
            let after = DrawTree.Rect.get_rect rect in
            render#refreshChanged (before, after)
        )

    method setColor (color : Color.t) : unit =
        DrawTree.Rect.set_color (rect, color);
        render#refreshSingle (DrawTree.get_rect rect);

    method setZIndex (z : int) : unit =
        DrawTree.Rect.set_z_index (rect, z);
        render#refreshSingle (DrawTree.Rect.get_bounds rect);

    method setMode (m : DrawTree.draw_type) : unit =
        DrawTree.Rect.set_mode (rect, m);
        render#refreshSingle (DrawTree.get_rect rect);
end

class textObject render = object
    val text = DrawTree.Text.create()
    val mutable id = ""

    method obj = DrawTree.Ex text

    method setPos (pos : Pos.t) : unit =
        let before = DrawTree.Text.get_bounds text in
        if not (Pos.equal (pos, Pos.{x=before.x; y=before.y})) then (
            DrawTree.Text.set_pos (text, pos);
            let after = DrawTree.Text.get_bounds text in
            render#refreshChanged (before, after)
        )

    method setText (str : string) : unit =
        let before = DrawTree.Text.get_bounds text in
        DrawTree.Text.set_text (text, str);
        let after = DrawTree.Text.get_bounds text in
        render#refreshSingle (Rect.union before after)

    method size : Size.t = 
        let r = DrawTree.Text.get_bounds text in
        Size.{w=r.w; h=r.h}

    method text = DrawTree.Text.get_text text
    method fontExtents : Font.font_extents = render#fontExtents Font.default_font
    method font = DrawTree.Text.get_font text
    method setId i = id <- i
    method setZIndex (z : int) : unit =
        DrawTree.Text.set_z_index (text, z);
        render#refreshSingle (DrawTree.Text.get_bounds text);
end

class renderer = object(self)
    val mutable requestDraw : unit -> unit = fun _ -> ()
    val mutable updates : update_type DynArray.t = DynArray.create ~capacity:10 ()
    val root = DrawTree.Viewport.create()

    method setRoot : DrawTree.any_object -> unit = fun (Ex obj) ->
        DrawTree.set_parent (obj, root)

    method createGroupObject = new groupObject self
    method createRectObject = new rectObject self
    method createTextObject = new textObject self
    method createViewportObject = new viewportObject self 
    method createUserObject = new userObject self

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
        DrawTree.Viewport.set_outer_bounds (root, Rect.{x=0.; y=0.; w=s.w; h=s.h});
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

    method renderRefresh (cr, rect : Graphics.context * Rect.t) =
        (*Caml.print_endline (Printf.sprintf "REFRESH RECT %.2f %.2f %.2f %.2f\n%!" rect.x rect.y rect.w rect.h);*)
        Graphics.save cr;
        Graphics.clip_rect cr rect;
        let s1 =  DrawTree.draw (cr, rect, root) in
        Graphics.restore cr;
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
            DrawTree.print_tree root;
            (*Caml.print_endline (DrawTree.str_tree root);*)
            let searchTime, drawTime = Util.time (fun _ ->
                (* Check if there is a FullRefresh, if so, ignore everything else *)
                if  DynArray.length updates > 50
                    || DynArray.exists (fun u -> Poly.(u = FullRefresh)) updates then (
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
