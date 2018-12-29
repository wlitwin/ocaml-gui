type scrollBarType = VerticalScroller
                   | HorizontalScroller

class scrollBar app scrollType = object(self)
    inherit Widget.basicWidget app as super

    val mutable pos = 0.
    val barSize = 10.
    val scrollType = scrollType
    val mutable ratio = 1.

    method setPosition (p : float) : unit =
        pos <- Util.clamp p 0. 1.

    method position = pos
    method barSize = barSize
    method ratio = ratio
    method setRatio new_ratio = ratio <- new_ratio

    method incrPos amt =
        self#setPosition (self#position +. amt)

    method decrPos amt =
        self#setPosition (self#position -. amt)

    method private paintVertical cr =
        let open Float in
        let barHeight = min (ratio *. rect.h) rect.h in
        Cairo.set_source_rgba cr 0. 0. 0. 1.;
        let y = rect.y +. pos *. (rect.h -. barHeight) in
        Cairo.rectangle cr rect.x y barSize barHeight;
        Cairo.fill cr

    method private paintHorizontal cr =
        let open Float in
        let barWidth  = min (ratio *. rect.w) rect.w in
        Cairo.set_source_rgba cr 0. 0. 0. 1.;
        let x = rect.x +. pos *. (rect.w -. barWidth) in
        Cairo.rectangle cr x rect.y barWidth barSize;
        Cairo.fill cr

    method! onDraw cr =
        if Float.(ratio < 1.) then
            super#onDraw cr

    method! paint cr =
        Cairo.clip_reset cr;
        Cairo.set_source_rgba cr 1. 1. 1. 1.;
        Cairo.rectangle cr rect.x rect.y rect.w rect.h;
        Cairo.fill cr;
        match scrollType with
        | VerticalScroller -> self#paintVertical cr
        | HorizontalScroller -> self#paintHorizontal cr
end

class scrollArea app cont =
object(self)
    inherit Widget.basicWidget app as super

    val mutable cont : Widget.basicWidget = cont
    val mutable horiz = true
    val mutable vert = true
    val vertScroller = new scrollBar app VerticalScroller
    val horzScroller = new scrollBar app HorizontalScroller

    method setControl c =
        cont <- c

    method! onKeyDown k =
        let ratio, fn = 
            let open Application in
            match k with
            | Keys.A when app#specialKeys.ctrlDown -> 0.0, vertScroller#setPosition
            | Keys.E when app#specialKeys.ctrlDown -> 1.0, vertScroller#setPosition
            | Keys.J when app#specialKeys.ctrlDown -> vertScroller#ratio*.0.5, vertScroller#incrPos
            | Keys.K when app#specialKeys.ctrlDown -> vertScroller#ratio*.0.5, vertScroller#decrPos
            | Keys.L when app#specialKeys.ctrlDown -> horzScroller#ratio*.0.5, horzScroller#incrPos
            | Keys.H when app#specialKeys.ctrlDown -> horzScroller#ratio*.0.5, horzScroller#decrPos
            | Keys.J -> vertScroller#ratio, vertScroller#incrPos
            | Keys.K -> vertScroller#ratio, vertScroller#decrPos
            | Keys.L -> horzScroller#ratio, horzScroller#incrPos
            | Keys.H -> horzScroller#ratio, horzScroller#decrPos
            | _ -> 1.0, fun _ -> ()
        in
        fn ratio;
        self#invalidate;
        Mixins.Propagate

    method! onResize r =
        super#onResize r |> ignore;
        let open Float in
        let sz = cont#preferredSize in
        let w = max rect.w sz.w
        and h = max rect.h sz.h in
        cont#onResize Rect.{x=0.; y=0.; w; h} |> ignore;
        (* Position the scroll bars *)
        let barSizeH = vertScroller#barSize in
        let barSizeW = horzScroller#barSize in
        vertScroller#setRatio (rect.h /. h);
        horzScroller#setRatio (rect.w /. w);
        vertScroller#resize Rect.{x=rect.x +. rect.w -. barSizeW; 
                                  y=rect.y; h=rect.h; w=barSizeW};
        horzScroller#resize Rect.{x=rect.x;
                                  y=rect.y +. rect.h -. barSizeH; h=barSizeH; w=rect.w};
        Mixins.Propagate

    method ensureVisible (region : Rect.t) =
        let open Float in
        let cgeom = cont#rect in
        let r = Rect.intersection region cgeom in
        let ox = self#offsetX in
        let oy = self#offsetY in
        let v = { self#rect with x=ox; y=oy; } in
        if not (Rect.inside r v) then begin
            let oy1 = Float.abs ((r.y +. r.h) -. (v.y +. v.h))
            and oy2 = Float.abs (v.y -. r.y)
            and ox1 = Float.abs ((r.x +. r.w) -. (v.x +. v.w))
            and ox2 = Float.abs (v.x -. r.x) in
            let vdenom = max (cont#rect.h -. self#rect.h) 1. in
            let wdenom = max (cont#rect.w -. self#rect.w) 1. in
            let vpos = (if oy1 < oy2 then (oy +. oy1) else (oy -. oy2)) /. vdenom in
            let wpos = (if ox1 < ox2 then (ox +. ox1) else (ox -. ox2)) /. wdenom in
            vertScroller#setPosition vpos;
            horzScroller#setPosition wpos;
        end

    (* Can never be < 0 or > cont.w - rect.w *)
    method offsetX =
        horzScroller#position *. (cont#rect.w -. self#rect.w)

    (* Can never be < 0 or > cont.h - rect.h *)
    method offsetY =
        vertScroller#position *. (cont#rect.h -. self#rect.h)

    method! contentSize =
        cont#preferredSize

    method! paint cr =
        (* Create a cairo surface *)
        Cairo.save cr;
        let module CI = Cairo.Image in
        let r = rect in
        let w, h = Util.iceil r.w, Util.iceil r.h in
        let img = CI.create CI.ARGB32 w h in
        let cri = Cairo.create img in
        let ox = self#offsetX
        and oy = self#offsetY in
        Cairo.translate cri (-.ox) (-.oy);
        cont#onDraw cri;
        let x = rect.x in
        let y = rect.y in
        Cairo.set_source_surface cr img x y;
        Cairo.paint cr;
        Cairo.restore cr;
        vertScroller#onDraw cr;
        horzScroller#onDraw cr;
end
