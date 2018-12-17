(*
open Rect
open Control

type scrollBarType = VerticalScroller
                   | HorizontalScroller

class scrollBar app scrollType = object(self)
    inherit control app as super

    val mutable pos = 0.
    val barSize = 10.
    val scrollType = scrollType
    val minBarOtherSize = 20.
    val mutable ratio = 1.

    method setPosition (p : float) : unit =
        pos <- Util.clamp p 0. 1.

    method position = pos
    method barSize = barSize
    method ratio = ratio
    method setRatio new_ratio = ratio <- new_ratio

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

    method! beginPaint cr =
        let open Float in
        if ratio < 1. then super#beginPaint cr

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
    let _app = app in
object(self)
    inherit control app as super

    val mutable cont : control = cont
    val mutable horiz = true
    val mutable vert = true
    val vertScroller = new scrollBar _app VerticalScroller
    val horzScroller = new scrollBar _app HorizontalScroller

    method setControl c =
        cont <- c

    method relayout cr =
        let open Float in
        let sz = cont#sizeHint cr in
        let w = max rect.w sz.w
        and h = max rect.h sz.h in
        cont#setGeometry {x=0.; y=0.; w; h};
        cont#relayout cr;
        (* Position the scroll bars *)
        let barSizeH = vertScroller#barSize in
        let barSizeW = horzScroller#barSize in
        vertScroller#setGeometry {x=rect.x +. rect.w (*-. barSizeW*); y=rect.y; h=rect.h; w=barSizeW};
        horzScroller#setGeometry {x=rect.x; y=rect.y +. rect.h (*-. barSizeH*); h=barSizeH; w=rect.w};
        vertScroller#setRatio (rect.h /. h);
        horzScroller#setRatio (rect.w /. w);

    method ensureVisible (region : rect) =
        let open Float in
        let cgeom = cont#geom in
        let r = Rect.intersection region cgeom in
        let ox = self#offsetX in
        let oy = self#offsetY in
        let v = { self#geom with x=ox; y=oy; } in
        if not (Rect.inside r v) then begin
            let oy1 = Float.abs ((r.y +. r.h) -. (v.y +. v.h))
            and oy2 = Float.abs (v.y -. r.y)
            and ox1 = Float.abs ((r.x +. r.w) -. (v.x +. v.w))
            and ox2 = Float.abs (v.x -. r.x) in
            let vdenom = max (cont#geom.h -. self#geom.h) 1. in
            let wdenom = max (cont#geom.w -. self#geom.w) 1. in
            let vpos = (if oy1 < oy2 then (oy +. oy1) else (oy -. oy2)) /. vdenom in
            let wpos = (if ox1 < ox2 then (ox +. ox1) else (ox -. ox2)) /. wdenom in
            vertScroller#setPosition vpos;
            horzScroller#setPosition wpos;
        end

    (* Can never be < 0 or > cont.w - rect.w *)
    method offsetX =
        horzScroller#position *. (cont#geom.w -. self#geom.w)

    (* Can never be < 0 or > cont.h - rect.h *)
    method offsetY =
        vertScroller#position *. (cont#geom.h -. self#geom.h)

    method sizeHint cr =
        cont#sizeHint cr

    method paint cr =
        (* Create a cairo surface *)
        Cairo.save cr;
        let module CI = Cairo.Image in
        let r : rect = cont#geom in
        let w, h = Util.iceil r.w, Util.iceil r.h in
        let img = CI.create CI.ARGB32 w h in
        let cri = Cairo.create img in
        let ox = self#offsetX
        and oy = self#offsetY in
        Cairo.translate cri (-.ox) (-.oy);
        cont#beginPaint cri;
        let x = rect.x in
        let y = rect.y in
        Cairo.set_source_surface cr img x y;
        Cairo.paint cr;
        Cairo.restore cr;
        vertScroller#beginPaint cr;
        horzScroller#beginPaint cr;
        (*
        Cairo.set_source_rgba cr 0.25 0.1 0.0 1.0;
        Cairo.rectangle cr rect.x rect.y rect.w rect.h;
        Cairo.fill cr;
        *)
end
*)
