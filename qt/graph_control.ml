open Label
open Scroll
open GraphTypes

module HV = Dot.XDotInput.HV
module HE = Dot.XDotInput.HE

let debug_graph = false
let debug f = if debug_graph then f() else ()

let draw_command cr =
    let open Dgraph.XDotDraw in
    function 
    | Filled_ellipse (p, w, h) ->
            debug (fun _ -> Printf.printf "  Filled ellipse\n");
            ()
    | Unfilled_ellipse (p, w, h) ->
            debug (fun _ -> Printf.printf "  Unfilled ellipse\n");
            ()
    | Filled_polygon parr ->
            debug (fun _ -> Printf.printf "  Filled polygon\n");
            Cairo.set_source_rgba cr 0. 0. 0. 1.;
            Cairo.Path.sub cr;
            let x, y = parr.(0) in
            Cairo.move_to cr x y;
            for i=1 to Array.length parr - 1 do
                let x, y = parr.(i) in
                Cairo.line_to cr x y
            done;
            Cairo.Path.close cr;
            Cairo.fill cr;
            ()
    | Unfilled_polygon parr ->
            debug (fun _ -> Printf.printf "  Unfilled polygon\n");
            ()
    | Polyline parr ->
            debug (fun _ -> Printf.printf "  Polyline\n");
            ()
    | Bspline parr ->
            (* How to draw bsplines with more control points!? *)
            (* For self loops, may have more control points *)
            debug (fun _ ->
                Printf.printf "  Bspline\n";
                Array.iter (fun (x, y) ->
                    Printf.printf "    %f %f\n" x y
                ) parr);
            Cairo.set_source_rgba cr 0. 0. 0. 1.;
            let len = Array.length parr / 3 - 1 in
            for i=0 to len do
                let i = i*3 in
                let x1, y1 = parr.(i) in
                let x2, y2 = parr.(i+1)
                and x3, y3 = parr.(i+2)
                and x4, y4 = parr.(i+3) in
                Cairo.move_to cr x1 y1;
                Cairo.curve_to cr x2 y2 x3 y3 x4 y4;
                Cairo.stroke cr;
            done;
    | Filled_bspline parr ->
            debug (fun _ -> Printf.printf "  Filled bspline\n");
            ()
    | Text (p, align, w, text) ->
            debug (fun _ -> Printf.printf "  Text\n");
            ()
    | Fill_color str ->
            debug (fun _ -> Printf.printf "  Fill color\n");
            ()
    | Pen_color str ->
            debug (fun _ -> Printf.printf "  Pen color\n");
            ()
    | Font (size, name) ->
            debug (fun _ -> Printf.printf "  Font\n");
            ()
    | Style styles ->
            debug (fun _ -> Printf.printf "  Style\n");
            ()

class graph_area app =
object(self)
    inherit control app as super
    
    val mutable graph = PLC.empty
    val mutable bbox = Rect.{w=400.;h=200.}
    val mutable graph_layout : Dot.XDotInput.graph_layout option = None

    method setGraph g =
        graph <- g;
        self#relayoutNoCr 

    method! relayout cr =
        debug (fun _ -> Printf.printf "=========\n\n\n\n GRAPH CONTROL IS LAYING OUT\n\n\n===========================\n");
        graph_layout <- Some (Dot.xdot_layout graph); 
        bbox <-
            HV.fold Dgraph.XDot.(fun vertex node_layout bbox ->
                let ((x, y), (w, h)) = node_layout.n_bbox in
                let w = w *. 72.0
                and h = h *. 72.0 in
                let r = Rect.{x=rect.x +. x-.w*.0.5;y=rect.y +. y-.h*.0.5;w;h} in
                vertex.control#setGeometry r;
                vertex.control#relayout cr;
                Rect.union r bbox
            ) (Util.some graph_layout).vertex_layouts Rect.empty_rect
            |> Rect.size_of_rect;
        HE.iter Dgraph.XDot.(fun (e1, lbl, e2) edge_layout ->
            ()
        ) (Util.some graph_layout).edge_layouts;
        debug (fun _ -> Printf.printf "BBOX IS %f %f\n" bbox.w bbox.h)

    method! sizeHint cr = 
        (*self#relayout cr;*)
        (*super#relayoutNoCr;*)
        debug (fun _ -> Printf.printf "SETTING BBOX TO %f %f\n" bbox.w bbox.h);
        bbox

    method! setGeometry r =
        debug (fun _ -> Printf.printf "GETTING GEOMETRY %f %f %f %f\n" r.x r.y r.w r.h);
        super#setGeometry r

    method! paint cr =
        debug (fun _ -> Printf.printf "GRAPH CONTROL IS PAINTING\n");
        match graph_layout with
        | Some graph_layout ->
            HV.iter (fun vertex _ ->
                let r = vertex.control#geom in
                debug (fun _ -> Printf.printf "Child loc %f %f %f %f\n" r.x r.y r.w r.h);
                vertex.control#beginPaint cr
            ) graph_layout.vertex_layouts;
            (* Need to translate everything *)
            Cairo.translate cr rect.x rect.y;
            HE.iter (fun (e1, lbl, e2) edge_layout ->
                let open Dgraph.XDot in
                debug (fun _ -> Printf.printf "e_draw\n");
                List.iter (draw_command cr) edge_layout.e_draw;
                debug (fun _ -> Printf.printf "l_draw\n");
                List.iter (draw_command cr) edge_layout.e_ldraw;
                debug (fun _ -> Printf.printf "h_draw\n");
                List.iter (draw_command cr) edge_layout.e_hdraw;
                debug (fun _ -> Printf.printf "t_draw\n");
                List.iter (draw_command cr) edge_layout.e_tdraw;
                debug (fun _ -> Printf.printf "hl_draw\n");
                List.iter (draw_command cr) edge_layout.e_hldraw;
                debug (fun _ -> Printf.printf "tl_draw\n");
                List.iter (draw_command cr) edge_layout.e_tldraw;
            ) graph_layout.edge_layouts
        | None -> ()

end

class graph app =
    let graph_area = new graph_area app in
object(self)
    inherit scrollArea app (graph_area :> control) as super

    val graph_area = graph_area

    method setGraph = graph_area#setGraph
    
end
