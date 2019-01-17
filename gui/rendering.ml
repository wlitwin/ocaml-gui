(* TODO figure out how to use JaneStreet's Incremental library for this *)

type draw_type = Stroke
               | Fill

type text_data = {
    font : Font.t;
    text : string;
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
             | AddZ of int

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

let fill_text text font color pos =
    {
        pos;
        draw_type = Fill;
        shape_type = Text {
            font;
            text;
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
    let rec traverse (z_index, pos : int * Pos.t) node =
        let z_index = z_index + node#zIndex in
        match node#content with
        | Translate amt -> List.iter node#children (traverse (z_index, Pos.add pos amt))
        | Group groups ->
            List.iter groups (fun (idx, prims) -> 
                let z_index = z_index + idx in
                add_prims_by_z (z_index, pos, prims);
            );
            List.iter node#children (traverse (z_index, pos)) 
        | AddZ amt -> List.iter node#children (traverse (z_index+amt, pos))
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

let draw_tree cr tree =
    (*Stdio.printf "RENDER LIST IS %d IN LENGTH\n" (List.length tree);*)
    Array.iter tree (fun lst ->
        List.iter lst (function
            | TextList {draw_type=Stroke} -> failwith "Unsupported stroke text"
            | TextList {draw_type=Fill; font; color; text} ->
                Util.timeit "TEXT LIST TIME" (fun _ ->
                    Stdio.printf "TEXT LIST color %.2f %.2f %.2f %d\n" color.r color.g color.b (List.length text);
                    Graphics.set_color cr color;
                    Graphics.set_font_info cr font;
                    List.iter text (fun (pos, text) ->
                        Graphics.draw_text_ cr pos text
                        (*Graphics.draw_text cr font Rect.{x=pos.x; y=pos.y; w=0.; h=0.} text*)
                    )
                )
            | RectangleList {draw_type=Fill; color; rects} ->
                Util.timeit "RECT LIST [F] TIME" (fun _ ->
                    Stdio.printf "RECT LIST (F) %.2f %.2f %.2f %d\n" color.r color.g color.b (List.length rects);
                    Graphics.set_color cr color;
                    List.iter rects (fun r ->
                        Graphics.rectangle cr r;
                        Graphics.fill cr;
                    )
                )
            | RectangleList {draw_type=Stroke; color; rects} ->
                Util.timeit "RECT LIST [S] TIME" (fun _ ->
                    Stdio.printf "RECT LIST (S) %.2f %.2f %.2f %d\n" color.r color.g color.b (List.length rects);
                    Graphics.set_color cr color;
                    List.iter rects (fun r ->
                        Graphics.rectangle cr r;
                        Graphics.stroke cr;
                    )
                )
    ))
;;

let print_tree tree =
    let rec print idnt node =
        let pad = String.make idnt ' ' in
        begin match node#content with
        | Group lst -> 
            let items = List.fold lst ~init:0 ~f:(fun acc (_, lst) ->
                List.length lst + acc
            ) in
            Stdio.printf "%sGROUP [%d items]" pad items
        | Translate _ -> Stdio.printf "%sTranslate" pad
        | AddZ amt -> Stdio.printf "%sAddZ %d" pad amt
        | Primitive {shape_type=Rectangle _} -> Stdio.printf "%sPrim[Rect]" pad
        | Primitive {shape_type=Text _} -> Stdio.printf "%sPrim[Text]" pad
        end;
        Stdio.printf " -- with children %d\n" (List.length node#children);
        List.iter node#children (print (idnt+2))
    in
    print 0 tree;
;;

let draw cr tree =
    print_tree tree;
    let tree, tree_time = Util.time (fun _ ->
        sort_tree tree
    ) in
    let _, draw_time = Util.time (fun _ ->
        draw_tree cr tree
    ) in
    tree_time, draw_time
;;

class nodeObject renderer = object(self)
    val mutable content = Group [0, []]
    val mutable children : nodeObject list = []
    val renderer = renderer
    val mutable z_index = 0

    method rect = Rect.empty

    method zIndex = z_index
    method setZIndex z = z_index <- z

    method content = content
    method setContent c : unit = 
        content <- c;
        renderer#update

    method children = children

    method attach obj =
        self#detach obj;
        children <- obj :: children

    method detach obj =
        children <- List.filter children (fun o -> not (phys_equal obj o))
end

class groupObject renderer = object(self)
    inherit nodeObject renderer
end

class primObject renderer = object(self)
    inherit nodeObject renderer

    method private prim =
        match self#content with
        | Primitive prim -> prim
        | _ -> failwith "Invalid"

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

    val mutable size = Size.zero

    method private text_info =
        match self#prim.shape_type with
        | Text ti -> ti
        | _ -> failwith "Invalid"

    method text = self#text_info.text
    method font = self#text_info.font

    method fontExtents : Font.font_extents =
        renderer#fontExtents self#font

    method! rect =
        let prim = self#prim in
        Rect.{x=prim.pos.x; y=prim.pos.y; w=size.w; h=size.h}

    method size = size

    method setText text =
        let ti = self#text_info in
        size <- renderer#measureText(ti.font, ti.text);
        self#setContent (Primitive {self#prim with shape_type=Text {ti with text}})

    method setFont font =
        self#setContent (Primitive {self#prim with shape_type=Text {self#text_info with font}})

    initializer
        self#setContent (Primitive {
            pos=Pos.zero;
            draw_type=Fill;
            shape_type=Text {text=""; font=Font.default_font};
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
    
    initializer
        self#setContent (Primitive {
            pos=Pos.zero;
            draw_type=Fill;
            shape_type=Rectangle Size.{w=100.; h=100.};
            color=Color.red;
        })
end

class renderer = object(self)
    val mutable root = new nodeObject (object method update = () end)
    val mutable requestDraw : unit -> unit = fun _ -> ()
    val mutable immediateUpdates = true

    method setRoot r = root <- r
    method root = root

    method setImmediateUpdates p = immediateUpdates <- p
    method pause = immediateUpdates <- false
    method resume = immediateUpdates <- true

    method measureText (font, text) =
        let metrics = Graphics.measure_text_no_context(font, text) in
        Size.{w=metrics.width; h=metrics.ascent+.metrics.descent}

    method fontExtents font : Font.font_extents =
        Graphics.font_extents_no_context font

    method createTextObject = new textObject self
    method createRectObject = new rectObject self
    method createGroupObject = new groupObject self

    method update = requestDraw()

    method setRequestDraw f =
        requestDraw <- f

    val stat_font = Font.{
        size = 20.;
        font = "Ubuntu mono";
        weight = Bold;
    }

    val mutable size = Size.zero
    method setSize s = size <- s

    method render cr =
        if immediateUpdates then (
            let sort_time, draw_time =  draw cr root in
            let text = Printf.sprintf "S %.3fms D %.3fms" sort_time draw_time in
            Graphics.set_font_info cr stat_font;
            Graphics.draw_text_ cr Pos.{x=0.; y=size.h -. 2.} text;
        )
end

(*
let _ =
    let renderer = new renderer in
    let top = new nodeObject in
    let text = new textObject in
    let rect = new rectObject in
    ()
    *)
