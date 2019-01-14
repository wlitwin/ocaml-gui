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

type node = {
    parent : node option;
    content : content;
    children : node list;
}

class nodeObject = object(self)
    val mutable content = Group [0, []]
    val mutable children : nodeObject list = []

    method content = content
    method setContent c = content <- c
    method children = children

    method attach obj =
        self#detach obj;
        children <- obj :: children

    method detach obj =
        children <- List.filter children (fun o -> not (phys_equal obj o))
end

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
        match node#content with
        | Translate amt -> List.iter node#children (traverse (z_index, Pos.add pos amt))
        | Group groups ->
            List.iter groups (fun (idx, prims) -> 
                let z_index = z_index + idx in
                add_prims_by_z (z_index, pos, prims);
                List.iter node#children (traverse (z_index, pos)) 
            )
    in
    traverse (0, Pos.zero) root;
    let keys = 
        HP.keys z_tbl 
        |> List.sort ~compare
    in
    (*let count = List.length keys in*)
    let sorted : render_list list list = List.fold keys ~init:[] ~f:(fun lst z_index ->
        let prims = HP.find_exn z_tbl z_index in
        (group_prims prims) :: lst
    ) in
    List.rev sorted |> List.concat
;;

module Graphics = Platform.Windowing.Graphics

let draw_tree cr tree =
    Stdio.printf "RENDER LIST IS %d IN LENGTH\n" (List.length tree);
    List.iter tree (function
        | TextList {draw_type=Stroke} -> failwith "Unsupported stroke text"
        | TextList {draw_type=Fill; font; color; text} ->
            Graphics.set_color cr color;
            Graphics.set_font_info cr font;
            List.iter text (fun (pos, text) ->
                Graphics.draw_text_ cr pos text
                (*Graphics.draw_text cr font Rect.{x=pos.x; y=pos.y; w=0.; h=0.} text*)
            )
        | RectangleList {draw_type=Fill; color; rects} ->
            Graphics.set_color cr color;
            List.iter rects (fun r ->
                Graphics.rectangle cr r;
                Graphics.fill cr;
            )
        | RectangleList {draw_type=Stroke; color; rects} ->
            Graphics.set_color cr color;
            List.iter rects (fun r ->
                Graphics.rectangle cr r;
                Graphics.stroke cr;
            )
    )
;;

let print_tree tree =
    let rec print node =
        Stdio.printf "NODE with children %d\n" (List.length node#children);
        List.iter node#children print
    in
    print tree;
    tree
;;

let draw cr tree =
    let tree = Util.timeit "Sort time" (fun _ ->
        sort_tree tree
    ) in
    Util.timeit "Draw time" (fun _ ->
        draw_tree cr tree
    )
;;

