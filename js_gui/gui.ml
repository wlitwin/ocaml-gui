module Html = Dom_html 

let doc = Html.window##.document

let create_div () = 
   let div = Html.createDiv doc in 
   div##.style##.width := Js.string "500px";
   div##.style##.overflowX := Js.string "scroll";
   div 


let create_canvas () =
   let canvas = Dom_html.createCanvas doc in
   canvas##.width  := 900;
   canvas##.height := 900;
   canvas
;;

let get_main () =
    Html.window##.document##.body
;;

let start _ =
  let main = get_main () in 
  let wrapper = create_div () in 
  let canvas = create_canvas () in 
  Dom.appendChild wrapper canvas; 
  Dom.appendChild main wrapper;
  Js._false in

Dom_html.window##.onload := Dom_html.handler start
