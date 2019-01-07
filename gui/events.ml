open Events3
open Events2

type evt += Event1
class obj1 = object(self)
    inherit eventHandler

    method onEvent1 =
        (upcast self, Event1)

    method go =
        self#handleEvent Event1
end

type evt += Event3
class obj3 = object(self)
    inherit eventHandler
    
    method onEvent3 = (upcast self, Event3)

    method go =
        self#handleEvent Event3
end

type 'a callable = <call : 'a -> unit>

type ('ty, 'v) attached =
    | End : ('v, 'v) attached
    | Event : ('ty, 'v) attached
    | Cons : 'a * ('ty, 'v) attached -> ('a  -> 'ty, 'v) attached

let ok = Cons (`Resize, Event)
let ok2 = match ok with
    | Cons (`Resize, End) -> Stdio.printf "OK"
    | Cons (`Resize, Event) -> Stdio.printf "OK"
    | _ -> ()
;;

module type EventSig = sig
    type t
    type param
    val event : t
    val create : param -> (t * param)
end

module ResizeEvent = struct
    type t = Resize
    type param = Rect.t
    let event = Resize
    let create p =
        Resize, p
end

module PaintEvent = struct
    type t = Paint
    type param = int
    let event = Paint
    let create p =
        Paint, p
end

let _ =
    let p = (module PaintEvent : EventSig) in
    ()
    (*
    Hashtbl.set p ResizeEvent.event (fun _ -> ());
    Hashtbl.set p PaintEvent.event (fun _ -> ());*)
;;

class virtual ['a] baseClass = object 
    val virtual table : ('a, 'a callable list) HP.t
    val virtual rev_table : ('a callable, 'a list) HP.t

    (* TODO can wrap FNs in <object> to make the comparable *)
    method addFn evt fn =
        (match HP.find table evt with
        | None -> HP.set table evt [fn]
        | Some lst -> HP.set table evt (lst @ [fn])
        );
        match HP.find rev_table fn with
        | None -> HP.set rev_table fn [evt]
        | Some lst -> HP.set rev_table fn (evt :: List.filter lst (fun e -> Poly.(<>) e evt))

    method removeFn obj = 
        match HP.find rev_table obj with
        | None -> ()
        | Some lst ->
            List.iter lst (fun evt ->
                match HP.find table evt with
                | None -> failwith "Shouldn't happen"
                | Some lst -> HP.set table evt (List.filter lst (fun o -> Poly.(<>) o obj))
            )
    
    method handleEvent evt = 
        match HP.find table evt with
        | None -> ()
        | Some lst -> List.iter lst (fun obj -> obj#call evt)
end

module type Evt = sig
    type t
end

type cool_events = OK1
                 | OK2


class cool1 = object
    inherit [cool_events] baseClass 

    val table : ('a, 'a callable list) HP.t = HP.create()
    val rev_table : ('a callable, 'a list) HP.t = HP.create()
end

type cooler_events = COOL2
                   | COOL4 

type 'a op = [> `OK | `OK2] as 'a

class virtual ['a] cool2 = object(self)
    inherit ['a] baseClass

    initializer
        (Event_new.repeat self `Paint (fun _ ->
            Stdio.printf "PAINT\n";
        ))#attach
end

type common_events = [`Paint]

class cool3 = object
    inherit [[`Cool | common_events]] cool2

    val table : ('a, 'a callable list) HP.t = HP.create()
    val rev_table : ('a callable, 'a list) HP.t = HP.create()
end

type ('a, 'b) hierarchy = Node of 'a baseClass * 'a * ('a -> ('b, 'b) hierarchy list)

let rec attach = function
    | Node (obj, evt, fn) ->
        obj#addFn evt (object(self)
            method call e =
                obj#removeFn self;
                let res = fn e in
                List.iter res (fun item ->
                    attach item
                )
        end)
;;

let mk obj evt fn = Node (obj, evt, fn)
let mkl obj evt fn = [mk obj evt fn]

let mk2 obj evt fn = object
    method attach =
        obj#addFn evt (object(self)
            method call e =
                obj#removeFn self;
                let res = fn e in
                List.iter res (fun item ->
                    item#attach
                )
        end)
end

let mkl2 obj evt fn = [mk2 obj evt fn]

let finishl obj evt fn = [
    mk2 obj evt (fun e -> fn e; [])
]

let finish obj evt fn = mk2 obj evt (fun e -> fn e; [])

let alternate obj1 evt1 fn1 obj2 evt2 fn2 =
    let rec swap () = mk obj1 evt1 (fun evt ->
        fn1 evt;
        mkl obj2 evt2 (fun evt ->
            fn2 evt;
            [swap ()];
        )
    )
    in
    swap()
;;

let _ =
    let c1 = new cool1 in
    let c2 = new cool3 in
    (*
    c1#addFn OK1 (object method call _ = Stdio.printf "OK CALLED!\n" end);
    let altObj = mk2 c1 OK1 (fun _ ->
        Stdio.printf "C1 OK1\n";
        finishl c2 COOL2 (fun _ ->
            Stdio.printf "C2 COOl2\n";
        )
    ) in
*)
    let rec alt1 () = mk2 c1 OK1 (fun _ ->
        Stdio.printf "WE GOT OK1\n";
        [
            (*finish c1 OK2 (fun _ -> Stdio.printf "END OK2!\n");*)
            alt1 ();
            finish c2 `Cool (fun _ -> Stdio.printf "END COOL2\n");
        ]
    ) in
    (alt1())#attach;
    (*
    let rec cycle () = mk c1 OK1 (fun _ ->
        Stdio.printf "Cycle1\n";
        [cycle()]
    ) in
    let rec test2 () = mk c1 OK1 (fun _ ->
        Stdio.printf "OK1!\n";
        mkl c1 OK2 (fun _ ->
            Stdio.printf "OK2!\n";
            [test2()];
        )
    ) in
    let alt = alternate 
        c1 OK1 (fun _ -> Stdio.printf "alt1 OK1\n")
        c1 OK2 (fun _ -> Stdio.printf "alt2 OK2\n")
    in
    *)
    (*altObj#attach;*)
    c1#handleEvent OK1;
    c1#handleEvent OK1;
    c1#handleEvent OK2;
    c2#handleEvent `Cool;
    c2#handleEvent `Cool;
    c1#handleEvent OK1;
    c2#handleEvent `Cool;
    (*
    attach (cycle());
    attach (test2());
    attach alt;
    c1#handleEvent OK1;
    c1#handleEvent OK1;
    c1#handleEvent OK2;
    c1#handleEvent OK1;
    c1#handleEvent OK1;
    *)
;;

let _ = 
    let o1 = new obj1 in
    let o2 = new obj2 in
    let o3 = new obj3 in
    o1#single o1#onEvent1 (fun _ ->
        Stdio.printf "Cool!\n%!";
        o1#select [
            o3#repeat o3#onEvent3 (fun _ -> Stdio.printf "Event3!"; o3#ignore);
            o2#repeat o2#onEvent2 (fun _ -> Stdio.printf "Event2!"; o2#ignore);
        ];
    ) |> o1#set; 
    (*o2#single o2#onEvent2 (fun _ -> Stdio.printf "Awesome!\n");*)
    o1#go;
    o3#go;
    o2#go;
    o3#go;
    ()
