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

type cool_events = OK1
                 | OK2

class cool1 = object 
    val table : (cool_events, (cool_events -> unit) list) HP.t = HP.create()

    (* TODO can wrap FNs in <object> to make the comparable *)
    method addFn evt fn =
        match HP.find table evt with
        | None -> HP.set table evt [fn]
        | Some lst -> HP.set table evt (fn :: lst)

    method removeFn () = ()
    
    method handleEvent evt = 
        match HP.find table evt with
        | None -> ()
        | Some lst -> List.iter lst (fun f -> f evt)
end

type doIt = < exec : unit -> unit >

type combinator = 
    | Single of cool1 * cool_events * (cool_events -> combinator)
    | Repeat of cool1 * cool_events * (cool_events -> combinator)
    | Select of combinator list
    | Ignore

let single_fn obj event fn =
    obj#addFn event (fun v ->
        obj#removeFn ();
        fn v;
    )
;;

let select_fn fns =
    let remove_all () = List.iter fns (fun (obj, event, fn) ->
        obj#removeFn ();
    ) in
    List.iter fns (fun (obj, event, fn) ->
        obj#addFn event (fun v ->
            remove_all();
            fn v;
        )
    )
;;

let repeat_fn obj event fn =
    obj#addFn event (fun v ->
        obj#removeFn();
        fn v;
        (*obj#addFn event ...*)
    )

let _ =
    let c1 = new cool1 in
    single_fn c1 OK1 (fun _ ->
        select_fn [
        ]
    )
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
