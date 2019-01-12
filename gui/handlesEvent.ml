type ('a, 'b) event = <event : 'a; arg : 'b>
type ('a, 'b) callable = <call : ('a, 'b) event -> unit>

module HP = Hashtbl.Poly

let mkEvent e a = object
    method event = e
    method arg = a
end

type ('a, 'b) event_store = <
    addFn : 'a -> ('a, 'b) callable -> unit;
    removeFn : ('a, 'b) callable -> unit;
    handle : ('a, 'b) event -> unit;
>

let create () = object(self)
    val table : ('a, (('a, 'b) callable) list) HP.t = HP.create()
    val rev_table : ((('a, 'b) callable), 'a list) HP.t = HP.create()

    val mutable any_list : ([>`Any], 'b) callable list = []

    method addFn (evt : 'a) (obj : ('a, 'b) callable) =
        match evt with
        | `Any -> any_list <- obj :: List.filter any_list (fun e -> Poly.(e <> obj))
        | _ -> begin
            (match HP.find table evt with
            | None -> HP.set table evt [obj]
            | Some lst -> HP.set table evt (obj :: lst)
            );
            match HP.find rev_table obj with
            | None -> HP.set rev_table obj [evt]
            | Some lst -> HP.set rev_table obj (evt :: List.filter lst (fun e -> Poly.(<>) e evt))
        end

    method removeFn (obj : ('a, 'b) callable) : unit =
        any_list <- List.filter any_list (fun e -> Poly.(e <> obj));
        match HP.find rev_table obj with
        | None -> ()
        | Some lst ->
            List.iter lst (fun event ->
                match HP.find table event with
                | None -> ()
                | Some lst -> HP.set table event (List.filter lst (fun e -> Poly.(<>) e obj))
            )

    method handle (evt : ('a, 'b) event) : unit =
        List.iter any_list (fun obj -> obj#call evt);
        match HP.find table evt#event with
        | None -> ()
        | Some lst -> List.iter lst (fun obj ->
            obj#call evt
        )
end

type ('a, 'b) handles_event = <
    events : ('a, 'b) event_store
>

let mk obj evt fn = object
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

let rec repeat obj evt fn =
    mk obj evt (fun arg ->
        fn arg;
        [repeat obj evt fn]
    )
;;

let mkl obj evt fn = [mk obj evt fn]

let finishl obj evt fn = [
    mk obj evt (fun e -> fn e; [])
]

let repeat_all obj event_list fn =
    List.iter event_list (fun event ->
        (repeat obj event fn)#attach
    )
;;

let finish obj evt fn = mk obj evt (fun e -> fn e; [])

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

