type 'a callable = <call : 'a -> unit>

module HP = Hashtbl.Poly

class ['a] eventHandler = object 
    val table : ('a, 'a callable list) HP.t = HP.create()
    val rev_table : ('a callable, 'a list) HP.t = HP.create()

    method addFn evt fn =
        (match HP.find table evt with
        | None -> HP.set table evt [fn]
        | Some lst -> HP.set table evt (fn :: lst)
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
    mk obj evt (fun evt ->
        fn evt;
        [repeat obj evt fn]
    )
;;

let mkl obj evt fn = [mk obj evt fn]

let finishl obj evt fn = [
    mk obj evt (fun e -> fn e; [])
]

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

