class virtual ['a] event = object
    val virtual evt : [> `Done]
    val virtual source : 'a
end

module HP = Hashtbl.Poly

type evt = ..
type evt += Any

type 'a event_state = {
    key : 'a;
    event : evt;
    behavior : 'a event_type;
}
and 'a event_type = Single of (unit -> 'a event_state)
                  | Repeated of (unit -> 'a event_state)
                  | SelectGroup of 'a event_state list
                  | Ignore

class eventHandler =
object(self)
    val table : ((eventHandler * evt), eventHandler event_state) HP.t = HP.create()

    method ignore = {key=(self:>eventHandler); event=Any; behavior=Ignore}

    method single (obj, event) func =
        {key=(obj:>eventHandler); event; behavior=Single func}

    method repeat (obj, event) func =
        {key=(obj:>eventHandler); event; behavior=Repeated func}

    method select lst =
        {key=(self:>eventHandler); event=Any; behavior=SelectGroup lst}

    method setReal evt value =
        HP.set table evt value

    method set (value : eventHandler event_state) =
        match value.behavior with
        | Single _ -> value.key#setReal (value.key, value.event) value
        | Repeated _ -> value.key#setReal (value.key, value.event) value
        | SelectGroup lst ->
            List.iter lst (fun v ->
                v.key#setReal (v.key, v.event) v
            )
        | Ignore -> ()

    method remove key =
        HP.remove table key

    method mkEvt (evt : evt) =
        (self :> eventHandler), evt

    method handleEvent evt =
        let key = (self :> eventHandler), evt in
        match HP.find table key with
        | None -> ()
        | Some event_state ->
            match event_state.behavior with
            | Single fn -> 
                HP.remove table (event_state.key, event_state.event);
                self#set (fn())
            | Repeated fn ->
                self#set (fn())
            | SelectGroup lst ->
                ()
end

let upcast item = (item :> eventHandler)
