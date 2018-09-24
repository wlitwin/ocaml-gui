class ['a] queue = object(self)
    val mutable head : 'a Dllist.node_t option = None
    val mutable tail : 'a Dllist.node_t option = None
    val mutable length = 0

    method length = length

    method enque (obj : 'a) =
        length <- length + 1;
        match tail with
        | None ->
            let node = Dllist.create obj in
            head <- Some node;
            tail <- Some node;
        | Some tl -> tail <- Some (Dllist.append tl obj)

    method front =
        match head with
        | Some head -> Some (Dllist.get head)
        | None -> None

    method head = head

    method deque =
        if length <= 0 then None
        else if length = 1 then begin
            length <- length - 1;
            match head with
            | Some hd ->
                head <- None;
                tail <- None;
                Some (Dllist.get hd)
            | None -> None
        end else begin
            length <- length - 1;
            match head with
            | Some hd ->
                head <- Some (Dllist.drop hd);
                Some (Dllist.get hd) 
            | None -> None
        end
end

