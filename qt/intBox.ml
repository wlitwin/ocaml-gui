open TextBox

class intBox app = object(self)
    inherit textBox app as super

    val mutable userKeyFilter : int -> bool = (fun _ -> true)

    method! setKeyFilter f = userKeyFilter <- f

    method number = Int.of_string self#text

    method isValid = try ignore(self#number); true with _ -> false

    initializer begin
        let isIntegerKey = function
            | 0x30 | 0x31 | 0x32 | 0x33 | 0x34
            | 0x35 | 0x36 | 0x37 | 0x38 | 0x39 -> true
            | _ -> false
        in
        super#setKeyFilter (fun k ->
            isIntegerKey k && (userKeyFilter k)
        )
    end
end

