type byte_array =
    (int, Bigarray.int8_unsigned_elt, Bigarray.c_layout) Bigarray.Array1.t

module ByteArray = struct
    include Bigarray.Array1
    
    let create = create Bigarray.int8_unsigned Bigarray.c_layout
    let of_array = of_array Bigarray.int8_unsigned Bigarray.c_layout
    let of_list = Fn.compose of_array Array.of_list
    let empty = create 0
end

