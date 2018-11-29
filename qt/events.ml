open Rect

type event = KeyPress of int
           | KeyRelease of int
           | Resize of size
           | Paint of Cairo.context
           | Layout of Cairo.context

let str_of_event = function
    | KeyPress _ -> "KeyPress"
    | KeyRelease _ -> "KeyRelease"
    | Resize _ -> "Resize"
    | Paint _ -> "Paint"
    | Layout _ -> "Layout"

