type key = 
    | A | B | C | D | E | F | G | H | I | J | K
    | L | M | N | O | P | Q | R | S | T | U | V
    | W | X | Y | Z | Key_0 | Key_1 | Key_2 | Key_3 
    | Key_4 | Key_5 | Key_6 | Key_7 | Key_8 | Key_9
    | Bang | At | Pound | Dollar | Percent | Caret
    | Ampersand | Star | LParen | RParen | Minus
    | Underscore | Plus | Equals | LShift | RShift
    | Backspace | LControl

exception Unknown_key_code
let key_code = function
    | A -> 97  | B -> 98  | C -> 99  | D -> 100 | E -> 101
    | F -> 102 | G -> 103 | H -> 104 | I -> 105 | J -> 106
    | K -> 107 | L -> 108 | M -> 109 | N -> 110 | O -> 111
    | P -> 112 | Q -> 113 | R -> 114 | S -> 115 | T -> 116
    | U -> 117 | V -> 118 | W -> 119 | X -> 120 | Y -> 121
    | Z -> 122
    | Key_0|Key_1|Key_2|Key_3|Key_4|Key_5|Key_6|Key_7|Key_8|Key_9|Bang|At|Pound|
Dollar|Percent|Caret|Ampersand|Star|LParen|RParen|Minus|Underscore|Plus|
Equals|LShift|RShift|Backspace|LControl -> raise Unknown_key_code
