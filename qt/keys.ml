type key = 
    | A | B | C | D | E | F | G | H | I | J | K
    | L | M | N | O | P | Q | R | S | T | U | V
    | W | X | Y | Z | Key_0 | Key_1 | Key_2 | Key_3 
    | Key_4 | Key_5 | Key_6 | Key_7 | Key_8 | Key_9
    | Bang | At | Pound | Dollar | Percent | Caret
    | Ampersand | Star | LParen | RParen | Minus
    | Underscore | Plus | Equals | LShift | RShift
    | Backspace | LControl | LSuper | LBracket | RBracket
    | LBrace | RBrace | SemiColon | Colon | Comma
    | LCaret | RCaret | Question | FSlash | BSlash
    | Pipe | Period | BackTick | Tilde | DoubleQuote
    | SingleQuote | Esc | LAlt | Enter

let is_modifier = function
    | LAlt
    | LControl
    | LShift
    | RShift
    | LSuper -> true
    | _ -> false

let is_printable = function
    | LAlt
    | LControl
    | LShift
    | RShift
    | LSuper
    | Esc
    | Enter
    | Backspace -> false
    | _ -> true

exception Unknown_key_code
let key_code = function
    | A -> 97  | B -> 98  | C -> 99  | D -> 100 | E -> 101
    | F -> 102 | G -> 103 | H -> 104 | I -> 105 | J -> 106
    | K -> 107 | L -> 108 | M -> 109 | N -> 110 | O -> 111
    | P -> 112 | Q -> 113 | R -> 114 | S -> 115 | T -> 116
    | U -> 117 | V -> 118 | W -> 119 | X -> 120 | Y -> 121
    | Z -> 122 | LBrace -> 123 | RBrace -> 125
    | Key_0 -> 48
    | Key_1 -> 49
    | Key_2 -> 50
    | Key_3 -> 51
    | Key_4 -> 52
    | Key_5 -> 53
    | Key_6 -> 54
    | Key_7 -> 55
    | Key_8 -> 56
    | Key_9 -> 57
    | Bang -> 33
    | DoubleQuote -> 34
    | SingleQuote -> 39
    | At -> 64
    | Pound -> 35
    | Dollar -> 36
    | Percent -> 37
    | Caret -> 94
    | Ampersand -> 38
    | Star -> 42
    | LParen -> 40
    | RParen -> 41
    | LBracket -> 91
    | RBracket -> 93
    | SemiColon -> 59
    | Colon -> 58
    | Comma -> 44
    | LCaret -> 60
    | RCaret -> 62
    | Question -> 63
    | FSlash -> 47
    | BSlash -> 92
    | Pipe -> 124
    | Period -> 46
    | BackTick -> 96
    | Tilde -> 126
    | Minus -> 45
    | Underscore -> 95
    | Plus -> 43
    | Equals -> 61
    | Esc -> 0xff1b
    | LShift -> 0xffe2
    | RShift -> 0xffe2
    | Backspace -> 0xff08
    | LControl -> 0xffe3
    | LAlt -> 0xffe9
    | Enter -> 0xff0d
    | LSuper  -> 0xffeb

exception Unknown_key_int of int
let of_code = function
    | 33 -> Bang
    | 34 -> DoubleQuote
    | 35 -> Pound
    | 36 -> Dollar
    | 37 -> Percent
    | 38 -> Ampersand
    | 39 -> SingleQuote
    | 40 -> LParen
    | 41 -> RParen
    | 42 -> Star
    | 43 -> Plus
    | 44 -> Comma
    | 45 -> Minus
    | 46 -> Period
    | 47 -> FSlash
    | 48 -> Key_0
    | 49 -> Key_1
    | 50 -> Key_2
    | 51 -> Key_3
    | 52 -> Key_4
    | 53 -> Key_5
    | 54 -> Key_6
    | 55 -> Key_7
    | 56 -> Key_8
    | 57 -> Key_9
    | 58 -> Colon
    | 59 -> SemiColon
    | 60 -> LCaret
    | 61 -> Equals
    | 62 -> RCaret
    | 63 -> Question
    | 64 -> At
    | 91 -> LBracket
    | 92 -> BSlash
    | 93 -> RBracket
    | 94 -> Caret
    | 95 -> Underscore
    | 96 -> BackTick
    | 97 -> A
    | 98 -> B
    | 99 -> C
    | 100 -> D
    | 101 -> E
    | 102 -> F
    | 103 -> G
    | 104 -> H
    | 105 -> I
    | 106 -> J
    | 107 -> K
    | 108 -> L
    | 109 -> M
    | 110 -> N
    | 111 -> O
    | 112 -> P
    | 113 -> Q
    | 114 -> R
    | 115 -> S
    | 116 -> T
    | 117 -> U
    | 118 -> V
    | 119 -> W
    | 120 -> X
    | 121 -> Y
    | 122 -> Z
    | 123 -> LBrace
    | 124 -> Pipe
    | 125 -> RBrace
    | 126 -> Tilde
    | 0xff08 -> Backspace
    | 0xffe2 -> LShift
    | 0xffe3 -> LControl
    | 0xff1b -> Esc
    | 0xffeb -> LSuper
    | 0xffe9 -> LAlt
    | 0xff0d -> Enter
    | code -> raise (Unknown_key_int code)

exception No_string_for_key of key
let to_string = function
    | A -> "a"
    | B -> "b"
    | C -> "c"
    | D -> "d"
    | E -> "e"
    | F -> "f"
    | G -> "g"
    | H -> "h"
    | I -> "i"
    | J -> "j"
    | K -> "k"
    | L -> "l"
    | M -> "m"
    | N -> "n"
    | O -> "o"
    | P -> "p"
    | Q -> "q"
    | R -> "r"
    | S -> "s"
    | T -> "t"
    | U -> "u"
    | V -> "v"
    | W -> "w"
    | X -> "x"
    | Y -> "y"
    | Z -> "z"
    | Key_0 -> "0"
    | Key_1 -> "1"
    | Key_2 -> "2"
    | Key_3 -> "3"
    | Key_4 -> "4"
    | Key_5 -> "5"
    | Key_6 -> "6"
    | Key_7 -> "7"
    | Key_8 -> "8"
    | Key_9 -> "9"
    | Bang -> "!"
    | At -> "@"
    | Pound -> "#"
    | Dollar -> "$"
    | Percent -> "%"
    | Caret -> "^" 
    | Ampersand -> "&"
    | Star -> "*"
    | LParen -> "("
    | RParen -> ")"
    | LBracket -> "["
    | RBracket -> "]"
    | LBrace -> "{"
    | RBrace -> "}"
    | Minus -> "-"
    | Underscore -> "_"
    | Plus -> "+"
    | Equals -> "="
    | SemiColon -> ";"
    | Colon -> ":"
    | Comma -> ","
    | LCaret -> "<"
    | RCaret -> ">"
    | Question -> "?"
    | FSlash -> "/"
    | BSlash -> "\\"
    | Pipe -> "|"
    | Period -> "."
    | BackTick -> "`"
    | Tilde -> "~"
    | DoubleQuote -> "\""
    | SingleQuote -> "'"
    | Esc | LAlt | Enter
    | LShift | RShift 
    | LControl | LSuper 
    | Backspace as key -> raise (No_string_for_key key)
