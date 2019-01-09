(*type z = Z : z
type 'n s = S : 'n -> 'n s

type (_, _) llist =
    | Nil  : ('a, 'a) llist
    | Cons : 'a * ('b, 'c) llist -> (('a * 'b, 'c) llist)

let x = S (S (10, 20))
let y = Cons (10., Cons (10, Nil))
let z = Cons (true, Cons ((`OK, 10), Cons ((`OK2, 20.), Nil)))
let q = Cons (y, z)

type ('ty,'v) t =
  | LNil : ('v, 'v) t
  | LCons : 'a * ('ty, 'v) t -> ('a -> 'ty, 'v) t

let rec append
  : type ty1 ty2 v.
    (ty1, ty2) t ->
    (ty2, v  ) t ->
    (ty1, v  ) t
  = fun l1 l2 -> match l1 with
  | LNil -> l2
  | LCons (h, t) -> LCons (h, append t l2)

let rec len : type a b. (a, b) llist -> int =
    function
    | Nil -> 0
    | Cons (_, t) -> 1 + len t
;;

let rec same_len : type a b c d. (a, b) llist -> (c, d) llist -> bool =
    fun l1 l2 ->
        match l1, l2 with
        | Nil, Nil -> true
        | Cons _, Nil -> false
        | Nil, Cons _ -> false
        | Cons (h1, t1), Cons (h2, t2) -> same_len t1 t2
;;

let rec append : type a b c. (a, b) llist -> (b, c) llist -> (a, c) llist =
    fun l1 l2 -> match l1 with
    | Nil -> l2
    | Cons (h, t) -> Cons (h, append t l2)

let l1 = len y
let l2 = len z
let _ =
    let new_lst = append y z in
    Stdio.printf "l1 %d, l2 %d, same_len %b new_lst %d\n%!" l1 l2 (same_len y z) (len new_lst)

type _ same =
    | Arg : 'a * ('a -> 'b -> unit) -> _ same
    *)

        (*
    = fun l1 l2 ->
        match l1 with
        | Nil -> l2
        | Cons (h, t) -> Cons (h, append t l2)
        *)

let _ =
    Platform.Windowing.run (fun context ->
        let app = new Application.application context Rect.{w=400.; h=400.} in
        (*let mainWidget = new Login.loginWidget app in*)
        (*let mainWidget = new Demo.main app in*)
        (*let mainWidget = (*new FileBrowser.fileBrowser app in*) new Widget.basicWidget app in*)
        let mainWidget = new Simple.simple app in
        app#setWidget (mainWidget :> Widget.basicWidget);
        app#main
    )
