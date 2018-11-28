module P = Graph.Persistent.Digraph
module PL = P.ConcreteBidirectionalLabeled

open Control

type vertex = {
    name : string;
    control : control;
}
type edge = vertex * vertex

module Compare = struct
    type t = vertex
    let compare = compare
    let hash = Hashtbl.hash
    let equal = ( = )
end

module Ordered = struct
    type t = string
    let compare = compare
    let default = "none"
end

module PLC = PL(Compare)(Ordered)
module SE = Set.Make(struct
    type t = PLC.edge
    let compare = PLC.E.compare
end)

module SV = Set.Make(struct
    type t = PLC.vertex
    let compare = PLC.V.compare
end)

