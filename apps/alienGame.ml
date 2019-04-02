type team = Aliens
          | Humans

type sector = Silent
            | Dangerous
            | Human
            | Alien
            | EscapeHatch

type item = Attack
          | Teleport
          | Adrenaline
          | Sedatives
          | Defense
          | Spotlight
          | Clone
          | Sensor
          | Mutation
          | Cat

type dangerous_sector = Noise_In_Your_Sector
                      | Noise_In_Any_Sector
                      | Silence
                      | Item of item (* Acts as a silence card *)

type escape_hatch = Good
                  | Damaged

type character = Captain
               | Pilot
               | Psychologist
               | Soldier
               | Executive_Officer
               | CoPilot
               | Engineer
               | Medic
               (* Aliens *)
               | Blink
               | Silent
               | Surge
               | Brute
               | Invisible
               | Lurking
               | Fast
               | Pyshic
               | Alien

let is_alien = function
    | Blink
    | Silent
    | Surge
    | Brute
    | Invisible
    | Lurking
    | Fast
    | Pyshic
    | Alien -> true
    | _ -> false
;;

let is_human = Fn.compose not is_alien

type entity = {
    id : int;
    character : character;
    name : string;
    moves : int list;
    items : item list;
    used_items : item list;
    move_amount : int;
}

(* What to do about all the character modifiers, would be
 * nice to add them as extras so that the rules don't have
 * to be hard coded *)

type loc = int

type board = {
    cells : sector Array.t Array.t;
}

type game_event =
    | Move of {from : loc; to_ : loc}
    | DrawItem of item * entity
    | UseItem of item * entity
    | RevealItem of item * entity
    | DiscardSectorCard of dangerous_sector
    | Revealed of entity
    | Escaped of entity * loc 
    | Attack of entity * loc
    | Killed of {
        attacker : entity;
        victims : entity list;
        loc : loc;
    }
    | PlayerTurnedAlien of entity
    | PlayerEliminated of entity
    | Win of team * entity list

type rule = {
    validate : game -> game;
}

and game = {
    max_turns : int;
    rules : rule list;
    round : int;
    players : entity list;
    eliminated_players : entity list;
    item_deck : item list;
    dangerous_deck : dangerous_sector list;
    dangerous_discard : dangerous_sector list;
}

(* Turn phase -
 * Use item (at any time during turn, even same turn it's drawn, can use as many as desired)
 * Move (unless ability says otherwise)
 * Draw sector card (or attack if alien, if attacking, don't draw card)
 *)

module Action = struct
    type t = Move of loc
           | Item of item
end

type turn_callback = game -> Action.t

let one_turn game callback = game

