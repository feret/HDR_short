
open Config
open Geometry
open Signature_toy

let empty = Signature_toy.signature_toy


let show_binding_type = false

(* CONTACT MAP *)
let
  [
    cm_p,[cm_g,_;cm_d,_]
  ],
  remanent
  =
  add_in_graph
    [
      protein,0.,0.,[],[site_gauche,[],[Internal_state (gauche_u,[Direction (of_degree 260.)]);Internal_state (gauche_p,[Direction (of_degree 280.)])];
                        site_droit,[],[Internal_state (droit_u,[Direction (of_degree 80.)]);Internal_state (droit_p,[Direction (of_degree 100.)])]]]
    empty





let contact_map = remanent

type state = U | P | Unknown
let _ = dump "contact_map_toy.ladot" contact_map

let string_of_state =
  function
  | U -> "u"
  | P -> "p"
  | Unknown -> "w"

let make_protein ~d ~g =
  let _, protein =
    add_in_graph
      [protein,0.,0.,[],[site_gauche,[],
                         (match g with
                           Unknown -> []
                         | P ->
                           [Internal_state (gauche_p,[])]
                         | U ->
                           [Internal_state (gauche_u,[])]);
                           site_droit,[],
                           match d with
                             Unknown -> []
                           | P -> [Internal_state (droit_p,[])]
                           | U -> [Internal_state (droit_u,[])];
                        ]]
      empty
  in
  let title = "toy_"^(string_of_state g)^"_"^(string_of_state d)^".ladot"
  in
  dump title protein

let () = make_protein ~d:U ~g:U
let () = make_protein ~d:U ~g:P
let () = make_protein ~d:P ~g:U
let () = make_protein ~d:P ~g:P
let () = make_protein ~d:U ~g:Unknown
let () = make_protein ~d:Unknown ~g:P
let () = make_protein ~d:P ~g:Unknown
let () = make_protein ~d:Unknown ~g:U

type side = G | D
let string_of_side =
  function
  | G -> "g"
  | D -> "d"

let make_rule ~side ~action ~other =
  let [agent,_], protein =
    add_in_graph
      [protein,0.,0.,[],[]]
      empty
  in
  let protein =
    insert_text_here "." 0.7 0. ~directives:[Width 0.0001;Height 0.0001;FontColor "white"] protein
  in
  let protein =
    insert_text_here "." (-0.7) 0. ~directives:[Width 0.0001;Height 0.0001;FontColor "white"] protein
  in
  let protein =
    match side, other with
    | _, Unknown -> protein
    | D, P ->
      let site, protein =
        add_site agent site_gauche protein
      in
      snd (add_internal_state site  gauche_p protein)
    | G, P ->
      let site, protein =
        add_site agent site_droit protein
      in
      snd (add_internal_state site droit_p protein)
    | D, U ->
      let site, protein =
        add_site agent site_gauche protein
      in
      snd (add_internal_state site  gauche_u protein)
    | G, U ->
      let site, protein =
        add_site agent site_droit protein
      in
      snd (add_internal_state site droit_u protein)
  in
  let phospho_d =
    (function protein ->
       ([],[],[]),
       let site, protein =
         add_site agent site_droit protein
       in snd (add_internal_state site droit_p protein))
  in
  let unphospho_d =
    (function protein ->
       ([],[],[]),
       let site, protein =
         add_site agent site_droit protein
       in snd (add_internal_state site droit_u protein))
  in
  let phospho_g =
    (function protein ->
       ([],[],[]),
       let site, protein =
         add_site agent site_gauche protein
       in snd (add_internal_state site gauche_p protein))
  in
  let unphospho_g =
    (function protein ->
       ([],[],[]),
       let site, protein =
         add_site agent site_gauche protein
       in snd (add_internal_state site gauche_u protein))
  in
  let lhs, rhs =
    match
      side, action
    with
    | D, U -> phospho_d, unphospho_d
    | D, P -> unphospho_d, phospho_d
    | G, U -> phospho_g, unphospho_g
    | G, P -> unphospho_g, phospho_g
  in
  let file =
    "toy_rule_"^(string_of_side side)^"_"^(string_of_state action)^"_"^(string_of_state other)^".ladot"
  in
  build_rule ~file protein lhs rhs

let _ = make_rule ~side:D ~action:U ~other:Unknown
let _ = make_rule ~side:D ~action:P ~other:Unknown
let _ = make_rule ~side:G ~action:U ~other:Unknown
let _ = make_rule ~side:G ~action:P ~other:Unknown
let _ = make_rule ~side:D ~action:U ~other:U
let _ = make_rule ~side:D ~action:P ~other:U
let _ = make_rule ~side:G ~action:U ~other:U
let _ = make_rule ~side:G ~action:P ~other:U
let _ = make_rule ~side:D ~action:U ~other:P
let _ = make_rule ~side:D ~action:P ~other:P
let _ = make_rule ~side:G ~action:U ~other:P
let _ = make_rule ~side:G ~action:P ~other:P
