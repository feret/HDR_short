
open Config
open Geometry
open Signature_polymer

let empty = signature_polymer

(* CONTACT MAP *)
let
  [
    p,[s_g,_;s_d,_]
  ],
  remanent
  =
  add_in_graph
    [
      protein,0.,0.,[],[site_gauche,[],[];
                        site_droit,[],[]]]
    empty


let contact_map =
  add_fictitious_link
    [0.538462,-.0.490339;-.0.538462,-.0.490339] remanent


let _ = dump "ring1.ladot" contact_map

let _,contact_map =
  add_free_list [s_g,[];s_d,[]] contact_map

let _ = dump "contact_map_poly.ladot" contact_map

type state = Free | Bound | Unknown
let string_of_state =
  function
  | Free -> "_free"
  | Bound -> "_bound"
  | Unknown -> "_whatever"
let string_of_bool =
  function
  | true -> "_crossed"
  | false -> ""

let make_protein b d g =
  let _, protein =
    add_in_graph
      [protein,0.,0.,[],[site_gauche,[],
                         (match g with
                           Unknown -> []
                         | Free ->
                           [Free_site []]
                         | Bound ->
                           [Bound_site []]);
                         site_droit,[],
                           (match d with
                             Unknown -> []
                           | Free -> [Free_site[]]
                           | Bound -> [Bound_site []])  ;
                        ]]
      empty
  in
  let protein = if b then
      cross protein else protein
  in
  let title = "poly"^(string_of_bool b)^(string_of_state g)^""^(string_of_state d)^".ladot"
  in
  dump title protein

let bool_list = [false;true]
let list_elt = [Bound;Free;Unknown]
let iter2 f l1 l2 =
  List.iter
    (fun a -> List.iter (f a) l2)
    l1
let iter3 f l1 l2 l3 =
  List.iter
    (fun a -> iter2 (f a) l2 l3)
    l1
let () = iter3 make_protein bool_list list_elt list_elt

let
  [
    p1,[s_g1,_;s_d1,_];
    p2,[s_g2,_;s_d2,_]
  ],
  ring2
  =
  add_in_graph
    [
      protein,0.,0.,[],[site_gauche,[Direction ne],[];
                        site_droit,[Direction se],[]];
      protein,1.5,0.,[],[site_gauche,[Direction sw],[];
                         site_droit,[Direction nw],[]]]
    empty

let ring2 = add_link_list [s_g1,s_d2;s_g2,s_d1] ring2
let _ = dump "ring2.ladot" ring2

let
  [
    p1,[s_g1,_;s_d1,_];
    p2,[s_g2,_;s_d2,_];
    p3,[s_g3,_;s_d3,_]
  ],
  ring3
  =
  add_in_graph
    [
      protein,0.,0.,[],[site_gauche,[Direction n],[];
                        site_droit,[Direction se],[]];
      protein,1.5,0.,[],[site_gauche,[Direction sw],[];
                         site_droit,[Direction n],[]];
      protein,0.75,1.25,[],[site_gauche,[Direction se],[];
                            site_droit,[Direction sw],[]]]
    empty

let ring3 = add_link_list [s_g1,s_d3;s_g2,s_d1;s_g3,s_d2] ring3
let _ = dump "ring3.ladot" ring3

let chain b n =
  let rec aux coord site_opt remanent n =
    let coord = coord -. 1.5 in
    let [ag,[std,_;stg,_]], remanent =
      add_in_graph
        [protein,coord,0.,[],
         [site_droit,[],[];
          site_gauche,[],[]
         ]]
        remanent
    in
    let remanent =
      match site_opt with
      | None ->
        snd ((if b then add_bound else add_free) std remanent)
      | Some st' ->
        add_link_list [std,st'] remanent
    in
    if n=1 then
      let _,remanent = add_free_list [stg,[]] remanent in
      remanent
    else
      aux coord (Some stg) remanent (n-1)
  in
  let s = if b then "open_" else "closed_" in
  let remanent = aux 0. None empty n in
dump (s^"chain_"^(string_of_int n)^".ladot") remanent

let () = iter2 chain [false;true] [1;2;3]
