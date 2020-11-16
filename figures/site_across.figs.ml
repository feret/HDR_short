open Config
open Geometry
open Signature_across

let x_d = 0.
let x_e = 0.7

let lift = (fun g -> ([],[],[]),g)
let lift' g = (fun _ -> lift g)

let [agent_d,[site_d_r,_;site_d_x,_]], graph_d =
  add_in_graph
    [
      d,x_d,0.,[],[d_r,[],[Free_site []];d_x,[],[]]
    ]
    signature

let lhs = snd (add_internal_state site_d_x d_x_u graph_d)
let rhs = snd (add_internal_state site_d_x d_x_p graph_d)

let _ = dump "site_across_monomer_d_cyto.ladot" lhs
let _ = dump "site_across_monomer_d_nucleus.ladot" rhs

let lhs = lift' lhs
let rhs = lift' rhs

let _ =
  build_rule ~file:"site_across_intern_d.ladot" signature
    lhs rhs
let _ =
  build_rule ~reversible:true ~file:"site_across_intern_extern_d.ladot" signature
    lhs rhs

let _ =
  build_rule ~file:"site_across_extern_d.ladot" signature
    rhs lhs

let [agent_e,[site_e_l,_;site_e_x,_]], graph_e =
  add_in_graph
    [
      e,x_e,0.,[],[e_l,[],[Free_site []];e_x,[],[]]
    ]
    signature

let lhs = snd (add_internal_state site_e_x e_x_u graph_e)
let rhs = snd (add_internal_state site_e_x e_x_p graph_e)

let _ = dump "site_across_monomer_e_cyto.ladot" lhs
let _ = dump "site_across_monomer_e_nucleus.ladot" rhs

let lhs = lift' lhs
let rhs = lift' rhs
let _ =
  build_rule ~file:"site_across_intern_e.ladot" signature
    lhs rhs
let _ =
  build_rule ~reversible:true ~file:"site_across_intern_extern_e.ladot"
    signature lhs rhs
let _ =
  build_rule ~file:"site_across_extern_e.ladot" signature
    rhs lhs


let [
  agent_d,[site_d_r,_;site_d_x,_ ];
  agent_e,[site_e_l,_;site_e_x,_]], d_e =
      add_in_graph
        [
          d,x_d,0.,[],[d_r,[],[];d_x,[],[]];
          e,x_e,0.,[],[e_l,[],[];e_x,[],[]]
        ]
        signature
let f b =
  let state1, state2, string =
    if
      b
    then
      d_x_u,e_x_u,"u"
    else
      d_x_p,e_x_p,"p"
  in
  let bind = (fun g -> lift (add_link_list [site_d_r,site_e_l] g)) in
  let free = (fun g -> lift (snd (add_free_list [site_d_r,[];site_e_l,[]] g))) in
  let d_e = snd (add_internal_state site_d_x state1 d_e) in
  let d_e = snd (add_internal_state site_e_x state2 d_e) in
  let _ = build_rule ~file:("site_across_bind_"^string^".ladot")
      d_e
      free
      bind
  in
  let _ = build_rule ~file:("site_across_bind_unbind_"^string^".ladot")
      ~reversible:true
      d_e
      free
      bind
  in
  let _ = build_rule ~file:("site_across_unbind_"^string^".ladot")
      d_e bind free
  in
()

let () = f true
let () = f false

let d_e_bound = add_link_list [site_d_r,site_e_l] d_e
let d_e_bound_u =
  snd
    (add_internal_state site_d_x d_x_u
       (snd (add_internal_state site_e_x e_x_u d_e_bound)))

let d_e_bound_p =
  snd (
    add_internal_state site_d_x d_x_p
      (snd (add_internal_state site_e_x e_x_p d_e_bound)))

let lhs = lift' d_e_bound_u
let rhs = lift' d_e_bound_p
let _ =
  build_rule ~file:("site_across_intern_d_e.ladot")
    d_e lhs rhs
let _ =
  build_rule ~file:("site_across_extern_d_e.ladot")
    d_e rhs lhs
let _ =
  build_rule
    ~reversible:true ~file:("site_accorss_intern_extern_d_e.ladot")
    d_e lhs rhs

let f ?d_nucleus:(d_nucleus=false) ?e_nucleus:(e_nucleus=false) =
  let graph = d_e_bound in
  let title_d, graph =
    if d_nucleus
    then
      "nucleus",snd (add_internal_state site_d_x d_x_p graph)
    else
      "cyto",snd (add_internal_state site_d_x d_x_u graph)
  in
  let title_e, graph =
    if e_nucleus
    then
      "nucleus",snd (add_internal_state site_e_x e_x_p graph)
    else
      "cyto",snd (add_internal_state site_e_x e_x_u graph)
  in
  let _ =
    dump ("site_across_dimer_d_"^title_d^"_e_"^title_e^".ladot")
      graph
  in
  dump ("cross_site_across_dimer_d_"^title_d^"_e_"^title_e^".ladot")
    (cross graph)

let rec aux k l =
  if k = 0 then l
  else
    aux (k-1)
      (List.fold_left
         (fun accu l -> (true::l)::(false::l)::accu)
         []
         l)
let l = aux 2 [[]]

let () =
  List.iter
    (fun [d_nucleus;e_nucleus] ->
       f ~d_nucleus ~e_nucleus
    )
    l

let [
  agent_d,[site_d_r,_];
  agent_e,[site_e_l,_]], d_e
                        =
  add_in_graph
    [
      d,x_d,0.,[],[d_r,[],[]];
      e,x_e,0.,[],[e_l,[],[]]
    ]
    signature

let bind = (fun g -> lift (add_link_list [site_d_r,site_e_l] g))
let free = (fun g -> lift (snd (add_free_list [site_d_r,[];site_e_l,[]] g)))
let _ =
  build_rule ~file:("site_across_unbinding.ladot")
    d_e
    bind
    free
