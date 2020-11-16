open Config
open Geometry
open Signature_across

let x_a = 0.
let x_b = 0.6
let x_c = 1.2

let lift g = (fun _ -> ([],[],[]),g)

let bind_unbind s1 s2 g =
  (lift (snd (add_free_list [s1,[];s2,[]] g))),
  (lift (add_link_list [s1,s2] g))

let [agent_a,[site_a_r,_];
     agent_b,[site_b_l,_]],
              a_b =
  add_in_graph
    [
      a,x_a,0.,[],[a_r,[],[]];
      b,x_b,0.,[],[b_l,[],[]]
    ]
    signature

let lhs, rhs = bind_unbind site_a_r site_b_l a_b
let _ =
  build_rule ~file:"constraints_bind_a_b.ladot" signature
    lhs rhs

let site_x,a_b = add_site agent_b b_x a_b
let _,a_b = add_internal_state site_x b_x_u a_b

let rhs, lhs = bind_unbind site_a_r site_b_l a_b
let _ =
  build_rule ~file:"constraints_unbind_a_b.ladot" signature
    lhs rhs

let [agent_b,[site_b_x,_]]
     ,
    b_ =
  add_in_graph
    [
      b,x_b,0.,[],[b_x,[],[]]
    ]
    signature
let site_b_r,b_free = add_site agent_b b_r b_
let _,b_free = add_free_list [site_b_r,[]] b_free
let site_b_l,b_bound = add_site agent_b b_l b_
let _,b_bound = add_bound site_b_r b_bound
let _ =
  build_rule ~file:"constraints_activate_b.ladot" signature
    (lift (snd (add_internal_state site_b_x b_x_u b_bound )))
    (lift (snd (add_internal_state site_b_x b_x_p b_bound )))
let _ =
  build_rule ~file:"constraints_desactivate_b.ladot" signature
    (lift (snd (add_internal_state site_b_x b_x_p b_free )))
    (lift (snd (add_internal_state site_b_x b_x_u b_free )))


let [agent_b,[site_b_r,_];
         agent_c,[site_c_l,_]],
        b_c =
      add_in_graph
        [
          b,
          x_b,0.,[],[b_r,[],[]];
          c,x_c,0.,[],[c_l,[],[]]
        ]
        signature
    let lhs,rhs = bind_unbind site_b_r site_c_l b_c
    let _ =
      build_rule ~file:"constraints_unbind_b_c.ladot" signature
        rhs lhs

let site_x,b_c = add_site agent_b b_x b_c
let _,b_c = add_internal_state site_x b_x_p b_c
let lhs,rhs = bind_unbind site_b_r site_c_l b_c
let _ =
  build_rule ~file:"constraints_bind_b_c.ladot" signature
    lhs rhs

let
  [agent_b,[site_b_l,_;
            site_b_r,_;
            site_b_x,_
           ]],
   bb =
   add_in_graph
     [
       b,x_b,0.,[],
       [
         b_l,[],[];
         b_r,[],[];
         b_x,[],[]]
     ]
     signature

let f ?l_bound:(l_bound=false) ?r_bound:(r_bound=false) ?x_phos:(x_phos=false) =
  let add bool site graph =
    if bool
    then
      "bound",snd (add_bound site graph)
    else
      "free",
      (snd (add_free_list [site,[]] graph ))
  in
  let title_l, b = add l_bound site_b_l bb in
  let title_r, b = add r_bound site_b_r b in
  let title_x, b =
    if x_phos
    then
      "phos",snd (add_internal_state site_b_x b_x_p b)
    else
      "unphos",snd (add_internal_state site_b_x b_x_u b)
  in
  let _ =
  dump ("constraints_left_"^title_l^"_right_"^title_r^"_x_"^title_x^".ladot")
    b
  in
  ()

let rec aux k l =
  if k = 0 then l
  else
    aux (k-1)
      (List.fold_left
         (fun accu l -> (true::l)::(false::l)::accu)
         []
         l)
let l = aux 3 [[]]

let () =
  List.iter
    (fun [l_bound;r_bound;x_phos] ->
       f ~l_bound ~r_bound ~x_phos
    )
    l

let
  [
    agent_b,[site_b_l,_;site_b_r,_]
  ],
  pattern =
      add_in_graph
        [
          b,x_b,0.,[],
          [
            b_l,[],[Bound_site []];
            b_r,[],[Free_site []]];
        ]
        signature

let _ = dump "constraints_pattern.ladot" pattern

let
  [
    agent_a,[site_a_r,_];
    agent_b,[site_b_l,_;site_b_r,_;site_b_x,_]
  ],
  sg =
      add_in_graph
        [
          a,x_a,0.,[],
          [
            a_r,[],[];
          ];
          b,x_b,0.,[],
          [
            b_l,[],[];
            b_r,[],[Free_site []];
            b_x,[],[Internal_state (b_x_u,[])]
          ]
        ]
        signature
let sg = add_link_list [site_a_r,site_b_l] sg
let _ = dump "constraints_site_graph.ladot" sg
