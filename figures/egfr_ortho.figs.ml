open Config
open Geometry
open Signature_egfr

let [agent_egfr,[cr,_];],r=
  add_in_graph
    [egfr,0.,0.,[],
     [egfr_r,[Direction (of_degree 45.)],[];
     ]]
    signature_egfr

let sigma,sigma',rr = duplicate ~add:(+.0.2) r

let site1, rr =
  add_site
    (lift_agent sigma agent_egfr) egfr_c ~directives:[Direction (of_degree 90.)] rr

let site2, rr =
  add_site (lift_agent sigma' agent_egfr) egfr_n ~directives:[Direction (of_degree (-.135.))] rr

let rr_same = add_link_list [lift_site sigma cr,lift_site sigma' cr;site1,site2] rr
let _ = dump "r_cr_bound_c_bound_same.ladot" rr_same
let _ = dump "crossed_r_cr_bound_c_bound_same.ladot" (cross rr_same)

let [agent_egfr,_],r=
  add_in_graph
    [egfr,0.,0.,[],
     []]
    signature_egfr

let r' = move_remanent_above 0.2 r r
let sigmad,sigmau,rr = disjoint_union r r'
let rr' = translate_graph {abscisse=0.;ordinate=0.6} (move_remanent_right_to 1. rr r')
let sigmal,sigmar,rrr = disjoint_union r' rr'

let site1, rrr =
  add_site
    (lift_agent sigmal agent_egfr) egfr_c ~directives:[Direction (of_degree 135.)] rrr

let site2, rrr =
  add_site (lift_agent sigmar (lift_agent sigmad agent_egfr)) egfr_n ~directives:[Direction (of_degree (-.65.))] rrr

let site3, rrr =
    add_site
      (lift_agent sigmal agent_egfr) egfr_r ~directives:[Direction (of_degree 45.)] rrr

let site4, rrr =
    add_site (lift_agent sigmar (lift_agent sigmau agent_egfr)) egfr_r ~directives:[Direction (of_degree (-.115.))] rrr

let rr_distinct =
  add_link_list
    [site3,site4;
     site1,site2;
    ]
    rrr


let _ = dump "r_cr_bound_c_bound_distinct.ladot" rr_distinct
let _ = dump "crossed_r_cr_bound_c_bound_distinct.ladot" (cross rr_distinct)
