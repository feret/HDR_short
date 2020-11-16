open Config
open Geometry
open Signature_egfr

let empty = signature_egfr

let ne = of_degree 30.
let se = of_degree 150.

let [agent_egf,[segf,_];agent_egfr,[segfr,_]],couple =
  add_in_graph
    [egf,0.,0.,[],[egf_r,[Direction e],[]];
     egfr,2.,0.,[],[egfr_l,[Direction w],[]]]
    signature_egfr

let add_bond_er =
  (fun remanent ->
     let graph = add_link_list [segf,segfr] remanent in
     ([],[],[]),graph)
let add_free_er =
  (fun remanent ->
     let _, graph = add_free_list [segf,[];segfr,[]] remanent
     in ([],[],[]),graph)

let _ =
  build_rule  ~file:"e+r.ladot"  couple
    add_free_er
    add_bond_er


let segfr_r,couple' = add_site agent_egfr egfr_r couple
let _,couple' = add_free_list [segfr_r,[]] couple'

let _,_,_,e_r =
  build_rule  ~file:"e++r.ladot" couple'
    add_bond_er
    add_free_er

let [agent_egfr,[l,_;cr,_];],r=
  add_in_graph
    [egfr,0.,0.,[],[egfr_l,[Direction (of_degree 0.)],[Bound_site []];egfr_r,[Direction (of_degree 45.)],[]];
    ]
    signature_egfr


let sigma,sigma',rr = duplicate ~add:0.5 r
let free = (fun remanent -> ([],[],[]),snd (add_free_list [lift_site sigma cr,[];lift_site sigma' cr,[]] remanent))
let bound = (fun remanent -> ([],[],[]), (add_link_list [lift_site sigma cr,lift_site sigma' cr] remanent))

let _,_,_,rr = build_rule  ~file:"r+r_cr.ladot" rr free bound

let [agent_egf,[segf,_];agent_egfr,[segfrl,_;cr,_]],couple =
  add_in_graph
    [egf,0.,1.8,[],[egf_r,[Direction s],[]];
     egfr,0.,0.,[],[egfr_l,[Direction n],[];
                    egfr_r,[Direction (of_degree 90.)],[]]]
    signature_egfr

let couple = add_link_list [segf,segfrl] couple
let sigma,sigma',erre = duplicate ~add:0.5 couple
let free = (fun remanent -> ([],[],[]),snd (add_free_list [lift_site sigma cr,[];lift_site sigma' cr,[]] remanent))
let bound = (fun remanent -> ([],[],[]), (add_link_list [lift_site sigma cr,lift_site sigma' cr] remanent))
let _,_,_,_ = build_rule  ~file:"er+er_cr.ladot" erre free bound


let free = (fun remanent -> ([],[],[]),
                            add_flow_list [lift_site sigma' segf,lift_site sigma' segfrl;lift_site sigma' segfrl,lift_site sigma' cr;
                                          lift_site sigma segf,lift_site sigma segfrl;lift_site sigma segfrl,lift_site sigma cr]

                              (snd (add_free_list [lift_site sigma cr,[];lift_site sigma' cr,[]] remanent)))
let _,_,_,erre = build_rule  ~file:"er+er_cr_annotated.ladot" erre free bound


let [agent_egfr,[cr,_;c,_;n,_];],r=
  add_in_graph
    [egfr,0.,0.,[],
     [egfr_r,[Direction (of_degree 45.)],[];
      egfr_c,[Direction (of_degree 90.)],[Free_site []];
      egfr_n,[Direction (of_degree 135.)],[Free_site []]]]
    signature_egfr


let sigma,sigma',rr = duplicate ~add:(-.0.2) r

let free = (fun remanent -> ([],[],[]),snd (add_free_list [lift_site sigma cr,[];lift_site sigma' cr,[]] remanent))
let bound = (fun remanent -> ([],[],[]), (add_link_list [lift_site sigma cr,lift_site sigma' cr] remanent))

let _ = build_rule (*~directives:[Comment "$\text{\agent{R}{}/\agent{R}{}}$"]*) ~file:"r++r_cr.ladot" rr bound free

let [agent_egfr,[cr,_;c,_;n,_];],r=
  add_in_graph
    [egfr,0.,0.,[],
     [egfr_r,[Direction (of_degree 45.)],[];
      egfr_c,[Direction (of_degree 90.)],[Free_site []];
      egfr_n,[Direction (of_degree 135.)],[Free_site []]]]
    signature_egfr


let sigma,sigma',rr = duplicate ~add:(-.0.2) r

let free = (fun remanent -> ([],[],[]),snd (add_free_list [lift_site sigma cr,[];lift_site sigma' cr,[]] remanent))
let bound = (fun remanent -> ([],[],[]), (add_link_list [lift_site sigma cr,lift_site sigma' cr] remanent))

let _ = build_rule  ~file:"r++r_c.ladot" rr bound free

let [agent_egfr,[cr,_;c,_;n,_];],r=
  add_in_graph
    [egfr,0.,0.,[],
     [egfr_r,[Direction (of_degree 45.)],[];
      egfr_c,[Direction (of_degree 90.)],[];
      egfr_n,[Direction (of_degree 135.)],[]]]
    signature_egfr


let sigma,sigma',rr = duplicate ~add:(-.0.) r
let rr = add_link_list [lift_site sigma cr,lift_site sigma' cr] rr

let rr = snd (add_free_list [lift_site sigma n,[];lift_site sigma' c,[]] rr)
let free = (fun remanent -> ([],[],[]),snd (add_free_list [lift_site sigma c,[];lift_site sigma' n,[]] remanent))
let bound = (fun remanent -> ([],[],[]), (add_link_list [lift_site sigma c,lift_site sigma' n] remanent))

let _ = build_rule  ~file:"r+r_cn.ladot" rr free bound

let [agent_egfr,[site_c,_;site_y68,_]],r=
  add_in_graph
    [egfr,0.,0.,[],
     [egfr_c,[],[Bound_site []];
      egfr_Y68,[],[]]]
    signature_egfr


let u = (fun remanent -> ([],[],[]),snd (add_internal_state ~directives:[Direction (of_degree 135.)] site_y68 egfr_Y68_u remanent))

let p = (fun remanent -> ([],[],[]),snd (add_internal_state ~directives:[Direction (of_degree 135.)] site_y68 egfr_Y68_p remanent))

let _ = build_rule  ~file:"r68-p.ladot" r u p

let [agent_egfr,[site_y68,_]],r=
  add_in_graph
    [egfr,0.,0.,[],
     [
       egfr_Y68,[],[Free_site []]]]
    signature_egfr

let u = (fun remanent -> ([],[],[]),snd (add_internal_state ~directives:[Direction (of_degree 135.)] site_y68 egfr_Y68_u remanent))

let p = (fun remanent -> ([],[],[]),snd (add_internal_state ~directives:[Direction (of_degree 135.)] site_y68 egfr_Y68_p remanent))

let _ = build_rule  ~file:"r68-u.ladot" r p u


let [agent_egfr,[]],r=
  add_in_graph
    [egfr,0.,0.,[],
     []]
    signature_egfr

let c,r1 = add_site ~directives:[Direction (of_degree 90.)] agent_egfr egfr_c r
let n,r2 = add_site ~directives:[Direction (of_degree 135.)] agent_egfr egfr_n r
let sigma,sigma',rr = disjoint_union r1 (move_remanent_right_to 0.29 (vertical_swap r2) r1)

let free = (fun remanent -> ([],[],[]),snd (add_free_list [lift_site sigma c,[];lift_site sigma' n,[]] remanent))
let bound = (fun remanent -> ([],[],[]),  (add_link_list [lift_site sigma c,lift_site sigma' n] remanent))
let _ = build_rule  ~file:"r++r_cn.ladot" rr bound free

let [agent_grb,[a,_];
     agent_egfr,[y,_;]], gr =
  add_in_graph
    [grb2,2.,0.,[],[grb2_a,[Direction (of_degree 270.)],[]];
     egfr,0.,0.,[],[egfr_Y68,[Direction (of_degree 90.)],[Internal_state (egfr_Y68_p,[Direction (of_degree 180.)])]]]
    signature_egfr

let free = (fun remanent -> ([],[],[]),snd (add_free_list [a,[];y,[]] remanent))
let bound = (fun remanent -> ([],[],[]), (add_link_list [a,y] remanent))

let _ = build_rule ~file:"g+r.ladot" gr free bound

let [agent_grb,[a,_];
     agent_egfr,[y,_;]], gr =
  add_in_graph
    [grb2,0.,0.,[],[grb2_a,[Direction (of_degree 90.)],[]];
     egfr,2.0,0.,[],[egfr_Y68,[Direction (of_degree 270.)],[]]]
    signature_egfr

let free = (fun remanent -> ([],[],[]),snd (add_free_list [a,[];y,[]] remanent))
let bound = (fun remanent -> ([],[],[]), (add_link_list [a,y] remanent))


let _ = build_rule  ~file:"g++r.ladot" gr bound free

let [agent_grb,[b,_];
     agent_sos,[d,_;]], gr =
  add_in_graph
    [grb2,0.,0.,[],[grb2_b,[Direction (of_degree 90.)],[]];
     sos,2.0,0.,[],[sos_d,[Direction (of_degree 270.)],[]]]
    signature_egfr

let free = (fun remanent -> ([],[],[]),snd (add_free_list [a,[];y,[]] remanent))
let bound = (fun remanent -> ([],[],[]), (add_link_list [a,y] remanent))

let _ = build_rule ~file:"g+sos.ladot" gr free bound

let _ = build_rule ~file:"g++sos.ladot" gr bound free
