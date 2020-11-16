open Config
open Geometry
open Signature_egfr_bis

let [agent_egfr,[cr,_;c,_;n,_];],r=
  add_in_graph
    [egfr,0.,0.,[],
     [egfr_r,[Direction (of_degree 45.)],[];
      egfr_c,[Direction (of_degree 90.)],[];
      egfr_n,[Direction (of_degree 135.)],[]]]
    signature_egfr_bis
let [agent_egf,[siter,_]],er=
  add_in_graph
    [egf,0.,2.,[],
     [egf_r,[Direction (of_degree 180.)],[];]]
    r
let y48,er = add_site ~directives:[Direction (of_degree 300.)] agent_egfr egfr_Y48 er
let _,er = add_internal_state y48 egfr_Y48_u er
let y68,er = add_site ~directives:[Direction (of_degree 240.)] agent_egfr egfr_Y68 er
let _,er = add_internal_state y68 egfr_Y68_u er
let _,er = add_free_list [y48,[Direction (of_degree 280.)];y68,[Direction (of_degree 220.)]] er

let l,er = add_site ~directives:[Direction (of_degree 0.)] agent_egfr egfr_l er
let er = add_link_list [siter,l] er

let sigma,sigma',rr = duplicate ~add:(-.0.) r
let agent_egfr' = lift_agent sigma' agent_egfr
let rr = add_link_list [lift_site sigma cr,lift_site sigma' cr] rr
let rr = snd (add_free_list [lift_site sigma n,[];lift_site sigma' c,[]] rr)
let free = (fun remanent -> ([],[],[]),snd (add_free_list [lift_site sigma c,[];lift_site sigma' n,[]] remanent))
let bound = (fun remanent -> ([],[],[]), (add_link_list [lift_site sigma c,lift_site sigma' n] remanent))
let sigmarulel,sigmaruler,_,rule  = build_rule rr free bound

let l,rr_refined =
  add_site ~directives:[Color "red";Direction (of_degree (-.30.))]
    (lift_agent sigma agent_egfr) egfr_l  rr
let _,rr_refined =
  add_bound ~directives:[Color "red"] l rr_refined

let rr_refined = add_link_list [lift_site sigma cr,lift_site sigma' cr] rr_refined
let rr_refined = snd (add_free_list [lift_site sigma n,[];lift_site sigma' c,[]] rr_refined)
let free = (fun remanent -> ([],[],[]),snd (add_free_list [lift_site sigma c,[];lift_site sigma' n,[]] remanent))
let bound = (fun remanent -> ([],[],[]), (add_link_list [lift_site sigma c,lift_site sigma' n] remanent))
let sigmarulel',sigmaruler',_,rule_refined  = build_rule rr_refined free bound

let sigma,sigma',erre = duplicate ~add:(-.0.) er
let erre = add_link_list [lift_site sigma cr,lift_site sigma' cr] erre
let erre = snd (add_free_list [lift_site sigma n,[];lift_site sigma' c,[];lift_site sigma c,[];lift_site sigma' n,[]] erre)

let [agent_pattern,_], pattern =
  add_in_graph
    [egfr,0.,0.,[],
     [egfr_r,[Direction (of_degree 45.)],[Bound_site []];
      egfr_c,[Direction (of_degree 90.)],[Bound_site []];
      egfr_l,[Direction (of_degree (-.30.))],[Bound_site []]]]
    signature_egfr_bis

let pattern = move_remanent_above 0. pattern rule
let pattern = translate_graph {abscisse=5.2;ordinate=0.} pattern
let sr, sp, fig = disjoint_union rule pattern
let erre = move_remanent_above 5. erre rule
let sreac,sother,fig = disjoint_union erre fig
let rule_refined = move_remanent_above 2. rule_refined rule
let sref,sother',fig = disjoint_union rule_refined fig

let p2 s s' ag ag' =
  lift_agent sref
    (lift_agent s' ag'),
  lift_agent sother'
    (lift_agent sreac
       (
           (lift_agent s ag)))

let p s s' ag  =
  lift_agent sother'
    (lift_agent sother
       (lift_agent sr
          (lift_agent s ag))),
  lift_agent sref
    (lift_agent s' ag)

let fig =
  add_proj
    ~color:"green"
    [p sigmarulel sigmarulel' agent_egfr;
       p sigmarulel sigmarulel' agent_egfr';

    ]
    fig

let fig =
  add_proj
    ~color:"brown"
    [
    p sigmaruler sigmaruler' agent_egfr';
       p sigmaruler sigmaruler' agent_egfr
    ]
    fig

let fig =
  add_proj
    [
     lift_agent sother'
       (lift_agent sother
          (lift_agent sp agent_pattern)),
     lift_agent sref
       (lift_agent sigmaruler' agent_egfr)
   ]
    fig
let fig =
  add_proj
    ~color:"red"
    [
     p2 sigmarulel sigmarulel' agent_egfr agent_egfr;
     p2 sigmarulel sigmarulel' (lift_agent sigma' agent_egfr) agent_egfr'
     ;

    ]
    fig

let _ = dump "ai_step.ladot" fig
