open Config
open Geometry
open Signature_egfr_bis

let [agent_egfr,[l,_;cr,_(*;c,_;n,_*)];],r=
  add_in_graph
    [egfr,0.,0.,[],
     [egfr_l,[Direction w],[];
      egfr_r,[Direction sw],[Free_site []]]]
    signature_egfr_bis
let [agent_egf,[siter,_]],er=
  add_in_graph
    [egf,-.2.,0.,[],
     [egf_r,[Direction e],[];]]
    r

let free = (fun remanent -> ([],[],[]),snd (add_free_list [l,[];siter,[]] remanent))
let bound = (fun remanent -> ([],[],[]), (add_link_list [siter,l]  remanent))
let sigmarulel,sigmaruler,_,rule  = build_rule er bound free

let l,er_refined =
  add_site ~directives:[Color "red";Direction (of_degree (-.30.))]
    agent_egfr egfr_c er
let _,er_refined =
  add_bound ~directives:[Color "red"] l er_refined

let sigmarulel',sigmaruler',_,rule_refined  = build_rule er_refined bound free

(*let erre = add_link_list [lift_site sigma cr,lift_site sigma' cr] erre
let erre = snd (add_free_list [lift_site sigma n,[];lift_site sigma' c,[];lift_site sigma c,[];lift_site sigma' n,[]] erre)
*)

let [agent_pattern,_], pattern =
  add_in_graph
    [egfr,0.,0.,[],
     [egfr_r,[Direction e],[Free_site []];
      egfr_c,[Direction w],[Bound_site []];
      egfr_l,[Direction (of_degree 30.)],[Free_site []]]]
    signature_egfr_bis

let pattern = move_remanent_above 0. pattern rule
let pattern = translate_graph {abscisse=8.2;ordinate=0.} pattern
let sr, sp, fig = disjoint_union rule pattern
let rule_refined = move_remanent_above 2. rule_refined rule
let sref,sother',fig = disjoint_union rule_refined fig

(*let p2 s s' ag ag' =
  lift_agent sref
    (lift_agent s' ag'),
  lift_agent sother'
    (lift_agent sreac
       (
          (lift_agent s ag)))*)

let p s s' ag  =
  lift_agent sother'

       (lift_agent sr
          (lift_agent s ag)),
  lift_agent sref
    (lift_agent s' ag)

let fig =
  add_proj
    ~color:"green"
    [p sigmarulel sigmarulel' agent_egfr;
     p sigmarulel sigmarulel' agent_egf;

    ]
    fig

let fig =
  add_proj
    ~color:"brown"
    [
    p sigmaruler sigmaruler' agent_egfr;
    p sigmaruler sigmaruler' agent_egf
    ]
    fig

let fig =
  add_proj
    [

       (
         (lift_agent sother'
            (lift_agent sp agent_pattern))),
     lift_agent sref
       (lift_agent sigmaruler' agent_egfr)
   ]
    fig

let _ = dump "ai_step_KO.ladot" fig
