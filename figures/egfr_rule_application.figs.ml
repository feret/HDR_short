open Config
open Geometry
open Signature_egfr

let build_rule_application bool highlight =
  let
  [
    sp_egf1,[sp_egf1_r,_];
    sp_egfr1,[sp_egfr1_l,_;sp_egfr1_r,_;sp_egfr1_c,_;sp_egfr1_n,_;sp_egfr1_Y68,_;sp_egfr1_Y48,_];
    sp_egf2,[sp_egf2_r,_];
    sp_egfr2,[sp_egfr2_l,_;sp_egfr2_r,_;sp_egfr2_c,_;sp_egfr2_n,_;sp_egfr2_Y68,_;sp_egfr2_Y48,_]  ;
    sp_shc1,[sp_shc1_pi,_;sp_shc1_Y7,_] ;
    sp_shc2,[sp_shc2_pi,_;sp_shc2_Y7,_] ;
    sp_grb21,[sp_grb21_a,_;sp_grb21_b,_] ;
    sp_grb22,[sp_grb22_a,_;sp_grb22_b,_] ;
    sp_sos1,[sp_sos1_d,_]
  ],
  species
  =
  add_in_graph
    [
      egf,1.,13.8,[Tag ("pattern",0)],
      [egf_r,[Direction s],[]];
      egfr,1.,12.,[],
      [egfr_l,[Direction (of_degree 0.);Tag ("pattern",0)],[];
       egfr_r,[Direction (of_degree 45.)],[];
       egfr_c,[Direction (of_degree 90.);Tag ("pattern",0)],[Free_site []];
       egfr_n,[Direction (of_degree 135.);Tag ("pattern",0)],[Free_site []];
       egfr_Y68,[Direction (of_degree (to_degree sw-.10.));Tag ("pattern",0)],
       [
        Internal_state (egfr_Y68_p,[Direction sw;Tag ("pattern",0)])];
       egfr_Y48,[Direction w;Tag ("frag",2)],[Internal_state (egfr_Y48_p,[Direction w])]
      ];
      egf,3.5,13.8,[Tag ("pattern",0)],
      [egf_r,[Direction s],[]];
      egfr,3.5,12.,[],
      [egfr_l,[Direction (of_degree 0.);Tag ("pattern",0)],[];
       egfr_r,[Direction (of_degree (-.45.))],[];
       egfr_c,[Direction (of_degree (-.90.));Tag ("pattern",0)],[Free_site []];
       egfr_n,[Direction (of_degree (-.135.));Tag ("pattern",0)],[Free_site []];
       egfr_Y68,[Direction (of_degree (to_degree se-.10.));Tag ("frag",3)],[Internal_state (egfr_Y68_p,[Direction e;Tag ("pattern",0)])];
       egfr_Y48,[Direction ne;Tag ("frag",4)],[Internal_state (egfr_Y48_p,[Direction e;Tag ("pattern",0)])]
      ];
      shc,-0.4,10.5,[Tag ("frag",2);Tag("pattern",0)],
      [shc_pi,[Direction (of_degree 0.)],[];
       shc_Y7,[Direction se;Tag ("pattern",0)],[Internal_state((if bool then shc_Y7_p else shc_Y7_u),[Direction s]);Free_site []]];
      shc,5.,13.5,[Tag ("frag",4)],
      [shc_pi,[Direction sw],[];
       shc_Y7,[Direction (of_degree 0.);Tag ("pattern",0)],[Free_site [Direction nw;Tag ("pattern",0)];Internal_state((if bool then shc_Y7_u else shc_Y7_p),[Direction ne;Tag ("pattern",0)])]];
      grb2,0.5,9.,[Tag ("frag",2);Tag ("pattern",0)],
      [grb2_a,[Direction nw],[];
       grb2_b,[Direction e],[]];
      grb2,5.,10.,[Tag ("frag",3);Tag ("pattern",0)],
      [grb2_a,[Direction (of_degree 0.)],[];
       grb2_b,[Direction w],[Free_site [Direction sw]]];
      sos,3.2,9.,[Tag ("frag",2);Tag("pattern",0)],
      [sos_d,[Direction w],[]]]
    signature_egfr
  in
  let species =
    if highlight then
      let species =
        draw_circle_around_site
          sp_shc1_Y7
          ~color:"red"
          ~thickness:3
          ~radius:0.8
          species
      in
      draw_circle_around_site
        sp_shc2_Y7
        ~color:"red"
        ~thickness:3
        ~radius:0.8
        species
    else
      species
  in
let species =
  add_link_list
    [
      sp_egf2_r,sp_egfr2_l;
      sp_egf1_r,sp_egfr1_l;
      sp_egfr1_l,sp_egf1_r;
      sp_egfr2_l,sp_egf2_r;
      sp_egfr1_Y48,sp_shc1_pi;
      sp_egfr2_Y48,sp_shc2_pi;
      sp_egfr1_Y68,sp_grb21_a;
      sp_grb21_b,sp_sos1_d;
      sp_egfr2_Y68,sp_grb22_a
    ]
    species
in

let prefix =
  if bool then "reaction_1" else "reaction_2"
in
let infix =
  if highlight then "_highlighted" else "" in
let prefix = prefix^infix in
let lhs =
  add_link_list [sp_egfr2_r,sp_egfr1_r] species
in
let _,rhs =
  add_free_list
    [sp_egfr2_r,[];sp_egfr1_r,[]]  species
in
let _ = dump (prefix^"_lhs.ladot") lhs in
let _ = dump (prefix^"_lhs.ladot") rhs in
let sigmalhs,sigmarhs,_,reaction =
  build_rule signature_egfr
    (fun _ -> ([],[],[]),lhs)
    (fun _ -> ([],[],[]),rhs)
    ~file:(prefix^".ladot")
in
let [agent_egfr,[cr,_;c,_;n,_];],r=
  add_in_graph
    [egfr,0.,0.,[],
     [egfr_r,[Direction (of_degree 45.)],[];
      egfr_c,[Direction (of_degree 90.)],[Free_site []];
      egfr_n,[Direction (of_degree 135.)],[Free_site []]]]
    signature_egfr
in

let sigma,sigma',rr = duplicate ~add:(-.0.2) r in

let free =
  (fun remanent -> ([],[],[]),snd (add_free_list [lift_site sigma cr,[];lift_site sigma' cr,[]] remanent))
in
let bound =
  (fun remanent -> ([],[],[]), (add_link_list [lift_site sigma cr,lift_site sigma' cr] remanent))
in

let sigmarlhs,sigmarrhs,_,break =
  build_rule ~file:(prefix^"_rule.ladot") rr bound free
in
let break =
  translate_graph
    {abscisse=3.;ordinate=0.}
    break
in
let break = move_remanent_bellow 2. break reaction in
let sigmad,sigmau,diagram =
  disjoint_union break reaction
in

let diagram =
  add_proj
    [lift_agent sigmad
       (lift_agent sigmarlhs
          (lift_agent sigma agent_egfr)),
     lift_agent sigmau
       (lift_agent sigmalhs
          sp_egfr1);
     lift_agent sigmad
             (lift_agent sigmarlhs
                (lift_agent sigma' agent_egfr)),
           lift_agent sigmau
             (lift_agent sigmalhs
                sp_egfr2);
    ] diagram
in
let _ = dump (prefix^"_diagram.ladot") diagram
in ()

let () = build_rule_application true true
let () = build_rule_application false true
let () = build_rule_application true false
let () = build_rule_application false false
