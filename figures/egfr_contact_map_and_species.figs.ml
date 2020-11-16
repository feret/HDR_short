open Config
open Geometry
open Signature_egfr

let empty = signature_egfr

(* CONTACT MAP *)
let
  [
    cm_egf,[cm_egf_r,_];
    cm_egfr,[cm_egfr_l,_;cm_egfr_r,_;cm_egfr_c,_;cm_egfr_n,_;
             cm_egfr_Y68,_;cm_egfr_Y48,_];
    cm_shc,[cm_shc_pi,_;cm_shc_Y7,_];
    cm_grb2,[cm_grb2_a,_;cm_grb2_b,_];
    cm_sos,[cm_sos_d,_]
  ],
  remanent
  =
  add_in_graph
    [
      egf,2.2,12.5,[],[egf_r,[Direction (of_degree 220.)],[Free_site [Direction se ]]];
      egfr,0.6,11.,[],
      [egfr_l,[Direction (of_degree 30.)],
       [Free_site [Direction e]];
       egfr_r,[Direction (of_degree 90.)],
       [Free_site [Direction ne]];
       egfr_c,[Direction (of_degree 120.)],
       [Free_site [Direction e]];
       egfr_n,[Direction (of_degree 150.)],
       [Free_site [Direction s]];
       egfr_Y68,[Direction (of_degree 225.)],
       [Free_site [Direction nw];
        Internal_state (egfr_Y68_u,[Direction s]);
        Internal_state (egfr_Y68_p,[Direction (of_degree (to_degree s-.20.))])];
       egfr_Y48,[Direction (of_degree 330.)],
       [Free_site [Direction ne];
        Internal_state (egfr_Y48_u,[Direction (of_degree (to_degree w+.10.))]);
        Internal_state (egfr_Y48_p,[Direction (of_degree (to_degree w-.10.))])
       ]];
      shc,-.0.85,12.5,[],
      [shc_pi,[Direction (of_degree 110.)],
       [Free_site []];
       shc_Y7,[Direction (of_degree 250.)],
       [Free_site [Direction sw];
        Internal_state (shc_Y7_u,[Direction (of_degree (to_degree w))]);
        Internal_state (shc_Y7_p,[Direction (of_degree (to_degree w+.20.))])
       ]];
      grb2,-.0.85,9.65,[],
      [grb2_a,[Direction n],
       [Free_site [Direction nw]];
       grb2_b,[Direction e],
       [Free_site [Direction se]]];
      sos,2.2,9.65,[],
      [sos_d,[Direction w],
       [Free_site [Direction sw]]]]
    empty


let x,remanent = add_empty_graph 1.8 10. remanent
let y,remanent = add_empty_graph 0. 9. remanent

let remanent =
  add_link_list
    [
      cm_egf_r,cm_egfr_l;
      cm_egfr_l,cm_egf_r;
      cm_egfr_r,cm_egfr_r;
      cm_egfr_c,cm_egfr_n;
      cm_egfr_Y48,cm_shc_pi;
      cm_shc_Y7,cm_grb2_a;
      cm_grb2_b,cm_sos_d;
      cm_egfr_Y68,cm_grb2_a;
    ]
    remanent

let remanent = add_fictitious_link [1.6,10.8;0.75,10.1] remanent


let contact_map = remanent


let _ = dump "contact_map.ladot" contact_map

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
  remanent
  =
  add_in_graph
    [
      egf,1.,13.8,[Tag ("pattern",0)],
      [egf_r,[Direction s],[]];
      egfr,1.,12.,[],
      [egfr_l,[Direction n],[];
       egfr_r,[Direction (of_degree 45.)],[];
       egfr_c,[Direction (of_degree 90.);Tag ("pattern",0)],[];
       egfr_n,[Direction (of_degree 135.);Tag ("pattern",0)],[Free_site []];
       egfr_Y68,[Direction (of_degree (to_degree sw-.10.));Tag ("frag",1);Tag ("pattern",0)],
       [Free_site ([Direction sw;Tag ("pattern",0)]);
        Internal_state (egfr_Y68_p,[Direction s;Tag ("pattern",0)])];
       egfr_Y48,[Direction w;Tag ("frag",2)],[Internal_state (egfr_Y48_p,[Direction w])]
      ];
      egf,3.5,13.8,[Tag ("pattern",0)],
      [egf_r,[Direction s],[]];
      egfr,3.5,12.,[],
      [egfr_l,[Direction n;Tag ("pattern",0)],[];
       egfr_r,[Direction (of_degree (-.45.))],[];
       egfr_c,[Direction (of_degree (-.90.));Tag ("pattern",0)],[Free_site []];
       egfr_n,[Direction (of_degree (-.135.));Tag ("pattern",0)],[];
       egfr_Y68,[Direction (of_degree (to_degree se+.10.));Tag ("frag",3)],[Internal_state (egfr_Y68_p,[Direction e;Tag ("pattern",0)])];
       egfr_Y48,[Direction ne;Tag ("frag",4)],[Internal_state (egfr_Y48_p,[Direction e;Tag ("pattern",0)])]
      ];
      shc,-0.4,10.5,[Tag ("frag",2);Tag("pattern",0)],
      [shc_pi,[Direction n],[];
       shc_Y7,[Direction se;Tag ("pattern",0)],[Internal_state(shc_Y7_p,[Direction s])]];
      shc,5.,13.5,[Tag ("frag",4)],
      [shc_pi,[Direction sw],[];
       shc_Y7,[Direction n;Tag ("pattern",0)],[Free_site [Direction nw;Tag ("pattern",0)];Internal_state(shc_Y7_u,[Direction ne;Tag ("pattern",0)])]];
      grb2,1.8,9.,[Tag ("frag",2);Tag ("pattern",0)],
      [grb2_a,[Direction nw],[];
       grb2_b,[Direction e],[]];
      grb2,2.8,10.,[Tag ("frag",3);Tag ("pattern",0)],
      [grb2_a,[Direction n],[];
       grb2_b,[Direction w],[Free_site [Direction sw]]];
      sos,4.5,9.,[Tag ("frag",2);Tag("pattern",0)],
      [sos_d,[Direction w],[]]]
    empty

let remanent =
  add_link_list
    [
      sp_egfr1_r,sp_egfr2_r;
      sp_egfr2_r,sp_egfr1_r;
      sp_egf2_r,sp_egfr2_l;
      sp_egf1_r,sp_egfr1_l;
      sp_egfr1_l,sp_egf1_r;
      sp_egfr2_l,sp_egf2_r;
      sp_egfr1_c,sp_egfr2_n;
      sp_egfr1_Y48,sp_shc1_pi;
      sp_egfr2_Y48,sp_shc2_pi;
      sp_shc1_Y7,sp_grb21_a;
      sp_grb21_b,sp_sos1_d;
      sp_egfr2_Y68,sp_grb22_a
    ]
    remanent

let species = remanent

let _ = dump "species.ladot" ~flags:["flow",0]  species

let ()=
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
  remanent
  =
  add_in_graph
    [
      egf,1.,13.8,[Tag ("pattern",0)],
      [egf_r,[Direction s],[]];
      egfr,1.,12.,[],
      [egfr_l,[Direction nw],[];
       egfr_r,[Direction (of_degree 45.)],[];
       egfr_c,[Direction se;Tag ("pattern",0)],[];
       egfr_n,[Direction w;Tag ("pattern",0)],[Free_site []];
       egfr_Y68,[Direction sw;Tag ("frag",1);Tag ("pattern",0)],
       [Free_site ([Direction sw;Tag ("pattern",0)]);
        Internal_state (egfr_Y68_p,[Direction s;Tag ("pattern",0)])];
       egfr_Y48,[Direction w;Tag ("frag",2)],[Internal_state (egfr_Y48_p,[Direction w])]
      ];
      egf,3.5,13.8,[Tag ("pattern",0)],
      [egf_r,[Direction s],[]];
      egfr,3.5,12.,[],
      [egfr_l,[Direction ne;Tag ("pattern",0)],[];
       egfr_r,[Direction (of_degree (-.45.))],[];
       egfr_c,[Direction e;Tag ("pattern",0)],[Free_site []];
       egfr_n,[Direction (of_degree (-.135.));Tag ("pattern",0)],[];
       egfr_Y68,[Direction s;Tag ("frag",3)],[Internal_state (egfr_Y68_p,[Direction e;Tag ("pattern",0)])];
       egfr_Y48,[Direction ne;Tag ("frag",4)],[Internal_state (egfr_Y48_p,[Direction e;Tag ("pattern",0)])]
      ];
      shc,-0.4,10.5,[Tag ("frag",2);Tag("pattern",0)],
      [shc_pi,[Direction n],[];
       shc_Y7,[Direction se;Tag ("pattern",0)],[Internal_state(shc_Y7_p,[Direction s])]];
      shc,5.,13.5,[Tag ("frag",4)],
      [shc_pi,[Direction sw],[];
       shc_Y7,[Direction n;Tag ("pattern",0)],[Free_site [Direction nw;Tag ("pattern",0)];Internal_state(shc_Y7_u,[Direction ne;Tag ("pattern",0)])]];
      grb2,1.8,9.,[Tag ("frag",2);Tag ("pattern",0)],
      [grb2_a,[Direction nw],[];
       grb2_b,[Direction e],[]];
      grb2,2.8,10.,[Tag ("frag",3);Tag ("pattern",0)],
      [grb2_a,[Direction n],[];
       grb2_b,[Direction s],[Free_site [Direction sw]]];
      sos,4.5,9.,[Tag ("frag",2);Tag("pattern",0)],
      [sos_d,[Direction w],[]]]
    empty
in
let remanent =
  add_link_list
    [
      sp_egfr1_r,sp_egfr2_r;
      sp_egfr2_r,sp_egfr1_r;
      sp_egf2_r,sp_egfr2_l;
      sp_egf1_r,sp_egfr1_l;
      sp_egfr1_l,sp_egf1_r;
      sp_egfr2_l,sp_egf2_r;
      sp_egfr1_c,sp_egfr2_n;
      sp_egfr1_Y48,sp_shc1_pi;
      sp_egfr2_Y48,sp_shc2_pi;
      sp_shc1_Y7,sp_grb21_a;
      sp_grb21_b,sp_sos1_d;
      sp_egfr2_Y68,sp_grb22_a
    ]
    remanent
in
let anspecies  =
  add_flow_list
    [sp_egf1_r,sp_egfr1_l;
     sp_egfr1_l,sp_egf1_r;
     sp_egf2_r,sp_egfr2_l;
     sp_egfr2_l,sp_egf2_r;
     sp_egfr1_r,sp_egfr2_r;
     sp_egfr2_r,sp_egfr1_r;
     sp_egfr1_r,sp_egfr1_c;
     sp_egfr1_r,sp_egfr1_n;
     sp_egfr1_c,sp_egfr1_r;
     sp_egfr1_c,sp_egfr1_n;

     sp_egfr1_n,sp_egfr1_r;
     sp_egfr1_n,sp_egfr1_c;

     sp_egfr2_r,sp_egfr2_c;
     sp_egfr2_r,sp_egfr2_n;
     sp_egfr2_c,sp_egfr2_r;
     sp_egfr2_c,sp_egfr2_n;
     sp_egfr2_n,sp_egfr2_r;
     sp_egfr2_n,sp_egfr2_c;
     sp_egfr1_l,sp_egfr1_r;
     sp_egfr2_l,sp_egfr2_r;
     sp_egfr1_c,sp_egfr1_Y48;
     sp_egfr1_c,sp_egfr1_Y68;
     sp_egfr2_c,sp_egfr2_Y48;
     sp_egfr2_c,sp_egfr2_Y68;
     sp_egfr2_Y48, sp_shc2_pi;
     sp_egfr1_Y48, sp_shc1_pi;
     sp_shc2_pi,sp_shc2_Y7;
     sp_shc2_Y7,sp_shc2_pi;
     sp_shc1_pi,sp_shc1_Y7;
     sp_shc1_Y7,sp_shc1_pi;
     sp_shc1_Y7,sp_grb21_a;
     sp_egfr1_Y68,sp_grb21_a;
     sp_egfr2_Y68,sp_grb22_a;
     sp_grb21_a,sp_grb21_b;
     sp_grb21_b,sp_grb21_a;
     sp_grb21_b,sp_sos1_d;
     sp_grb22_a,sp_grb22_b;
     sp_grb22_b,sp_grb22_a;
    ]
    remanent
in
let _ = dump "pattern1.ladot" ~flags:["frag",1] anspecies in
let _ = dump "pattern2.ladot" ~flags:["frag",2] anspecies in
let _ = dump "pattern3.ladot" ~flags:["frag",3] anspecies in
let _ = dump "pattern4.ladot" ~flags:["frag",4] anspecies in
()

let pattern = filter species ["pattern",1]
let _, pattern = add_bound sp_egfr1_l pattern
let _ = dump "pattern.ladot" pattern
(* SPECIES + CM *)


let trans_sp = translate_graph {abscisse = -9.;ordinate =  0.} species
let sigma_cm,sigma_sp,trans_sp_with_cm = disjoint_union contact_map trans_sp


let proj =
  List.rev_map
    (fun (x,y) -> lift_agent sigma_sp x,lift_agent sigma_cm y)
    [sp_grb22,cm_grb2;
     sp_grb21,cm_grb2;
     sp_egfr2,cm_egfr;
     sp_egfr1,cm_egfr;
     sp_egf2,cm_egf;
     sp_egf1,cm_egf;
     sp_shc2,cm_shc;
     sp_shc1,cm_shc;
     sp_sos1,cm_sos]

let proj_inv = List.map (fun (x,y) -> (y,x)) proj

let species_cm =
  add_proj
    proj
    trans_sp_with_cm

let _ = dump "egfr_embed.ladot"   species_cm



let trans_pattern =
  translate_graph {abscisse =(-1.5);ordinate = 5.5} pattern

let sigma_species, sigma_pattern,
    trans_pattern_with_species =
  disjoint_union
    species trans_pattern

let proj =
  List.rev_map
    (fun x -> lift_agent sigma_pattern x,lift_agent sigma_species x)
    [sp_egfr2;
     sp_egfr1;
     sp_shc2;
     sp_shc1;
    ]

let sp_pattern = add_proj proj trans_pattern_with_species

let trans_pattern =
  translate_graph {abscisse =(-3.);ordinate = 5.5} (vertical_swap pattern)

let sigma_species, sigma_pattern,
    trans_pattern_with_species =
  disjoint_union
    species trans_pattern
let proj2 =
  List.rev_map
    (fun (x,y) -> lift_agent sigma_pattern x,lift_agent sigma_species y)
    [sp_egfr2,sp_egfr1;
     sp_egfr1,sp_egfr2;
     sp_shc2,sp_shc1;
     sp_shc1,sp_shc2;
    ]

let sp_pattern2 = add_proj proj2 ~color:"red" trans_pattern_with_species

let _ = dump "embed.ladot"   sp_pattern
let _ = dump "embed2.ladot"   sp_pattern2
