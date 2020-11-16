open Config
open Geometry
open Sig_kappa

let init_0 config = init {config with site_colors = ["white";"cyan"]}

let init_1 config =
  init {config with site_colors = ["white"]}

let
    [
      a,
      [
	a_x,[];
 a_y,[];
 a_x_op,[];
 a_y_op,[];
      ]],
  signature_sym
  =
  add_in_signature
    [
      "$\text{\agent{A}{}}$",[Width 0.8;Height 0.8;Shape "square";],
      [
        "$\text{\site{x}{}{}}$",[Direction (of_degree 60.)],[];
        "$\text{\site{y}{}{}}$",[Direction (of_degree 120.)],[];
        "$\text{\site{x}{}{}}$",[Direction (of_degree 120.)],[];
        "$\text{\site{y}{}{}}$",[Direction (of_degree 60.)],[];
      ]
    ]
    (snd (init_0 config))

let [sa,[sax,_;say,_]],g_a =
  add_in_graph
    [a,0.,0.,[],[a_x,[],[];a_y,[],[]]]
    signature_sym

let [sa_op,[sax_op,_;say_op,_]],g_a_op =
  add_in_graph
    [a,0.,0.,[],[a_y_op,[],[];a_x_op,[],[]]]
    signature_sym

let g_a_free ~op =
  let g_a,sax,say =
    if op
    then
      g_a_op, sax_op, say_op
    else
      g_a,sax,say
  in
  snd (add_free_list [sax,[];say,[]] g_a)

let _ = dump "sym_4_1_a.ladot" (g_a_free ~op:false)
let _ = dump "sym_4_1_a_op.ladot" (g_a_free ~op:true)

let g_a_free_free ~op1 ~op2 =
  let g1 =
    g_a_free ~op:op1
  in
  let g2 =
    g_a_free ~op:op2
  in
  let g2 = move_remanent_right_to 0.3 (vertical_swap g2) g1 in
  let _,_,g = disjoint_union g1 g2 in
  g


let g_a_x_free = (snd (add_free sax g_a))
let g_a_y_free = (snd (add_free say g_a))
let g_a_x_free_op = (snd (add_free sax_op g_a_op))
let g_a_y_free_op = (snd (add_free say_op g_a_op))

let g_a_x_free ~op =
  if op then g_a_x_free_op else g_a_x_free

let g_a_y_free ~op =
  if op then g_a_y_free_op else g_a_y_free

let build_g site1 site2 ~op1 ~op2 =
  let gl,site1 =
    if site1 == sax
    then g_a_y_free ~op:op1,sax
    else g_a_x_free ~op:op1,say
  in
  let gr,site2 =
    if site2 == sax
    then g_a_y_free ~op:op2,sax
    else g_a_x_free ~op:op2,say
  in
  let g = move_remanent_right_to 0.3 (vertical_swap gr) gl in
  let inj1,inj2,g = disjoint_union gl g in
  add_link_list [lift_site inj1 site1,lift_site inj2 site2] g

let g_a_xx ~op1 ~op2 = build_g sax sax ~op1 ~op2
let g_a_yy ~op1 ~op2 = build_g say say ~op1 ~op2
let g_a_xy ~op1 ~op2 = build_g sax say ~op1 ~op2
let g_a_yx ~op1 ~op2 = build_g say sax ~op1 ~op2
let _ = dump "sym_4_1_b.ladot" (g_a_xx ~op1:false ~op2:false)
let _ = dump "sym_4_1_c.ladot" (g_a_yy ~op1:false ~op2:false)
let _ = dump "sym_4_1_d.ladot" (g_a_xy ~op1:false ~op2:false)

let l g = (fun _ -> ([],[],[]),g)




let buildrule k kd lhs rhs file  =
  let _,_,_,rule = build_rule ~reversible:true signature_sym (l lhs) (l rhs) in
  let txt = insert_text_here ("$@\; "^k^","^kd^"$") 0. 0. signature_sym in
  let txt  = move_remanent_right_to 0.6 txt rule in
  let _,_,rule = disjoint_union rule txt in
  let () = dump ("sym_4_2_"^file^".ladot") rule in
  let _,_,_,rule = build_rule signature_sym (l lhs) (l rhs) in
  let () = dump ("sym_4_2_asso_"^file^".ladot") rule in

  let txt = insert_text_here ("$@\; "^k^"$") 0. 0. signature_sym in
  let txt  = move_remanent_right_to 0.4 txt rule in
    let _,_,rule = disjoint_union rule txt in
  let () = dump ("sym_4_2_asso_"^file^"_rate.ladot") rule in
  let _,_,_,rule = build_rule  signature_sym (l rhs) (l lhs) in
    let () = dump ("sym_4_2_diss_"^file^".ladot") rule in
  let txt = insert_text_here ("$@\; "^k^"$") 0. 0. signature_sym in
  let txt  = move_remanent_right_to 0.4 txt rule in
  let _,_,rule = disjoint_union rule txt in
  let () = dump ("sym_4_2_diss_"^file^"_rate.ladot") rule in
  let () = dump ("sym_4_2_dimer_"^file^".ladot") rhs in
  ()

let permute_rate k ~op1 ~op2 =
  let single k op =
    if op then
      match k with
      | "\kxx" -> "\kxy"
      | "\kxy" -> "\kyy"
      | "\kyy" -> "\kxy"
      | "\kdxx" -> "\kdxy"
      | "\kdxy" -> "\kdyy"
      | "\kdyy" -> "\kdxy"
      | _ -> k
    else k
  in
  single (single k op1) op2
let do_it ~op1 ~op2 =
  let ext s =
    Printf.sprintf "%s_%s_%s" s
      (if op1 then "inv" else "id")
      (if op2 then "inv" else "id")
  in
  let _ =
    buildrule
      (permute_rate "\kxx" ~op1 ~op2)
      (permute_rate "\kdxx" ~op1 ~op2)
      (g_a_free_free ~op1 ~op2)
      (g_a_xx ~op1 ~op2)(ext "a") in
  let _ =
    buildrule
      (permute_rate "\kyy" ~op1 ~op2)
      (permute_rate "\kdyy" ~op1 ~op2)
      (g_a_free_free ~op1 ~op2) (g_a_yy ~op1 ~op2)  (ext "b") in
  let _ =
    buildrule
      (permute_rate "\kxy" ~op1 ~op2)
      (permute_rate "\kdxy"  ~op1 ~op2)
      (g_a_free_free ~op1 ~op2) (g_a_xy ~op1 ~op2) (ext "c") in
  let _ =
    buildrule
      (permute_rate "\kxy" ~op1 ~op2)
      (permute_rate "\kdxy" ~op1 ~op2)
      (g_a_free_free ~op1 ~op2) (g_a_yx ~op1 ~op2) (ext "d") in
  ()

let b = [true;false]
let () =
  List.iter
    (fun op1 ->
       List.iter
         (fun op2 ->
            do_it ~op1 ~op2)
    b) b



let [sa,[]],g_a_empty =
  add_in_graph
    [a,0.,0.,[],[]]
    signature_sym

    (*
let buildrule k site1 site2 file =
  let s1,gleft = add_site sa site1 g_a_empty in
  let s2,gright = add_site sa site2 g_a_empty in
  let gright = move_remanent_right_to 0.2 (vertical_swap gright) gleft in
  let inj1,inj2,g_a_dimer = disjoint_union gleft gright  in

  let add_left g =
    ([],[],[]),snd (add_free_list [lift_site inj1 s1,[];lift_site inj2 s2,[]] g)
  in
  let add_right g =
    ([],[],[]),add_link_list [lift_site inj1 s1,lift_site inj2 s2] g
  in
  let _,_,_,rule = build_rule ~directives:[Scale 0.8] g_a_dimer add_left add_right in
  let txt = insert_text_here ("$@\; "^k^"$") 0. 0. signature_sym in
  let txt  = move_remanent_right_to 0.2 txt rule in
  let _,_,rule = disjoint_union rule txt in
   dump ("sym_4_21_"^file^".ladot") rule

let _ = buildrule "k_1" a_x a_x "a"
let _ = buildrule "k_2" a_y a_y "b"
let _ = buildrule "k_3" a_x a_y "c"
let _ = buildrule "k'_3" a_y a_x "d"
*)
let
    [
      a,
      [
	a_x,[];
	a_y,[];
      ]],
  signature_sym
  =
  add_in_signature
    [
      "$\text{\agent{A}{}}$",[Shape "square";Width 0.8],
      [
	  "$$",[Direction (of_degree 60.)],[];
          "$$",[Direction (of_degree 120.)],[]]]
    (snd (init_1 config))

let [sa,[sax,_;say,_]],g_a =
  add_in_graph
    [a,0.,0.,[],[a_x,[],[];a_y,[],[]]]
    signature_sym

let g_a_free =
  snd (add_free_list [sax,[];say,[]] g_a)

let _ = dump "sym_4_9_a.ladot" g_a_free

let g_a_free_free =
   let g = move_remanent_right_to 0.2 (vertical_swap g_a_free) g_a_free in
  let inj1,inj2,g = disjoint_union g_a_free g in
  g

let g_a_x_free = (snd (add_free sax g_a))
let g_a_y_free = (snd (add_free say g_a))

let build_g site1 site2=
  let gl =
    if site1 == sax
    then g_a_y_free
    else g_a_x_free
  in
  let gr =
    if site2 == sax
    then g_a_y_free
    else g_a_x_free
  in
  let g = move_remanent_right_to 0.2 (vertical_swap gr) gl in
  let inj1,inj2,g = disjoint_union gl g in
  add_link_list [lift_site inj1 site1,lift_site inj2 site2] g

let g_a_xx = build_g sax sax
let _ = dump "sym_4_9_b.ladot" g_a_xx


let l g = (fun _ -> ([],[],[]),g)
let buildrule k kd lhs rhs file =
  let _,_,_,rule = build_rule (*~hgap:(Some 0.5)*)
      ~reversible:true ~directives:[Width 0.8] signature_sym (l lhs) (l rhs) in
  let txt = insert_text_here ("$@\, "^k^","^kd^"$") 0. 0. signature_sym in
  let txt  = move_remanent_right_to 0.6 txt rule in
  let _,_,rule = disjoint_union rule txt in
  dump file rule

let _ = buildrule "\K" "\KD" g_a_free_free g_a_xx "sym_4_11.ladot"

let
    [
      a,
      [
	a_x,[a_x_u;a_x_p];
	a_y,[a_y_u;a_y_p];
	a_z,_
      ];
      b,[b_u,[];b_v,[];b_w,[]]]
      ,  signature_sym
  =
  add_in_signature
    [
      "$\text{\agent{A}{}}$",[Shape "square";Width 0.8],
      [
	  "$\text{\site{x}{}{}}$",[Direction nw],["$u$",[Scale scale_internal_state];"$p$",[Scale scale_internal_state]];
          "$\text{\site{y}{}{}}$",[Direction sw],["$u$",[Scale scale_internal_state];"$p$",[Scale scale_internal_state]];
          "$\text{\site{z}{}{}}$",[Direction (of_degree 90.)],[]];
      "$\text{\agent{B}{}}$",[Shape "square";Width 0.8],
      [
	  "$\text{\site{u}{}{}}$",[Direction nw],[];
	 "$\text{\site{v}{}{}}$",[Direction nw],[];
	  "$\text{\site{w}{}{}}$",[Direction nw],[];
      ]]
    (snd (init_0 config))

let str x = if x then "p" else "u"


let dimer file x1 y1 x2 y2 =
  let [
    a1,[a1_x,_;a1_y,_;a1_z,_]
  ],left_agent
      =
    add_in_graph
      [a,0.,0.,[],
       [a_x,[],[];
	a_y,[],[];
	a_z,[Direction (of_degree 60.)],[]]]
      signature_sym
  in
  let right_agent = vertical_swap left_agent in
  let left_agent = insert_text_here ~directives:[Fontsize 10] "$1$" 0.15 (-0.1) left_agent in
  let right_agent = insert_text_here ~directives:[Fontsize 10] "$2$" (-0.15) (-0.1) right_agent in
  let right_agent = move_remanent_right_to 0.3 right_agent left_agent in
  let left,right,dimer = disjoint_union left_agent right_agent in
  let dimer = add_link_list [lift_site left a1_z,lift_site right a1_z] dimer in
  let add_site_state dimer (site,side,bool,u,p) =
    snd (
      add_internal_state
	(lift_site side site)
	(if bool then p else u)
	dimer)
  in
  let dimer =
    List.fold_left
      add_site_state
      dimer
      [a1_x,left,x1,a_x_u,a_x_p;
       a1_y,left,y1,a_y_u,a_y_p;
       a1_x,right,x2,a_x_u,a_x_p;
       a1_y,right,y2,a_y_u,a_y_p]
  in
  let _ = dump file dimer in
  dimer

let sigma = "\{2\}"
let domain = "E"
let codomain = "F"
let embedding = "f"

let create bool =
  let [a,_],gE =
    add_in_graph
      [a,0.,0.,[],
       [a_y,[],
	[Internal_state ((if bool then a_y_u else a_y_p),[])]]]
      signature_sym
  in
  let a_x,gF= add_site a a_x gE in
  let _,gF = add_internal_state a_x (if bool then a_x_p else a_x_u) gF
  in
  let gF = insert_text_on_boarder ~padding:1.3
    (if bool then ["$\actiongraphlong{"^sigma^"}{"^codomain^"}$",ne]
     else ["$"^codomain^"$",nw])
    gF
  in
  let gF = insert_text_here ~directives:[Fontsize 10] "$2$" 0.15 (-0.1) gF in
  let gE = insert_text_on_boarder ~padding:1.4
    (if bool then ["$\actiongraphlong{(\actionembactionlong{"^sigma^"}{"^embedding^"})}{"^domain^"}$",se]
     else ["$"^domain^"$",sw])
    gE
  in
  let gE = insert_text_here ~directives:[Fontsize 10] "$1$" 0.15 (-0.1) gE in
  let gF = move_remanent_above 1.5 gF gE in
  let inj1,inj2,g = disjoint_union gE gF in
  let g = add_proj
    ~name:(Some [Some (if bool then ("$\hspace*{130mm}\actionembemblong{"^sigma^"}{"^embedding^"}$") else "$\hspace*{40mm}"^embedding^"$")])
    [lift_agent inj1 a,lift_agent inj2 a] g in
  dump ("sym_exa_"^(if bool then "right" else "left")^".ladot") g

let _ = create true
let _ = create false

let count n =
  let rec aux k l =
    if k=0 then Some l
    else aux (k-1) (false::l)
  in
  aux n []

let rec succ l =
  match l
  with
    [] -> None
  | false::q -> Some (true::q)
  | true::q ->
    match succ q
    with
      None -> None
    | Some rep -> Some (false::rep)

let rec aux init =
    match init
    with None -> ()
    | Some l ->
      let [x1;x2] = l in
      let _ = dimer
	("sym_dimer1_"^(str x1)^(str x2)^".ladot")
	x1 (not x1) x2 (not x2) in
      aux (succ l)

let _ = aux (count 2)
let _ = dimer "sym_dimer1_uu_pp.ladot" false false true true

let [a1,_],left_agent
  =
  add_in_graph
    [a,0.,0.,[],
     [a_x,[],[Internal_state (a_x_u,[])];
      a_y,[],[Internal_state (a_y_p,[])]]]
    signature_sym
let right_agent = vertical_swap left_agent
let right_agent = move_remanent_right_to 0.4 right_agent left_agent

let [a2,_],left_agent'
  =
  add_in_graph
    [a,0.,0.,[],
     [a_x,[],[Internal_state (a_x_p,[])];
      a_y,[],[Internal_state (a_y_u,[])]]]
    signature_sym
let right_agent' = vertical_swap left_agent'
let right_agent' = move_remanent_right_to 0.4 right_agent' left_agent
let make_dimer_embedding bdimer left_agent a1 right_agent a2 bool bool2 col =
  let left,right,dimer1 = disjoint_union left_agent right_agent in
  let dimers,l =
    if bool
    then
      let a1_z,dimer2 = add_site (lift_agent left a1) a_z dimer1 in
      let a2_z,dimer2 = add_site (lift_agent right a1) a_z ~directives:[Direction w] dimer2 in
      let dimer2 = add_link_list [a1_z,a2_z] dimer2 in
      let dimer2 =
	if bool2
	then
	  color "red" dimer2
	else
	  dimer2
      in
      let d1 = if bdimer then dimer1 else translate_graph {abscisse=(-0.5);ordinate=0.} right_agent in
      let dimer2 = move_remanent_above 1. dimer2 d1 in
      let bottom,top,dimers = disjoint_union d1 dimer2 in
      dimers,
      ((if bdimer
	then
	  (fun x ->
	   ((lift_agent bottom (lift_agent left a1),
	     lift_agent top (lift_agent left a1))::x))
	else (fun x ->x))
	 [lift_agent bottom ((if bdimer then lift_agent right else (fun x->x)) a2),
	  lift_agent top (lift_agent right a2)])

    else
      let h1,dimers = add_empty_node  0. 1.69819 (if bdimer then dimer1 else right_agent) in
      let dimers = put_a_cross (-0.1) 0.7 0.1 0.9 dimers in
      if bdimer then
	 let h2,dimers = add_empty_node  0.84 1.69819 dimers in
	 let dimers = put_a_cross (0.7) 0.7 0.9 0.9 dimers in
	 dimers,[lift_agent left a1,h1;lift_agent right a2,h2]
      else
	dimers,[lift_agent left a1,h1]
  in
  let dimers = add_proj ~color:col l dimers
  in
  dimers



let dimer1 = make_dimer_embedding true left_agent a1 right_agent a1 true false "blue"
let dimer2 = make_dimer_embedding true left_agent a1 right_agent' a2 true true "red"
let dimer3 = make_dimer_embedding true left_agent a1 right_agent' a2 false true "red"
let dimer4 = make_dimer_embedding false left_agent a1 right_agent a1 true false "blue"
let dimer5 = make_dimer_embedding false left_agent' a1 right_agent' a1 true true "red"
let dimer6 = make_dimer_embedding false left_agent a1 right_agent' a1 true true "red"


let dimer2 = move_remanent_right_to 0.8 dimer2 dimer1
let _,_,fo_example = disjoint_union dimer1 dimer2
let _ = dump "sym_4_17a.ladot" fo_example

let dimer3 = move_remanent_right_to 1. dimer3 dimer1
let _,_,fo_counter = disjoint_union dimer1 dimer3
let fo_counter = put_a_cross 1.6 2.0 1.8 1.8 fo_counter
let _ = dump "sym_4_17b.ladot" fo_counter

let dimer5 = move_remanent_right_to 0.8 dimer5 dimer4
let _,_,ex1 = disjoint_union dimer4 dimer5
let _ = dump "sym_4_17c.ladot" ex1

let dimer6 = move_remanent_right_to 0.8 dimer6 dimer4
let _,_,ex2 = disjoint_union dimer4 dimer6
let _ = dump "sym_4_17d.ladot" ex2




let makea ?file:(file="") bool s1 d1 s2 d2 s3 d3 s4 =
  let [ga,[gx,_;gy,_;gz,_]],g =
    add_in_graph
      [a,0.,0.,[],[s1,[Direction d1],[];s2,[Direction d2],[];s3,[Direction d3],[]]]
      signature_sym
  in
  let g = insert_text_here ~directives:[Fontsize 10] "$1$" 0.15 (-0.1) g in
  let _,g = add_bound gx g in
  let _,g = add_free gy g in
  let g =
    if bool
    then
      snd (add_binding_type gz s4 g)
    else
      g
  in
  let _ = if file = "" then () else dump file g in
  ga,g

let a1 = makea ~file:"sym_4_18_top.ladot" true a_x w a_y n a_z s b_u
let a2 = makea ~file:"sym_4_18_left.ladot" true a_z w a_y n a_x s b_v
let a3 = makea ~file:"sym_4_18_right.ladot" true a_z w a_x n a_y s b_w
let b1 = makea ~file:"sym_4_18bis_top.ladot" false a_x w a_y n a_z s b_u
let b2 = makea ~file:"sym_4_18bis_left.ladot" false a_z w a_y n a_x s b_v
let b3 = makea ~file:"sym_4_18bis_right.ladot" false a_z w a_x n a_y s b_w


let makeab ?file:(file="") bool d1 d2 d3 s1 s2 s3 s4 s5 s6 name1 name2 emb dir =
  let ga_a,ga = makea bool s1 (of_degree d1) s2 (of_degree d2) s3 (of_degree d3) s4 in
  let [gab_a,[gab_x,_;gab_y,_;gab_z,_];gab_b,[gab_u,_;gab_v,_;gab_w,_]],gab =
    add_in_graph
      [a,0.,0.,[],
       [
	 s1,[Direction (of_degree d1)],[];
	 s2,[Direction (of_degree d2)],[Free_site []];
	 s3,[Direction (of_degree d3)],[]];
       b,(-0.9),0.,[],
       [
	 s4,[Direction (of_degree (-.d3))],[];
	 s5,[Direction (of_degree (-.d2))],[Free_site []];
	 s6,[Direction (of_degree (-.d1))],[]]]
      signature_sym
  in
  let gab = insert_text_here ~directives:[Fontsize 10] "$2$" 0.15 (-0.1) gab in
  let gab = insert_text_here ~directives:[Fontsize 10] "$3$" (-1.05) (-0.1) gab in
  let gab = add_link_list [gab_z,gab_u;gab_x,gab_w] gab in
  let ga = insert_text_on_boarder ~padding:1.2 [name1,dir] ga in
  let gab = insert_text_on_boarder ~padding:1.2 [name2,dir] gab in
  let gab = move_remanent_above 1. gab ga in
  let inj1,inj2,g = disjoint_union ga gab in
  let g =
    add_proj
      ~name:(Some [Some emb])
      [
	lift_agent inj1 ga_a,
	lift_agent inj2 gab_a
      ] g in
  let _ =
    if file <> ""
    then dump file g
  in
  g

let gleft = makeab ~file:"sym_4_19_left.ladot" true (-55.) 35. 235. a_x a_y a_z b_u b_v b_w "$G$" "$H$" "$h$" w
let gright = makeab ~file:"sym_4_19_right.ladot" true (-55.) 35. 235. a_z a_y a_x b_v b_u b_w "$\hspace*{2cm}\actiongraphlong{(\actionembactionlong{\sigma_3}{h})}{G}$" "$\hspace*{1cm}\actiongraphlong{\sigma_3}{H}$" "$\hspace*{9cm}\actionemblong{\sigma_3}{h}$" (of_degree 90.)
let gright = move_remanent_right_to 1.5 gright gleft
let _,_,g = disjoint_union gleft gright
let _ = dump "sym_4_19.ladot" g

let gleft = makeab ~file:"sym_4_19bis_left.ladot" false (-55.) 35. 235. a_x a_y a_z b_u b_v b_w "$G$" "$H$" "$h$" w
let gright = makeab ~file:"sym_4_19bis_right.ladot" false (-55.) 35. 235. a_z a_y a_x b_v b_u b_w "$\hspace*{2cm}\actiongraphlong{(\actionembactionlong{\sigma_3}{h})}{G}$" "$\hspace*{1cm}\actiongraphlong{\sigma_3}{H}$" "$\hspace*{9cm}\actionembemblong{\sigma_3}{h}$" (of_degree 90.)

let gright = move_remanent_right_to 1.5 gright gleft
let _,_,g = disjoint_union gleft gright
let _ = dump "sym_4_19bis.ladot" g

let [gax_a,[gax_x,_]],gax =
  add_in_graph
    [a,0.,0.,[],[a_x,[Direction ne],[]]]
    signature_sym

let add_leftx g = ([],[],[]),snd (add_internal_state gax_x a_x_u g)
let add_rightx g = ([],[],[]),snd (add_internal_state gax_x a_x_p g)

let aux file domain add_left add_right k =
  let _,_,_,rule1 = build_rule domain add_left add_right in
  let txt = insert_text_here ("$@\; "^k^"$") 0. 0. signature_sym in
  let txt  = move_remanent_right_to 0.2 txt rule1 in
  let _,_,rule1 = disjoint_union rule1 txt in
  let _ = dump file rule1 in
  ()

let _ = aux "sym_4_20_a.ladot" gax add_leftx add_rightx "k_1"

let [gay_a,[gay_y,_]],gay =
  add_in_graph
    [a,0.,0.,[],[a_y,[Direction se],[]]]
    signature_sym

let add_lefty g = ([],[],[]),snd (add_internal_state gay_y a_y_u g)
let add_righty g = ([],[],[]),snd (add_internal_state gay_y a_y_p g)

let _ = aux "sym_4_20_b.ladot" gay add_lefty add_righty "k_2"

let _,_,_,rule3 = build_rule signature_sym
  (fun g->
    let _,gax =
      add_in_graph
	[a,0.,0.,[],
	 [
	   a_x,[Direction ne],[Internal_state (a_x_p,[])];
	   a_y,[Direction se],[Internal_state (a_y_p,[])]]]
	g
    in ([],[],[]),gax )
    (fun g -> ([],[],[]),g)
let txt = insert_text_here ("$@\; k_3$") 0. 0. signature_sym
let txt  = move_remanent_right_to 0.45 txt rule3
let _,_,rule3 = disjoint_union rule3 txt
let _ = dump "sym_4_20_c.ladot" rule3


let make b0 b1 b2 b3 name =
  let delta = 0.15 in
  let f b =
    if b then
      a_x
    else
      a_y
  in
  let ax0 = f b0 in
  let
    [
      a0,_;a1,_
    ],
    domain
    =
    add_in_graph
    [
      a,-0.8,0.,[],

      [ax0,[Direction s],[]];
      a,0.,0.,[],
      []]
    signature_sym
  in
  let add_d graph =
    let ax1,graph =
      add_site a1 a_z ~directives:[Direction n] graph in
    let _,graph = add_bound ax1 graph in
    ([],[],[]),graph
  in
  let add_h b b1 b2 graph =
    let ax = f b in
    let ax1 = f b1 in
    let [a2,_],graph =
      add_in_graph
	[a,0.8,0.,[],
	 [ax,[Direction s],[]]]
	graph
    in
    let _,graph = add_site a1 ax1 ~directives:[Direction s] graph in
    let graph =
      if b2 then
	let a2_z,graph = add_site a2 a_z ~directives:[Direction w] graph in
	let a1_z,graph = add_site a1 a_z ~directives:[Direction (of_degree 90.)] graph in
	let graph = add_link_list [a1_z,a2_z] graph in
	let graph = translate_agent {abscisse=delta;ordinate=0.} a0 graph in
	graph
      else
	let a0_z,graph = add_site a1 a_z ~directives:[Direction w] graph in
	let a1_z,graph = add_site a0 a_z ~directives:[Direction (of_degree 90.)] graph in
	let graph = add_link_list [a1_z,a0_z] graph in
	let graph = translate_agent {abscisse=(-.delta);ordinate=0.} a2 graph in
	graph
    in
    ([a0,[]],[],[]),graph
  in
  let _ = build_rule ~file:("sym_4_20_"^name^".ladot") ~rule_symb:false ~explicit:true ~hgap:(Some 0.3) ~vgap:(Some 1.) domain ~extend_domain:add_d (add_h b2 b1 true)  (add_h b3 b1 false) in
  ()
let _ = make true false true false "xyxy"

let _ = make false false true false "yyxy"
let _ = make false false true true "yyxx"

let _ = make true true true false "xxxy"
let _ = make true true false false "xxyy"

let _ = make true false false false  "xyyy"
let _ = make true false false true  "xyyx"

let _ = make true false true true  "xyxx"
let _ = make false true true false "yxxy"
