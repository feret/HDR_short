open Config
open Geometry
open Sig_kappa

let init_0 config = init {config with site_colors = ["white";"cyan"];
                                      agent_colors = ["\"#6767f2\""]}

let init_1 config =
  init {config with site_colors = ["white"]}

let e = of_degree 90.

let
    [
      a,
      [
        a_b,[];
      ];
      b,
      [ b_a,[];
        b_c,[]];
      c,
      [c_b,[]]],
  signature_sym
  =
  add_in_signature
    [
      "$\text{\agent{A}{}}$",[Width 0.8;Height 0.8;Shape "square";],
      [
        "$\text{\site{b}{}{}}$",[Direction e],[];
      ];
      "$\text{\agent{B}{}}$",[Width 0.8;Height 0.8;Shape "circle";],
      [
        "$\text{\site{a}{}{}}$",[Direction w],[];
        "$\text{\site{a}{}{}}$",[Direction e],[];
      ];
      "$\text{\agent{C}{}}$",[Width 0.8;Height 0.8;Shape "square";],
      [
        "$\text{\site{b}{}{}}$",[Direction w],[];
      ];
    ]
    (snd (init_0 config))

let string_of_bool_opt =
  function
  | None -> "unknown"
  | Some true -> "bound"
  | Some false -> "false"

let make_b ~sitea ~sitec =
  let [protein_b,_],g =
    add_in_graph [b,0.,0.,[],[]] signature_sym
  in
  let g =
    match sitea with
    | None -> g
    | Some bool  ->
      let b_a, g =
        add_site protein_b b_a g
      in
      if bool then
        let [a,[a_b,_]],g =
          add_in_graph
            [a,-.1.9,0.,[],[a_b,[],[]]]
            g
        in
        let g = add_link_list [a_b,b_a] g in
        g
      else
        let _,g = add_free b_a g in
        g
  in
  let g =
    match sitec with
    | None -> g
    | Some bool  ->
      let b_c, g =
        add_site protein_b b_c g
      in
      if bool then
        let [c,[c_b,_]],g =
          add_in_graph
            [c,1.9,0.,[],[c_b,[],[]]]
            g
        in
        let g = add_link_list [c_b,b_c] g in
        g
      else
        let _,g = add_free b_c g in
        g
  in
  let s =
    Format.sprintf
      "chp5_b_%s_%s.ladot"
      (string_of_bool_opt sitea)
      (string_of_bool_opt sitec)
  in
  let () = dump s g in
  g

let state = [None;Some true;Some false]
let () =
  List.iter
    (fun sitea ->
       List.iter
         (fun sitec ->
            let _ = make_b ~sitea ~sitec in ())
         state)
    state

let l g = (fun _ -> ([],[],[]),g)

let bind_a ~sitec =
  let bc =
    make_b ~sitea:(Some false) ~sitec
  in
  let [_,[sa,_]],a_bc =
    add_in_graph
      [a,-1.9,0.,[],[a_b,[],[]]]
      bc
  in
  let _,a_bc = add_free sa a_bc in
  let abc =
    make_b ~sitea:(Some true) ~sitec
  in
  let s =
    Format.sprintf
      "chp5_bind_AB_%s.ladot"
      (string_of_bool_opt sitec)
  in
  let abc = l abc in
  let a_bc = l a_bc in
  let _,_,_,rule =
    build_rule
      ~directives:[Width 0.8] signature_sym a_bc abc
  in
  let txt = insert_text_here ("$@\, \\ka$") 0. 0. signature_sym in
  let txt  = move_remanent_right_to 0.6 txt rule in
  let _,_,rule = disjoint_union rule txt in
  let () = dump s rule in
  let s =
    Format.sprintf
      "chp5_unbind_AB_%s.ladot"
      (string_of_bool_opt sitec)
  in
  let _,_,_,rule =
    build_rule
      ~directives:[Width 0.8] signature_sym abc a_bc
  in
  let txt = insert_text_here ("$@\, \\kda$") 0. 0. signature_sym in
  let txt  = move_remanent_right_to 0.6 txt rule in
  let _,_,rule = disjoint_union rule txt in
  let () = dump s rule in
  let s =
    Format.sprintf
      "chp5_bindunbind_AB_%s.ladot"
      (string_of_bool_opt sitec)
  in
  let _,_,_,rule =
    build_rule
      ~reversible:true
      ~directives:[Width 0.8] signature_sym a_bc abc
  in
  let txt = insert_text_here ("$@\, \\ka,\\kda$") 0. 0. signature_sym in
  let txt  = move_remanent_right_to 0.6 txt rule in
  let _,_,rule = disjoint_union rule txt in
  let () = dump s rule in
  ()

let bind_c ~sitea =
  let ab =
    make_b ~sitec:(Some false) ~sitea
  in
  let [_,[sc,_]],ab_c =
    add_in_graph
      [c,1.9,0.,[],[c_b,[],[]]]
      ab
  in
  let _,ab_c = add_free sc ab_c in
  let abc =
    make_b ~sitec:(Some true) ~sitea
  in
  let ab_c = l ab_c in
  let abc = l abc in
  let s =
    Format.sprintf
      "chp5_bind_BC_%s.ladot"
      (string_of_bool_opt sitea)
  in
  let _,_,_,rule =
    build_rule
      ~directives:[Width 0.8] signature_sym ab_c abc
  in
  let txt = insert_text_here ("$@\, \\kc$") 0. 0. signature_sym in
  let txt  = move_remanent_right_to 0.6 txt rule in
  let _,_,rule = disjoint_union rule txt in
  let () = dump s rule in
  let s =
    Format.sprintf
      "chp5_unbind_BC_%s.ladot"
      (string_of_bool_opt sitea)
  in
  let _,_,_,rule =
    build_rule
      ~directives:[Width 0.8] signature_sym abc ab_c
  in
  let txt = insert_text_here ("$@\, \\kdc$") 0. 0. signature_sym in
  let txt  = move_remanent_right_to 0.6 txt rule in
  let _,_,rule = disjoint_union rule txt in
  let () = dump s rule in
  let s =
    Format.sprintf
      "chp5_bindunbind_BC_%s.ladot"
      (string_of_bool_opt sitea)
  in
  let _,_,_,rule  =
    build_rule
      ~reversible:true
      ~directives:[Width 0.8] signature_sym ab_c abc
  in
  let txt = insert_text_here ("$@\, \\kc,\\kdc$") 0. 0. signature_sym in
  let txt  = move_remanent_right_to 0.6 txt rule in
  let _,_,rule = disjoint_union rule txt in
  let () = dump s rule in
  ()

let () =
  List.iter
    (fun sitec -> bind_a ~sitec)
    state

let () =
  List.iter
    (fun sitea -> bind_c ~sitea)
    state
