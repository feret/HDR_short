open Config
open Geometry

let show_binding_type = false
let x_a = 0.
let x_b = 0.5
let x_c = 1.1

let config =
  {
    config
    with
      site_width = 0.25*.config.site_width;
      site_height = 0.25 *. config.site_height;
      state_width = 0.5 *. config.state_width ;
      state_height = 0.5 *. config.state_height ;
      free_width = 0.6 *. config.free_width ;
      free_height = 0.4 *. config.free_height ;
      bound_height = 0.3 *. config.bound_height ;
      rule_length = 0.5 *. config.rule_length ;
      rule_margin = 0.05 *. config.rule_margin ;
      cross_width = 2 ;
      agent_colors = ["magenta";"blue";"green";"purple";"darkgreen";"yellow";"orange"];
      show_agent_names = false ;
      show_site_names = false ;
      show_state_names = false ;
      txt_font = 10;
  }

let dump f g =
  let g' = cross g in
  let _ = dump f g in
  let _ = dump ("crossed_"^f) g' in
  ()

let build_rule = build_rule ~hgap:(Some 0.8)

let lift g = (fun _ -> ([],[],[]),g)

let bind_unbind s1 s2 g =
  (lift (snd (add_free_list [s1,[];s2,[]] g))),
  (lift (add_link_list [s1,s2] g))

let shift = 20.
let ne = of_degree (to_degree ne +. shift)
let nw = of_degree (to_degree nw -. shift)
let ssw = sw
let nnw = nw
let sw = of_degree (to_degree sw +. shift)
let se = of_degree (to_degree se -. shift)

let
  [
    a,[a_r,[]];
    b,[b_l,[];b_r,[];b_x,[b_x_u;b_x_p]];
    c,[c_l,[]];
    d,[d_r,[];d_x,[d_x_u;d_x_p]];
    e,[e_l,[];e_x,[e_x_u;e_x_p]];
    f,[f_x,[];f_y,[]];
    g,[g_x,[];g_y,[]];
  ],
  signature
  =
  add_in_signature
    [
      "$\text{\agent{A}{}}$",
      [
        Shape "rectangle";
        Width (0.1*.config.agent_width);
        Height (0.1*.config.agent_width)],
      [
        "$\text{\site{r}{}{}}$",[Direction e],[]
      ];

      "$\text{\agent{B}{}}$",
      [
        Shape "rectangle";
        Width (0.15*.config.agent_width);
        Height (0.1*.config.agent_width)],
      [
        "$\text{\site{l}{}{}}$",[Direction w],[];
        "$\text{\site{r}{}{}}$",[Direction se],[];
        "$\text{\site{x}{}{}}$",[Direction ne],["u",[];"p",[]]
      ];

      "$\text{\agent{C}{}}$",
      [
        Shape "ellipse";
        Width (0.15*.config.agent_width);
        Height (0.1*.config.agent_width)],
      [
        "$\text{\site{l}{}{}}", [Direction ssw],[]
      ];

      "$\text{\agent{D}{}}$",
      [
        Shape "rectangle";
        Width (0.15*.config.agent_width);
        Height (0.1*.config.agent_width)],
      [
        "$\text{\site{r}{}{}}$",[Direction se],[];
                                              "$\text{\site{x}{}{}}$",[Direction ne],["u",[];"p",[]]];

      "$\text{\agent{E}{}}$",
      [
        Shape "ellipse";
        Width (0.15*.config.agent_width);
        Height (0.1*.config.agent_width)],
        [
          "$\text{\site{l}{}{}}$",[Direction ssw],[];
          "$\text{\site{x}{}{}}$",[Direction nnw],["u",[];"p",[]]];
      "$\text{\agent{F}{}}$",
      [
        Shape "rectangle";
        Width (0.13*.config.agent_width);
        Height (0.13*.config.agent_width)
      ],
      [
        "$\text{\site{x}{}{}}$",[Direction ne],[];
        "$\text{\site{y}{}{}}$",[Direction se],[]];
      "$\text{\agent{G}{}}$",
        [
          Shape "rectangle";
          Width (0.13*.config.agent_width);
          Height (0.13*.config.agent_width)
        ],
        [
          "$\text{\site{x}{}{}}$",[Direction nw],[];
          "$\text{\site{y}{}{}}$",[Direction sw],[]];
    ]
    (snd (init config))
