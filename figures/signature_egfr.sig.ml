(**
 * signature_egfr.ml
 * GKappa
 * Jérôme Feret, projet Antique, INRIA Paris-Rocquencourt
 *
 * Creation: March, the 28th of 2015
 * Last modification: Time-stamp: <2015-07-05 14:11:14 feret>
 * *
 *
 * Copyright 2015 Institut National de Recherche en Informatique  * et en Automatique.  All rights reserved.
 * This file is distributed under the terms of the
 * GNU Library General Public License *)


(** GKappa is a OCAML library to help making slides in Kappa *)
(** More applications are coming soon (hopefully) *)

(** signature of the early egfr model *)
open Geometry
open Gkappa
open Config

let color_sos = "\"#ff7f50\""
let color_egf = "\"#6767f2\""
let color_egfr = "\"#709d54\""
let color_shc = "\"#b5dce6\""
let color_grb2 = "\"#e587de\""


let config =
  {config with
    agent_colors = ["cyan";"blue";"green";"purple";"darkgreen";];
    site_colors = ["white";"cyan";"yellow";"pink";"lavender";"green";"darkgreen"];
    state_colors = ["white";"black"];
    show_agent_names = true ;
    show_site_names = true ;
    show_state_names = false ;
    show_free_symbols = true ;
    color_agents = true ;
    color_sites = true ;
    color_states = true ;
    site_width = 0.4 ;
    site_height = 0.4;
    agent_width = 2. ;
    agent_height = 1. ;
    state_width = 0.1 ;
    state_height = 0.1 ;
    pi = 3.1416 ;
    free_width = 0.15 ;
    free_height = 0.1 ;
    bound_height = 0.3 ;
    rule_length = 1. ;
    rule_width = 1;
    cross_width = 5 ;
    edge_label_font = 50 ;
    link_width = 2 ;
    empty_graph = "";
    pairing_style = "dashed";
    pairing_color = "cyan";
    pairing_width = 2;
    weak_flow_color = "cyan";
    weak_flow_style = "dashed";
    flow_color = "red";
    strong_flow_color = "red";
    strong_flow_style = "";
    flow_style = "";
    binding_type_font = 15 ;
    agent_label_font = 18 ;
    site_label_font = 14;
    state_label_font = 10 ;
    txt_font = 25 ;
    rule_name_font = 20;
    dummy_font = 20;
    rule_margin = 0.6;
    flow_padding = 0.05;
    projection_width = 2;
    rule_color = "black";
    rule_style = "" ;
    projection_color = "blue";
    projection_style = "dashed";
    weak_flow_width = 1;
    flow_width = 2;
    strong_flow_width =3;
    losange = ("dotted","black"),("dashed",("blue","blue4")),("solid",("blue","blue4")),("solid",("red","red4")),("dashed",("red","red4")),("dotted","black");
    losange_corners = empty_co ;
    losange_padding = 0.5;
    rule = ("dashed","blue"),("dashed","blue4") ;
    rule_corners = empty_ru ;
 }

let width i = Width (0.9*.i)
let height i = Height (0.9*.i)
let ws = [Width (0.8*.config.site_width);
          Height (0.8*.config.site_height)
         ]
(* chemical species*)
let _,init = init {config with agent_width = 0.9*.config.agent_width}
(*signature*)

let phosphorylable=["u",[];"p",[]]

let
  [
    egf,
    [egf_r,[]];
    egfr,
    [egfr_l,[];
     egfr_r,[];
     egfr_c,[];
     egfr_n,[];
     egfr_Y48,[egfr_Y48_u;egfr_Y48_p];
     egfr_Y68,[egfr_Y68_u;egfr_Y68_p]];
    shc,
    [shc_pi,[];
     shc_Y7,[shc_Y7_u;shc_Y7_p]];
    grb2,
    [grb2_a,[];
     grb2_b,[]];
    sos,
    [sos_d,[]];
    segf,
    [segf_r,[]];
    segfr,
    [segfr_l,[];
     segfr_r,[];
     segfr_c,[];
     segfr_n,[];
     segfr_Y48,[segfr_Y48_u;segfr_Y48_p];
     segfr_Y68,[segfr_Y68_u;segfr_Y68_p]];
    sshc,
    [sshc_pi,[];
     sshc_Y7,[sshc_Y7_u;sshc_Y7_p]];
    sgrb2,
    [sgrb2_a,[];
     sgrb2_b,[]];
    ssos,
    [ssos_d,[]]],
  signature_egfr
  =
  add_in_signature
    [
      "EGF",[Width 0.8;Height 0.8;Shape "square";FillColor color_egf],
      [
	"r",[Direction s],[]
      ];
      "EGFR",[Width 1.2;Height 1.;Shape "hexagon";FillColor color_egfr],
      [
	"l",[Direction n],[];
 "r",[Direction (of_degree 45.)],[];
 "c",[Direction e],[];
 "n",[Direction (of_degree 135.)],[];
 "Y48",[Direction w],phosphorylable;
 "Y68",[Direction (of_degree (to_degree sw-.10.))],phosphorylable
      ];
"ShC",[Width 1.2;Height 0.6;Shape "rectangle";FillColor color_shc],
      [
	"pi",[],[];
 "Y7",[],phosphorylable
      ];
      "Grb2",[Width 1.2;Height 0.8;Shape "hexagon";FillColor color_grb2],
      [
	"a",[],[];
	"b",[],[]
      ];
      "Sos",[Width 1.2;Height 0.8;Shape "rectangle";FillColor color_sos],
      [	"d",[],[]
      ];


      "EGF",[width 0.8;height 0.8;Shape "square";FillColor color_egf],
      [
        "r",(Direction s)::ws,[]
      ];
      "EGFR",[width 1.2;height 1.;Shape "hexagon";FillColor color_egfr],
      [
        "l",ws,[];
        "r",ws,[];
        "c",ws,[];
        "n",ws,[];
        "Y48",ws,phosphorylable;
        "Y68",ws,phosphorylable
      ];
      "ShC",[width 1.2;height 0.6;Shape "rectangle";FillColor color_shc],
      [
        "pi",ws,[];
        "Y7",ws,phosphorylable
      ];
      "Grb2",[width 1.2;height 0.8;Shape "hexagon";FillColor color_grb2],
      [
        "a",ws,[];
        "b",ws,[]
      ];
      "Sos",[width 1.2;height 0.8;Shape "rectangle";FillColor color_sos],
      [	"d",ws,[]
      ]]
    init

let build_rule ?mul =
  let h =
    match mul with
    | None -> 0.9
    | Some f -> 0.9*.f
  in
  build_rule ~hgap:(Some h)

let duplicate ?add r =
  let h = match add with None -> 0.29 | Some h -> h+.0.29 in
  let sigma,sigma',r = disjoint_union r (move_remanent_right_to h (vertical_swap r) r) in
  sigma, sigma', r
