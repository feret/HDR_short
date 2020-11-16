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

let scale_internal_state = 1.2

let
    [
      e,
      [
	e_r,[]
      ];
      r,
      [
	r_a,[r_a_u;r_a_p];
	r_b,[r_b_u;r_b_p];
	r_e,[];
	r_r,[]
      ]
    ],
  signature_er
  =
  add_in_signature
    [
      "$\text{\agent{E}{}}$",[Shape "ellipse";Width (0.9*.config.agent_width)],
      [
	  "$\text{\site{r}{}{}}$",[Direction w],[]];
	"$\text{\agent{R}{}}$",[Shape "square";Width (1.1*.config.agent_width)],
	[
	  "$\text{\site{a}{}{}}$",[Direction n],["$u$",[Scale scale_internal_state];"$p$",[Scale scale_internal_state]];
	  "$\text{\site{b}{}{}}$",[Direction n],["$u$",[Scale scale_internal_state];"$p$",[Scale scale_internal_state]];
	  "$\text{\site{e}{}{}}$",[Direction n],[];
	  "$\text{\site{r}{}{}}$",[Direction e],[]
	]]
    (snd (init config))
