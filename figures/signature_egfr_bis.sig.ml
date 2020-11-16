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


let config =
  {Signature_egfr.config with
      rule_length = 2. ;
}

let width i = Signature_egfr.width i
let height i = Signature_egfr.height i
let ws = Signature_egfr.ws

(* chemical species*)
let init = Signature_egfr.init

(*signature*)

let phosphorylable= Signature_egfr.phosphorylable

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
  signature_egfr_bis
  =
  add_in_signature
    [
      "EGF",[Width 0.8;Height 0.8;Shape "square";FillColor Signature_egfr.color_egf],
      [
	"r",[Direction s],[]
      ];
      "EGFR",[Width 1.2;Height 1.;Shape "hexagon";FillColor Signature_egfr.color_egfr],
      [
	"l",[],[];
 "r",[],[];
 "c",[],[];
 "n",[],[];
 "Y48",[],phosphorylable;
 "Y68",[],phosphorylable
      ];
"ShC",[Width 1.2;Height 0.6;Shape "rectangle";FillColor Signature_egfr.color_shc],
      [
	"pi",[],[];
 "Y7",[],phosphorylable
      ];
      "Grb2",[Width 1.2;Height 0.8;Shape "hexagon";FillColor Signature_egfr.color_grb2],
      [
	"a",[],[];
	"b",[],[]
      ];
      "Sos",[Width 1.2;Height 0.8;Shape "rectangle";FillColor Signature_egfr.color_sos],
      [	"d",[],[]
      ];


      "EGF",[width 0.8;height 0.8;Shape "square";FillColor Signature_egfr.color_egf],
      [
        "r",(Direction s)::ws,[]
      ];
      "EGFR",[width 1.2;height 1.;Shape "hexagon";FillColor Signature_egfr.color_egfr],
      [
        "l",ws,[];
        "r",ws,[];
        "c",ws,[];
        "n",ws,[];
        "Y48",ws,phosphorylable;
        "Y68",ws,phosphorylable
      ];
      "ShC",[width 1.2;height 0.6;Shape "rectangle";FillColor Signature_egfr.color_shc],
      [
        "pi",ws,[];
        "Y7",ws,phosphorylable
      ];
      "Grb2",[width 1.2;height 0.8;Shape "hexagon";FillColor Signature_egfr.color_grb2],
      [
        "a",ws,[];
        "b",ws,[]
      ];
      "Sos",[width 1.2;height 0.8;Shape "rectangle";FillColor Signature_egfr.color_sos],
      [	"d",ws,[]
      ]]
    init

let build_rule ?mul =
  let h =
    match mul with
    | None -> 3.
    | Some f -> 3.*.f
  in
  build_rule ~hgap:(Some h)

let duplicate ?add r =
  let h = match add with None -> 0.29 | Some h -> h+.0.29 in
  let sigma,sigma',r = disjoint_union r (move_remanent_right_to h (vertical_swap r) r) in
  sigma, sigma', r
