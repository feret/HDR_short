
(**
 * dna.ml
 * GKappa
 * Jérôme Feret, projet Antique, INRIA Paris-Rocquencourt
 *
 * Creation:                      <2015-03-28 feret>
 * Last modification: Time-stamp: <2015-10-14 15:10:37 feret>
 * *
 *
 * Copyright 2015 Institut National de Recherche en Informatique  * et en Automatique.  All rights reserved.
 * This file is distributed under the terms of the
 * GNU Library General Public License *)


(** GKappa is a OCAML library to help making slides in Kappa *)
(** More applications are coming soon (hopefully) *)

(** Bidirectional gliding rule in DNA repair model *)

open Geometry
open Gkappa
open Figanr_config

(* chemical species*)
let _,init = init config
(*signature*)

let see = of_degree ((to_degree s)+.(to_degree sw)/.2.)

let
  [
    dna,
    [base,[];
     e5,[];
     e3,[];
     dna_dg,[]];
    dg,
    [dbd,[];
     cat,[]]],
  signature_dna
  =
  add_in_signature
    [
      "$\text{\agent{ADN}{}}$",
      [Width 1.2;Height 0.8;Shape "hexagon";(*FillColor "\"#6767f2\""*)],
      [
        "$\text{\site{base}{}{}}$",[Direction s],[];
        "$\text{\site{e5}{}{}}$",[Direction w],[];
        "$\text{\site{e3}{}{}}$",[Direction e],[];
        "$\text{\site{dg}{}{}}$",[Direction n],[];
      ];
      "$\text{\agent{DG}{}}$",[Width 0.6;Shape "oval";(*FillColor "\"#709d54\""*)],
      [
        "$\text{\site{dbd}{}{}}$",[Width 0.6; Direction s],[];
        "$\text{\site{cat}{}{}}$",[Width 0.6;Direction (of_degree (-10.))],[];
      ]]
    init
let empty = signature_dna

(*SPECIES*)

let
  [ dna1,[s_e3,_;dg1,_];
    dna2,[s_e5,_;dg2,_]], domain
  =
  add_in_graph
    [
      dna,1.8,14.2,[],
      [e3,[],[];dna_dg,[],[]];
      dna,4.0,14.2,[],
      [e5,[],[];dna_dg,[],[]]]
    empty

let domain = add_link_list [s_e3,s_e5] domain

let extend x_pos binding_site free_site g =
  let
    [dg,[dbd,_;cat,_]], g =
    add_in_graph
      [dg,x_pos,15.8,[],[dbd,[],[];cat,[],[Free_site []]]]
      g
  in
  let _,g = add_free_list [free_site,[]] g in
  let g = add_link_list [binding_site,dbd] g  in
  ([],[],[]),g

let reversible = Some true
let rule =
  build_rule
    ~file:"dna_rule.ladot" ?reversible
    domain
    (extend 1.8 dg1 dg2)
    (extend 4.0 dg2 dg1)
