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
open Signature_egfr

(* chemical species*)
let _,init = Config.init {Signature_egfr.config with
                          rule_length = 0.8 ;
                         site_colors = ["cyan";"yellow";"pink";"purple";"green";"darkgreen"];}
(*signature*)

let phosphorylable=["u",[];"p",[]]

let
  [
    protein,
    [site_gauche,[gauche_u;gauche_p];
     site_droit,[droit_u;droit_p]];
    ],
  signature_toy
  =
  add_in_signature
    [
      "P",[Width 0.8;Height 0.8;Shape "square";FillColor "\"#ffffff\""],
      [
        "g",[Direction w],phosphorylable;
        "d",[Direction e],phosphorylable
      ]]
    init
