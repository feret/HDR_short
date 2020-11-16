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
                          site_width = 0.9*. config.site_width ;
                          site_height =  0.9*. config.site_height ;
                          agent_colors = ["white";"lavender"];
                          site_colors = ["cyan";"yellow";"pink";"#b5dce6";"green";"darkgreen"];}
(*signature*)


let
  [
    protein,
    [site_gauche,[];
     site_droit,[]];
    ],
  signature_polymer
  =
  add_in_signature
    [
      "A",[Width 0.9;Height 0.8;Shape "ellipse";],
      [
        "g",[Direction sw],[];
        "d",[Direction se],[];
      ]]
    init
