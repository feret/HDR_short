open Config
open Geometry
open Signature_egfr

let buf = ref None
let doit ~crossed ~egf ~cr ~c ~n =
  let [egfr,[]], g =
    add_in_graph
      [
        egfr,0.,0.,[],[]]
      signature_egfr
  in
  let g=
      insert_text_here "."  (1.125) 0. ~directives:[Width 0.0001;Height 0.0001;FontColor "white"] g
  in
  let g=
    insert_text_here "." (-.1.125) 0. ~directives:[Width 0.0001;Height 0.0001;FontColor "white"] g
    in
  let file = "r" in
  let file, g =
    match egf with
    | None -> file, g
    | Some true -> file^"_egf_bound",
                   let st , g =
                     add_site ~directives:[Direction w] egfr egfr_l g
                   in
                   let _,g =
                     add_bound  st g
                   in

                   g
    | Some false -> file^"_egf_free",
                    let st, g =
                      add_site ~directives:[Direction w] egfr egfr_l g
                    in
                    let _,g =
                      add_free_list [st,[]] g
                    in
                    g
  in
  let file, g =
    match cr with
    | None -> file, g
    | Some true -> file^"_cr_bound",
                   let st , g =
                     add_site egfr egfr_r  g
                   in
                   let _,g =
                     add_bound ~directives:[Direction e] st g
                   in

                   g
    | Some false -> file^"_cr_free",
                    let st, g =
                      add_site egfr egfr_r g
                    in
                    let _,g =
                      add_free_list [st,[Direction (of_degree 90.)]] g
                    in
                    g
  in
  let file, g =
    match c with
    | None -> file, g
    | Some true -> file^"_c_bound",
                   let st , g =
                     add_site  egfr egfr_c g
                   in
                   let _,g =
                     add_bound ~directives:[Direction (of_degree 90.)] st g
                   in

                   g
    | Some false -> file^"_c_free",
                    let st, g =
                      add_site egfr egfr_c  g
                    in
                    let _,g =
                      add_free_list [st,[]] g
                    in
                    g
  in
  let file, g =
    match n with
    | None -> file, g
    | Some true -> file^"_n_bound",
                   let st , g =
                     add_site egfr egfr_n g
                   in
                   let _,g =
                     add_bound ~directives:[Direction (of_degree 90.)] st g
                   in

                   g
    | Some false -> file^"_n_free",
                    let st, g =
                      add_site egfr egfr_n  g
                    in
                    let _,g =
                      add_free_list [st,[Direction (of_degree 90.)]] g
                    in
                    g
  in
  let file, g =
    if crossed then
      "crossed_"^file,
      cross g
    else file, g
  in
  let () = buf:=(Some g) in
  dump (file^".ladot") g

let doit_l [crossed;egf;cr;c;n] =
  match crossed with
  | None -> ()
  | Some crossed -> doit ~crossed ~egf ~cr ~c ~n

let elt = [None; Some true; Some false]
let l =
  let rec aux rep k =
    if k=0 then rep
    else
      let rep =
        List.fold_left
          (fun accu elt ->
             List.fold_left
               (fun accu suf -> (elt::suf)::accu)
               accu
               rep
          )
          [] elt
      in aux rep (k-1)
  in aux [[]] 5

let ()=  List.iter doit_l l
