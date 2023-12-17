open OCanren
open OCanren.Std

(* Appendo decreases on 1st and 3rd arguemnt *)
let rec appendo a b ab =
  conde
    [
      a === nil () &&& (b === ab);
      fresh (h t ab') (a === h % t) (h % ab' === ab) (appendo t b ab');
    ]

let rec reverso_b xs ys =
  conde
    [
      xs === Std.nil () &&& (xs === ys);
      fresh (temp h tl)
        (xs === List.cons h tl)
        (appendo temp (List.cons h (List.nil ())) ys)
        (reverso_b tl temp);
    ]

let rec reverso_f xs ys =
  conde
    [
      xs === Std.nil () &&& (xs === ys);
      fresh (h tl temp)
        (xs === List.cons h tl)
        (reverso_f tl temp)
        (appendo temp (List.cons h (List.nil ())) ys);
    ]

let rec reverso_u xs ys =
  conde
    [
      xs === Std.nil () &&& (xs === ys);
      debug_var ys OCanren.reify (function
        | [ Var _ ] ->
            fresh (temp h tl)
              (xs === List.cons h tl)
              (reverso_u tl temp)
              (appendo temp (List.cons h (List.nil ())) ys)
        | [ Value _ ] ->
            fresh (temp h tl)
              (xs === List.cons h tl)
              (appendo temp (List.cons h (List.nil ())) ys)
              (reverso_u tl temp)
        | _ -> failwith "should not happen");
    ]

(* appendo before reverso --- backward execution of reverso OK
   reverso before appendo --- forward execution of reverso OK
*)

open Tester

let run_exn eta =
  run_r
    (Std.List.prj_exn OCanren.prj_exn)
    (GT.show Std.List.ground (GT.show GT.int))
    eta

let _ =
  run_exn (-1) qr qrh
    (REPR (fun q r -> appendo q r (list ( !! ) [ 1; 2; 3; 4 ])))

let _ =
  run_exn (-1) q qh (REPR (fun q -> reverso_b q (list ( !! ) [ 1; 2; 3; 4 ])))

let _ =
  run_exn (-1) q qh (REPR (fun q -> reverso_f (list ( !! ) [ 1; 2; 3; 4 ]) q))

let _ =
  run_exn (-1) q qh (REPR (fun q -> reverso_u (list ( !! ) [ 1; 2; 3; 4 ]) q));
  run_exn (-1) q qh (REPR (fun q -> reverso_u q (list ( !! ) [ 1; 2; 3; 4 ])))

open Benchmark

let all_answers ss = ss |> OCanren.Stream.take |> ignore

let __ () =
  let list_of_5 = list ( !! ) [ 1; 2; 3; 4 ] in
  let prepare rel lst () =
    all_answers
      (OCanren.run q (fun q -> rel q lst) (fun rr -> rr#reify OCanren.prj_exn))
  in
  let samples =
    throughputN 5
      [
        ("reverso_b", prepare reverso_b list_of_5, ());
        ("reverso_f", prepare (Fun.flip reverso_f) list_of_5, ());
      ]
  in
  tabulate samples

let () =
  let list_of_4 = list ( !! ) [ 1; 2; 3; 4 ] in
  let list_of_10 = list ( !! ) (Stdlib.List.init 10 Fun.id) in
  let prepare rel lst () =
    all_answers
      (OCanren.run q (fun q -> rel q lst) (fun rr -> rr#reify OCanren.prj_exn))
  in
  let delay_in_s = 2 in
  let () =
    tabulate
    @@ throughputN delay_in_s
         [
           ("reverso_b", prepare reverso_b list_of_4, ());
           ("reverso_u", prepare reverso_u list_of_4, ());
         ]
  in
  let () =
    tabulate
    @@ throughputN delay_in_s
         [
           ("reverso_b", prepare reverso_b list_of_10, ());
           ("reverso_u", prepare reverso_u list_of_10, ());
         ]
  in
  let () =
    tabulate
    @@ throughputN delay_in_s
         [
           ("reverso_b", prepare (Fun.flip reverso_f) list_of_4, ());
           ("reverso_u", prepare (Fun.flip reverso_u) list_of_4, ());
         ]
  in
  let () =
    tabulate
    @@ throughputN delay_in_s
         [
           ("reverso_b", prepare (Fun.flip reverso_f) list_of_10, ());
           ("reverso_u", prepare (Fun.flip reverso_u) list_of_10, ());
         ]
  in
  ()
