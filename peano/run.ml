open OCanren
open Tester
open Addo

let all_answers = -1

let () =
  run_r Std.Nat.prj_exn (GT.show Std.Nat.ground) all_answers q qh
    ("addo1", fun b -> addo_1 (Std.nat 5) b (Std.nat 10))

(* hangs  *)
let __ () =
  run_r Std.Nat.prj_exn (GT.show Std.Nat.ground) all_answers q qh
    ("addo1", fun b -> addo_1 b (Std.nat 5) (Std.nat 10))

let () =
  run_r Std.Nat.prj_exn (GT.show Std.Nat.ground) all_answers q qh
    (REPR (fun b -> Impl2.addo (Std.nat 5) b (Std.nat 10)))

let () =
  run_r Std.Nat.prj_exn (GT.show Std.Nat.ground) all_answers q qh
    (REPR (fun a -> Impl2.addo a (Std.nat 5) (Std.nat 10)))

let all_answers ss = ss |> OCanren.Stream.take |> ignore

let __ =
  let nat_5 = Std.nat 10 in
  let nat_10 = Std.nat 20 in
  let prepare rel arg sum () =
    all_answers
      (OCanren.run q
         (fun a -> rel arg a sum)
         (fun rr -> rr#reify OCanren.prj_exn))
  in

  let samples =
    Benchmark.throughputN 5
      [
        ("addo_1", prepare addo_1 nat_5 nat_10, ());
        (* ("addo_f", prepare Impl1.addo nat_5 nat_10, ()); *)
        ("addo_f_2", prepare Impl2.addo nat_5 nat_10, ());
      ]
  in
  Benchmark.tabulate samples

(*
include struct
  open Direction1

  let () = print_endline "First ordering of conjuction arguments"

  let () =
    run_r (Std.List.prj_exn prj_exn)
      (GT.show Std.List.ground (GT.show GT.int))
      all_answers q qh
      ( "Backward: asking for all answers works",
        fun xs -> reverso xs (Std.list ( !! ) [ 1; 2; 3 ]) )

  (* Asking all answers wil hang  *)
  let () =
    run_r (Std.List.prj_exn prj_exn)
      (GT.show Std.List.ground (GT.show GT.int))
      1 q qh
      ( "Forward: asking for all answers HANGS",
        fun xs -> reverso (Std.list ( !! ) [ 1; 2; 3 ]) xs )

  let __ () =
    run_r (Std.List.prj_exn prj_exn)
      (GT.show Std.List.ground (GT.show GT.int))
      all_answers q qh
      ( "Forward: asking for all answers HANGS",
        fun xs -> reverso (Std.list ( !! ) [ 1; 2; 3 ]) xs )
end

include struct
  open Direction2

  let () = print_endline "Another ordering of conjuction arguments"

  let () =
    run_r (Std.List.prj_exn prj_exn)
      (GT.show Std.List.ground (GT.show GT.int))
      all_answers q qh
      ( "Forward: asking for all answers works",
        fun xs -> reverso (Std.list ( !! ) [ 1; 2; 3 ]) xs )

  let () =
    run_r (Std.List.prj_exn prj_exn)
      (GT.show Std.List.ground (GT.show GT.int))
      1 q qh
      ( "Backward: asking for 1  answer",
        fun xs -> reverso xs (Std.list ( !! ) [ 1; 2; 3 ]) )

  let __ () =
    run_r (Std.List.prj_exn prj_exn)
      (GT.show Std.List.ground (GT.show GT.int))
      all_answers q qh
      ( "Backward: asking for all answers HANGS",
        fun xs -> reverso xs (Std.list ( !! ) [ 1; 2; 3 ]) )
end
 *)
