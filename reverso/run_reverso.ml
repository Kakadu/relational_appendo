open OCanren
open Tester
open Reverso

let all_answers = -1

(* let () =
   run_r (Std.List.prj_exn prj_exn)
     (GT.show Std.List.ground (GT.show GT.int))
     all_answers qr qrh
     ("", fun xs ys -> appendo xs ys (Std.list ( !! ) [ 1; 2; 3 ]))
*)
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
