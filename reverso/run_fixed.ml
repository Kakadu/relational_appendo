open OCanren
open Tester
open Reverso_fixed

let all_answers = -1

include struct
  open Direction1

  let () =
    run_r (Std.List.prj_exn prj_exn)
      (GT.show Std.List.ground (GT.show GT.int))
      all_answers q qh
      ( "Forward: asking for all answers WORKS",
        fun xs -> reverso (Std.list ( !! ) [ 1; 2; 3 ]) xs )

  let () =
    run_r (Std.List.prj_exn prj_exn)
      (GT.show Std.List.ground (GT.show GT.int))
      all_answers q qh
      ( "Backward: asking for all answers WORKS",
        fun xs -> reverso xs (Std.list ( !! ) [ 1; 2; 3 ]) )
end
