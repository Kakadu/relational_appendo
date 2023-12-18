open OCanren

(* Default: decreasing by first argument *)
let rec addo_1 a b sum =
  conde
    [
      a === Std.Nat.zero &&& (b === sum);
      fresh prev (a === Std.Nat.succ prev) (addo_1 prev b (Std.Nat.succ sum));
    ]

let rec addo_2 a b sum =
  conde
    [
      b === Std.Nat.zero &&& (a === sum);
      fresh prev (b === Std.Nat.succ prev) (addo_2 a prev (Std.Nat.succ sum));
    ]

let rec addo_fixed a b sum =
  conde
    [
      a === Std.Nat.zero &&& (b === sum);
      debug_var (Std.pair a b) (Std.Pair.reify OCanren.reify OCanren.reify)
        (function
        | [] | _ :: _ :: _ | [ Var _ ] -> assert false
        | [ Value (Value _, _) ] ->
            fresh prev
              (a === Std.Nat.succ prev)
              (addo_fixed prev b (Std.Nat.succ sum))
        | [ Value (Var _, Var _) ] | [ Value (_, Value _) ] ->
            fresh prev
              (b === Std.Nat.succ prev)
              (addo_fixed a prev (Std.Nat.succ sum)));
    ]
(*
   let rec addo_fixed2 a b sum =
     conde
       [
         a === Std.Nat.zero &&& (b === sum);
         debug_var a OCanren.reify (function
           | [ Value _ ] ->
               fresh prev
                 (a === Std.Nat.succ prev)
                 (addo_fixed2 prev b (Std.Nat.succ sum))
           | [ Var _ ] ->
               fresh prev
                 (b === Std.Nat.succ prev)
                 (addo_fixed2 a prev (Std.Nat.succ sum))
           | _ -> assert false);
       ] *)

(* conde
   [
     b === Std.Nat.zero &&& (a === sum);
     fresh prev (b === Std.Nat.succ prev) (addo_2 a prev (Std.Nat.succ sum));
   ]
*)
