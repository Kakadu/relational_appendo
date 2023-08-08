open OCanren

let reify_lists eta =
  Std.Pair.reify
    (Std.List.reify OCanren.reify)
    (Std.List.reify OCanren.reify)
    eta

let rec appendo xs ys xys =
  fresh (h tl xytl)
    (conde
       [
         xs === Std.nil () &&& (ys === xys);
         xs === Std.List.cons h tl
         &&& (xys === Std.List.cons h xytl)
         &&& appendo tl ys xytl;
       ])

let maybe_flip flg g1 g2 = if flg then g2 &&& g1 else g1 &&& g2

module Direction1 = struct
  let trace2args fmt1 xs fmt2 ys =
    debug_var (Std.pair xs ys) (Fun.flip reify_lists) (function
      | [ Value (xs, ys) ] ->
          Format.printf (fmt1 ^^ "%s\n%!")
            ((GT.show Std.List.logic (GT.show OCanren.logic (GT.show GT.int)))
               xs);
          Format.printf (fmt2 ^^ "%s\n%!")
            ((GT.show Std.List.logic (GT.show OCanren.logic (GT.show GT.int)))
               ys);
          success
      | _ -> assert false)

  let rec reverso xs ys =
    fresh (h tl temp)
      (conde
         [
           xs === Std.nil () &&& (xs === ys);
           fresh ()
             (xs === Std.List.cons h tl)
             (* Tracing of arguments *)
             (* (trace2args "  xs = " xs "  ys = " ys) *)
             (debug_var ys
                (Fun.flip (Std.List.reify OCanren.reify))
                (function
                  | [ Var _ ] ->
                      reverso tl temp
                      &&& appendo temp Std.(List.cons h (nil ())) ys
                  | _ ->
                      appendo temp Std.(List.cons h (nil ())) ys
                      &&& reverso tl temp));
         ])
end

module Direction2 = struct
  let rec reverso xs ys =
    fresh (h tl temp)
      (debug_var xs
         (Fun.flip (Std.List.reify OCanren.reify))
         (function [ _h ] -> success | _ -> assert false))
      (conde
         [
           xs === Std.nil () &&& (xs === ys);
           xs === Std.List.cons h tl &&& reverso tl temp
           &&& appendo temp Std.(List.cons h (nil ())) ys;
         ])
end
