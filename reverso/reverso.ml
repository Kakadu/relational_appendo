open OCanren

let rec appendo xs ys xys =
  fresh (h tl xytl)
    (conde
       [
         xs === Std.nil () &&& (ys === xys);
         xs === Std.List.cons h tl
         &&& (xys === Std.List.cons h xytl)
         &&& appendo tl ys xytl;
       ])

module Direction1 = struct
  let rec reverso xs ys =
    fresh (h tl temp)
      (conde
         [
           xs === Std.nil () &&& (xs === ys);
           xs === Std.List.cons h tl
           &&& appendo temp Std.(List.cons h (nil ())) ys
           &&& reverso tl temp;
         ])
end

module Direction2 = struct
  let rec reverso xs ys =
    fresh (h tl temp)
      (conde
         [
           xs === Std.nil () &&& (xs === ys);
           xs === Std.List.cons h tl &&& reverso tl temp
           &&& appendo temp Std.(List.cons h (nil ())) ys;
         ])
end
