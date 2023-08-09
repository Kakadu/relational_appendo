## Relational demos

In this repo you can find a few demos of relational programming with OCanren library. Relational programming is a form of logic programming with a minimum of basic fieatures, and implmented via embedding to general purpose host language (in our case it is OCaml). Relational programming represents computable functions as computable mathematical relations. Because of that you can specify input argument and query the output to simulate normal function execution ("forward direction") or you could specify the output and synthesize the input ("backward direction"). The goal is to write programs in careful manner (unlike Prolog), so relational programs are executable in any direction from the beginning, without any additional patches.

Relational programming is not a true declarative programming. Swapping some conjuncts may degrade performance, but all answers produced by the first order of conjuncts, should be produced by the another order sooner or later. The problem is that in one order it will finish saying that no more answers, but in another one it will hang. It's called "refutational completeness".

In this repo we can find two examples of relational programs with relevant behaviour. In both cases we can modify relations using human insight: looking at relation's arguments we are going to swap or not to swap a few conjuncts. It gives us a relation that finishes both in forward and backward direction. Although, the primitive `debug_var` which allows us to do that is dangerous: the one could break execution in one of direction if not careful.

The first example is about adding of peano numbers. There are two possible implementations using induction: decreasing on first or the second argument. If we check groundness of the arguments, we can decide which strategy is better.

Another argument is about appending and reversing lists. The relation `reverso` needs to do a recursive call and a call to `appendo` relation. Looking at the traces of the execution gives us the insight that checking groundness will help in this case too, but it's more difficult to explain why, comparately the provious example.

### Goal

In this repo I specified two examples where runtime specialization could help improve performance of relations (currently in the bad cases they deverge). The concrete heuristics may be different for various relational programs and discovering them is a future work. Right now, I'm curious whether JIT compilation could give us technical ability to specialize relational programs on the fly for these two not very complicated examples.