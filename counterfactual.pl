% A Semantic Tableaux for Counterfactuals, Semifactuals, and Necessity statements
next_world_id(Branch, ID) :-
    findall(I, member(p(I, _), Branch), S),
    max_list(S, M),
    ID is M + 1.

rule_tc1(Branch, [p(J, A)]) :-
    member(r(_, A, J), Branch),
    \+ member(p(J, A), Branch).

rule_and(Branch, [p(I, A)]) :-
    member(p(I, and(A, _)), Branch),
    \+ member(p(I, A), Branch).
rule_and(Branch, [p(I, B)]) :-
    member(p(I, and(_, B)), Branch),
    \+ member(p(I, B), Branch).

rule_nand(Branch, [p(I, or(not(A), not(B)))]) :-
    member(p(I, not(and(A, B))), Branch),
    \+ member(p(I, or(not(A), not(B))), Branch).

rule_nor(Branch, [p(I, and(not(A), not(B)))]) :-
    member(p(I, not(or(A, B))), Branch),
    \+ member(p(I, and(not(A), not(B))), Branch).

rule_or(Branch, [p(I, A)]) :-
    member(p(I, or(A, B)), Branch),
    \+ member(p(I, A), Branch),
    \+ member(p(I, B), Branch).
rule_or(Branch, [p(I, B)]) :-
    member(p(I, or(A, B)), Branch),
    \+ member(p(I, A), Branch),
    \+ member(p(I, B), Branch).

rule_necc(Branch, [p(I, A)]) :-
    member(p(I, n(A)), Branch),
    \+ member(p(I, A), Branch).
rule_necc(Branch, [p(I, A)]) :-
    member(p(I, n(A)), Branch),
    member(r(I, _, _), Branch),
    \+ member(p(I, A), Branch).
rule_necc(Branch, [p(J, A)]) :-
    member(p(I, n(A)), Branch),
    member(r(I, _, J), Branch),
    \+ member(p(J, A), Branch).

rule_notcounterfactual(Branch, [r(I, A, K), p(K, not(B))]) :-
    member(p(I, not(c(A, B))), Branch),
    \+ (member(r(I, A, J), Branch), member(p(J, not(B)), Branch)),
    next_world_id(Branch, K).
rule_notcounterfactual(Branch, [p(I, not(A))]) :-
    member(p(I, not(c(A, _))), Branch),
    \+ member(p(I, not(A)), Branch).
rule_notcounterfactual(Branch, [p(I, not(B))]) :-
    member(p(I, not(c(_, B))), Branch),
    \+ member(p(I, not(B)), Branch).

rule_counterfactual(Branch, [p(J, B)]) :-
    member(p(I, c(A, B)), Branch),
    member(r(I, A, J), Branch),
    \+ member(p(J, B), Branch).
rule_counterfactual(Branch, [p(I, not(A))]) :-
    member(p(I, c(A, _)), Branch),
    \+ member(p(I, not(A)), Branch).
rule_counterfactual(Branch, [p(I, not(B))]) :-
    member(p(I, c(_, B)), Branch),
    \+ member(p(I, not(B)), Branch).
rule_counterfactual(Branch, [r(I, A, J)]) :-
    member(p(I, c(A, _)), Branch),
    \+ member(r(I, A, _), Branch),
    next_world_id(Branch, J).

rule_notsemifactual(Branch, [r(I, A, K), p(K, not(B))]) :-
    member(p(I, not(s(A, B))), Branch),
    \+ (member(r(I, A, J), Branch), member(p(J, not(B)), Branch)),
    next_world_id(Branch, K).
rule_notsemifactual(Branch, [p(I, not(A))]) :-
    member(p(I, not(s(A, _))), Branch),
    \+ member(p(I, not(A)), Branch).
rule_notsemifactual(Branch, [p(I, B)]) :-
    member(p(I, not(s(_, B))), Branch),
    \+ member(p(I, B), Branch).

rule_semifactual(Branch, [p(J, B)]) :-
    member(p(I, s(A, B)), Branch),
    member(r(I, A, J), Branch),
    \+ member(p(J, B), Branch).
rule_semifactual(Branch, [p(I, not(A))]) :-
    member(p(I, s(A, _)), Branch),
    \+ member(p(I, not(A)), Branch).
rule_semifactual(Branch, [p(I, B)]) :-
    member(p(I, s(_, B)), Branch),
    \+ member(p(I, B), Branch).
rule_semifactual(Branch, [r(I, A, J)]) :-
    member(p(I, s(A, _)), Branch),
    \+ member(r(I, A, _), Branch),
    next_world_id(Branch, J).

rule_neg(Branch, [p(I, A)]) :-
    member(p(I, not(not(A))), Branch),
    \+ member(p(I, A), Branch).

rule_impl(Branch, [p(I, or(not(A), B))]) :-
    member(p(I, impl(A, B)), Branch),
    \+ member(p(I, or(not(A), B)), Branch).

expand(Branch, Result) :-
    rule_neg(Branch, New),!,
    union(Branch, New, BranchNew),
    expand(BranchNew, Result).
expand(Branch, Result) :-
    rule_nor(Branch, New),!,
    union(Branch, New, BranchNew),
    expand(BranchNew, Result).
expand(Branch, Result) :-
    rule_nand(Branch, New),!,
    union(Branch, New, BranchNew),
    expand(BranchNew, Result).
expand(Branch, Result) :-
    rule_counterfactual(Branch, New),!,
    union(Branch, New, BranchNew),
    expand(BranchNew, Result).
expand(Branch, Result) :-
    rule_notcounterfactual(Branch, New),!,
    union(Branch, New, BranchNew),
    expand(BranchNew, Result).
expand(Branch, Result) :-
    rule_semifactual(Branch, New),!,
    union(Branch, New, BranchNew),
    expand(BranchNew, Result).
expand(Branch, Result) :-
    rule_notsemifactual(Branch, New),!,
    union(Branch, New, BranchNew),
    expand(BranchNew, Result).
expand(Branch, Result) :-
    rule_necc(Branch, New),!,
    union(Branch, New, BranchNew),
    expand(BranchNew, Result).
expand(Branch, Result) :-
    rule_tc1(Branch, New),!,
    union(Branch, New, BranchNew),
    expand(BranchNew, Result).
expand(Branch, Result) :-
    rule_impl(Branch, New),!,
    union(Branch, New, BranchNew),
    expand(BranchNew, Result).
expand(Branch, Result) :-
    rule_and(Branch, New),!,
    union(Branch, New, BranchNew),
    expand(BranchNew, Result).
expand(Branch, Result) :-
    rule_or(Branch, New),
    union(Branch, New, BranchNew),
    expand(BranchNew, Result).
expand(Branch, Branch).

find_model(Branch):-
    expand(Branch, B),
    conflict_free(B), 
    saturated(B), !,
    write(B).

conflict_free(Branch) :-
    \+ has_conflict(Branch).
has_conflict(Branch) :-
    member(p(I, F), Branch),
    member(p(I, not(F)), Branch).

saturated(Branch) :-
    \+ unexpanded_or(Branch).

unexpanded_or(Branch) :-
    member(p(I, or(A, B)), Branch),
    \+ member(p(I, A), Branch),
    \+ member(p(I, B), Branch).
