-module(q3).
-export([read_edges/0, collect_vertices/1, prim_mst/2, kruskal_mst/0, sum_mst_costs/1]).

compare_edges({edge, _, _, CostA}, {edge, _, _, CostB}) -> 
    CostA < CostB.

read_edges() -> 
    {Ok,Device} = file:open("edges.txt", [read]),
    {Ok,_} = io:fread(Device, "", "~d ~d"),
    try get_all_edges(Device)
        after file:close(Device)
    end.

get_all_edges(Device) ->
    case io:fread(Device, "", "~d ~d ~d") of
        {ok,[Start,End,Costs]} -> [{edge, Start, End, Costs} | get_all_edges(Device)];
        eof -> []
    end.


collect_vertices(Edges) -> collect_vertices(Edges, []).
collect_vertices([], V) -> V;
collect_vertices([{edge, Start, End, _} | Rest], Vertices) -> collect_vertices(Rest, insert_vertex(End, insert_vertex(Start, Vertices))).

insert_vertex(V, []) -> [V];
insert_vertex(V, [V]) -> [V];
insert_vertex(V, [V | R]) -> [V | R];
insert_vertex(V, [A | R]) -> [A | insert_vertex(V, R)].

prim_mst(Edges, Vertices) -> prim_mst(Edges, Vertices, [], []).
prim_mst([], [], _, Mst) -> Mst;
prim_mst(Edges, [V | Rest], [], Mst) -> prim_mst(Edges, Rest, [V], Mst);
prim_mst(Edges, Vertices, SubGraph, Mst) ->
    MinCuttingEdge=get_minimum_cutting_edge(Edges, SubGraph, nil),
    ExploredVertex=get_new_vertex(MinCuttingEdge, SubGraph),
    NewSubGraph=[ExploredVertex | SubGraph],
    NewEdges=remove_inner_edges(Edges, NewSubGraph, []),
    prim_mst(NewEdges, lists:delete(ExploredVertex, Vertices), NewSubGraph, [MinCuttingEdge | Mst]).


sum_mst_costs(Mst) -> sum_mst_costs(Mst, 0).
sum_mst_costs([], Sum) -> Sum;
sum_mst_costs([{edge, _, _, C} | Rest], Sum) -> sum_mst_costs(Rest, C + Sum).


remove_inner_edges([], _, OutsideEdges) -> OutsideEdges;
remove_inner_edges([E= {edge, Start, End, _} | Rest], SubGraph, OutsideEdges) ->
    Ain=lists:member(Start, SubGraph),
    Bin=lists:member(End, SubGraph),
    if
        Ain and Bin -> remove_inner_edges(Rest, SubGraph, OutsideEdges);
        true -> remove_inner_edges(Rest, SubGraph, [E | OutsideEdges])
    end.


get_new_vertex({edge, A, B, _}, SubGraph) ->
    Ain=lists:member(A, SubGraph),
    if
        Ain -> B;
        true -> A
    end.

get_minimum_cutting_edge([], _, Minimum) -> Minimum;
get_minimum_cutting_edge([E= {edge, Start, End, NewCost} | Rest], SubGraph, OldMin) ->
    Ain=lists:member(Start, SubGraph),
    Bin=lists:member(End, SubGraph),
    if
        Ain or Bin ->
            if
                OldMin == nil -> get_minimum_cutting_edge(Rest, SubGraph, E);
                true ->
                    {edge, _, _, OldCost}= OldMin,
                    if
                        OldCost > NewCost -> get_minimum_cutting_edge(Rest, SubGraph, E);
                        true -> get_minimum_cutting_edge(Rest, SubGraph, OldMin)
                    end
            end;
        true -> get_minimum_cutting_edge(Rest, SubGraph, OldMin)
    end.


%% Kruskal MST implementation
construct_heap(Edges) -> lists:foldl(fun heap:insert/2, heap:new(fun compare_edges/2), Edges).
kruskal_mst() ->
    Edges=read_edges(),
    Heap=construct_heap(Edges),
    {_,_,MstCosts}=heap:foldmin(fun process_edge/2, {[], [], 0}, Heap),
    MstCosts.

process_edge(Edge={edge, _, _, Costs}, Acc={SubGraphs, MstEdges, MstCosts}) ->
    case check_cycle(Edge, SubGraphs) of
        {ok, NewSubGraphs} -> {NewSubGraphs, [Edge | MstEdges], MstCosts + Costs};
        cycle -> Acc
    end.

check_cycle(Edge={edge, A, B, _}, SubGraphs) -> 
    case find_matches(Edge, SubGraphs, none) of
        cycle -> cycle;
        none -> {ok, [ordsets:from_list([A,B]) | SubGraphs]};
        {one, Match} -> {ok, [ordsets:union(ordsets:from_list([A,B]), Match) | lists:delete(Match, SubGraphs)]};
        {two, Match1, Match2} -> {ok, [ordsets:union(Match1, Match2) | lists:delete(Match1, lists:delete(Match2, SubGraphs))]}
    end.

find_matches(_, [], none) -> none;
find_matches(_, [], Match) -> {one, Match};
find_matches(Edge={edge, A, B, _}, [F | R], none) ->
    Ain=ordsets:is_element(A, F),
    Bin=ordsets:is_element(B, F),
    if
        Ain and Bin -> cycle;
        Ain or Bin -> find_matches(Edge, R, F);
        true -> find_matches(Edge, R, none)
    end;
find_matches(Edge={edge, A, B, _}, [F | R], Match) ->
    Ain=ordsets:is_element(A, F),
    Bin=ordsets:is_element(B, F),
    if
        Ain or Bin -> {two, Match, F};
        true -> find_matches(Edge, R, Match)
    end.