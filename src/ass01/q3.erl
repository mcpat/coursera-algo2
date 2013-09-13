-module(q3).
-export([read_edges/0, collect_vertices/1, prim_mst/2, sum_mst_costs/1]).

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