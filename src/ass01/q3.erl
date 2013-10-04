-module(q3).
-export([read_edges/0, collect_vertices/1, prim_mst/0, kruskal_mst/0, sum_mst_costs/1]).
-export_type([edge/0]).

-record(edge, {
    a :: integer(),
    b :: integer(),
    costs :: integer()
}).

-opaque edge() :: #edge{}.

compare_edges(A, B) -> 
    A#edge.costs < B#edge.costs.

read_edges() -> 
    {Ok,Device} = file:open("ass01/edges.txt", [read]),
    {Ok,_} = io:fread(Device, "", "~d ~d"),
    try get_all_edges(Device)
        after file:close(Device)
    end.

get_all_edges(Device) ->
    case io:fread(Device, "", "~d ~d ~d") of
        {ok,[Start,End,Costs]} -> [#edge{a=Start, b=End, costs=Costs} | get_all_edges(Device)];
        eof -> []
    end.


collect_vertices(Edges) -> collect_vertices(Edges, []).
collect_vertices([], V) -> V;
collect_vertices([{edge, Start, End, _} | Rest], Vertices) -> collect_vertices(Rest, insert_vertex(End, insert_vertex(Start, Vertices))).

insert_vertex(V, []) -> [V];
insert_vertex(V, [V]) -> [V];
insert_vertex(V, [V | R]) -> [V | R];
insert_vertex(V, [A | R]) -> [A | insert_vertex(V, R)].

prim_mst() ->
    Edges=read_edges(),
    Vertices=collect_vertices(Edges),
    sum_mst_costs(prim_mst(Edges,Vertices)).

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


get_new_vertex(Edge, SubGraph) ->
    Ain=lists:member(Edge#edge.a, SubGraph),
    if
        Ain -> Edge#edge.b;
        true -> Edge#edge.a
    end.

-spec get_minimum_cutting_edge(Edges, SubGraph, OldMin) -> Min when
    Edges :: [edge()],
    SubGraph :: term(),
    OldMin :: term(),
    Min :: edge().

get_minimum_cutting_edge([], _, Minimum) -> Minimum;
get_minimum_cutting_edge([E | Rest], SubGraph, OldMin) ->
    Ain=lists:member(E#edge.a, SubGraph),
    Bin=lists:member(E#edge.b, SubGraph),
    if
        Ain or Bin ->
            if
                OldMin == nil -> get_minimum_cutting_edge(Rest, SubGraph, E);
                true ->
                    if
                        OldMin#edge.costs > E#edge.costs -> get_minimum_cutting_edge(Rest, SubGraph, E);
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
    UF=union_find:new(collect_vertices(Edges)),
    {_,_,MstCosts}=heap:walk(fun process_edge/2, {UF, [], 0}, Heap),
    MstCosts.

process_edge(Edge, Acc={UF, MstEdges, MstCosts}) ->
    case {union_find:find(Edge#edge.a, UF), union_find:find(Edge#edge.b, UF)} of
        {Same, Same} -> Acc;
        {AP, BP} ->
            union_find:union(AP,BP,UF),
            {UF, [Edge | MstEdges], MstCosts + Edge#edge.costs}
    end.