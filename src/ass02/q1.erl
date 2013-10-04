-module(q1).
-export([cluster/0]).

-record(edge, {
    a :: integer(),
    b :: integer(),
    costs :: integer()
}).

-opaque edge() :: #edge{}.

-spec compare_edges(EdgeA, EdgeB) -> Smaller when
    EdgeA :: edge(),
    EdgeB :: edge(),
    Smaller :: boolean().
compare_edges(A, B) -> 
    A#edge.costs < B#edge.costs.

read_edges() -> 
    {Ok,Device} = file:open("ass02/clustering1.txt", [read]),
    {Ok,_} = io:fread(Device, "", "~d"),
    try get_all_edges(Device)
        after file:close(Device)
    end.

get_all_edges(Device) ->
    case io:fread(Device, "", "~d ~d ~d") of
        {ok,[Start,End,Costs]} -> [#edge{a=Start, b=End, costs=Costs} | get_all_edges(Device)];
        eof -> []
    end.


construct_edge_heap(Edges) -> lists:foldl(fun heap:insert/2, heap:new(fun compare_edges/2), Edges).
construct_clusters(Edges) ->
    EdgeSet=lists:foldl(fun(E,Set) -> ordsets:add_element(E#edge.a, ordsets:add_element(E#edge.b, Set)) end, ordsets:new(), Edges),
    union_find:new(EdgeSet).

cluster() ->
    Edges=read_edges(),
    Heap=construct_edge_heap(Edges),
    UF=construct_clusters(Edges),
    do_reduce_cluster(4, union_find:count(UF), UF, Heap),
    max_spacing(UF, Heap).

do_reduce_cluster(Expected, Expected, _, _) -> true;
do_reduce_cluster(Expected, Current, UF, Heap) ->
    MinEdge=heap:extract_min(Heap),
    case {union_find:find(MinEdge#edge.a, UF), union_find:find(MinEdge#edge.b, UF)} of
        {Same, Same} -> do_reduce_cluster(Expected, Current, UF, Heap);
        {AP, BP} ->
            union_find:union(AP, BP, UF),
            do_reduce_cluster(Expected, union_find:count(UF), UF, Heap)
    end.

max_spacing(UF,Heap) ->
    MinEdge=heap:extract_min(Heap),
    case {union_find:find(MinEdge#edge.a, UF), union_find:find(MinEdge#edge.b, UF)} of
        {Same, Same} -> max_spacing(UF, Heap);
        {_, _} ->
            MinEdge#edge.costs
    end.