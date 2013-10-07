-module(q1).
-export([run/1,find_shortest_path_costs/2]).

-define(UNDEFINED, 'undefined').
-define(INFINITY, 16#FFFFFF).

load_graph(Name) ->
    {Ok,Device} = file:open(string:concat("ass04/",Name), [read]),
    {Ok,[NumNodes, _NumEdges]} = io:fread(Device, "", "~d ~d"),
    GraphArr=myarray:new(?UNDEFINED),
    try get_all_edges(Device, GraphArr)
        after file:close(Device)
    end,
    {NumNodes, GraphArr}.

get_all_edges(Device, GraphArr) ->
    case io:fread(Device, "", "~d ~d ~d") of
        {ok,[Start,End,Costs]} ->
            case myarray:get(End, GraphArr) of
                ?UNDEFINED -> ToInsert= [{Start,Costs}];
                Edges -> ToInsert= [{Start,Costs} | Edges]
            end,
            myarray:set(End, ToInsert, GraphArr), get_all_edges(Device, GraphArr);
        eof -> true
    end.


build_A0(N, G) ->
    A0=myarray:new(?INFINITY),
    init_A0(1, N, G, A0),
    A0.

init_A0(I, N, _G, _A0) when I > N -> ok;
init_A0(I, N, G, A0) ->
    case myarray:get(I, G) of
        ?UNDEFINED -> Edges=[];
        Value -> Edges=Value
    end,
    UnsortedEdges=[{I,0} | Edges],
    SortedEdges=lists:sort(fun({A, _}, {B, _}) -> A =< B end, UnsortedEdges),
    myarray:set(I, SortedEdges, A0),
    init_A0(I+1, N, G, A0).


find_shortest_path_costs(N, A) ->
    find_shortest_path_costs(1, N, A, ?INFINITY).

find_shortest_path_costs(I, N, _A, MinCosts) when I > N -> MinCosts;
find_shortest_path_costs(I, N, A, OldMin) ->
    Edges=myarray:get(I, A),
    NewMin= lists:foldl(
        (fun({J, JtoICosts}, CurrentMin) ->
            if
                I == J -> CurrentMin;
                true -> erlang:min(CurrentMin, JtoICosts)
            end
         end), OldMin, Edges),
    
    find_shortest_path_costs(I+1, N, A, erlang:min(OldMin, NewMin)).

run(Name) ->
    {N,G}=load_graph(Name),
%%     insert_node(N,G),
%%     bf:bellman_ford(N+1, N+1, G).
%%     
    A0=build_A0(N,G),
    AN=do_work(1, N, A0),
    find_shortest_path_costs(N, AN).


do_work(K, N, AKmin1) when K > N -> AKmin1;
do_work(K, N, AKmin1) ->
    io:fwrite("~w\n", [K]),
    AK=myarray:new([]),
    jloop(K, 1, N, AKmin1, AK),
    myarray:delete(AKmin1),
    do_work(K+1, N, AK).


jloop(_K, J, N, _AKmin1, _AK) when J > N -> ok;
jloop(K, J, N, AKmin1, AK) ->
    ToJEdges= myarray:get(J, AKmin1),
    case getktoj(K, ToJEdges) of
        none -> NewToJEdges=ToJEdges;
        KtoJ ->
            ToKEdges= myarray:get(K, AKmin1),
            NewToJEdges= process_edges(KtoJ, ToKEdges, ToJEdges)
    end,
    myarray:set(J, NewToJEdges, AK),
    jloop(K, J+1, N, AKmin1, AK).


getktoj(_K, []) -> none;
getktoj(K, [{I, _C} | _Rest]) when K < I -> none;
getktoj(K, [E={K, _C} | _Rest]) -> E;
getktoj(K, [_E | Rest]) ->
    getktoj(K, Rest).


process_edges(KtoJ, ToKEdges, ToJEdges) -> process_edges(KtoJ, ToKEdges, ToJEdges, []).

process_edges(_KtoJ, _ToKEdges=[], _ToJEdges=[], NewToJEdges) ->
    lists:reverse(NewToJEdges);

process_edges(KtoJ, _ToKEdges=[], [ToJE | ToJRest], NewToJEdges) ->
    process_edges(KtoJ, [], ToJRest, [ToJE | NewToJEdges]);

process_edges(KtoJ={_K, KtoJCosts}, [{I, ItoKCosts} | ToKRest], _ToJEdges=[], NewToJEdges) ->
    process_edges(KtoJ, ToKRest, [], [{I, ItoKCosts + KtoJCosts} | NewToJEdges]);

process_edges(KtoJ={_K, KtoJCosts}, [{I, ItoKCosts} | ToKRest], [{I, ItoJCosts} | ToJRest], NewToJEdges) ->
    process_edges(KtoJ, ToKRest, ToJRest, [{I, erlang:min(ItoJCosts, ItoKCosts + KtoJCosts)} | NewToJEdges]);

process_edges(KtoJ={_K, KtoJCosts}, [{A, AtoKCosts} | ToKRest], ToJEdges=[{B, _} | _], NewToJEdges) when A < B ->
    process_edges(KtoJ, ToKRest, ToJEdges, [{A, AtoKCosts + KtoJCosts} | NewToJEdges]);

process_edges(KtoJ, ToKEdges=[{A, _} | _], [BtoJE={B, _} | ToJRest], NewToJEdges) when A > B ->
    process_edges(KtoJ, ToKEdges, ToJRest, [BtoJE | NewToJEdges]).