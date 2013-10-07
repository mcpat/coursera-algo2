-module(bf).
-export([bellman_ford/3]).

-define(UNDEFINED, 'undefined').
-define(INFINITY, 16#FFFFFF).

create_A0(S,N) ->
    A0=myarray:new(?INFINITY),
    fill_A0(S,1,N,A0),
    A0.

fill_A0(_S, C, N, _A0) when C > N -> true;
fill_A0(S, C, N, A0) ->
    if
        C == S -> myarray:set(C,0,A0);
        true -> myarray:set(C,?INFINITY,A0)
    end,
    fill_A0(S,C+1,N,A0).


bellman_ford(S,N,G) ->
    A0=create_A0(S, N),
    ANmin1=do_work(1, N, A0, G),
    Check=cycle_check(1, N, ANmin1, G),
    {Check,ANmin1}.


do_work(I, N, AImin1, _G) when I > N - 1 -> AImin1;
do_work(I, N, AImin1, G) ->
    io:fwrite("~w\n", [I]),
    AI=myarray:new(?INFINITY),
    vstep(1, N, AImin1, AI, G),
    myarray:delete(AImin1),
    do_work(I+1, N, AI, G).


vstep(V, N, _AImin1, _AI, _G) when V > N -> ok;
vstep(V, N, AImin1, AI, G) ->
    NewMin=minimal_path(V, AImin1, G, myarray:get(V, AImin1)),
    
    if
        NewMin < ?INFINITY -> myarray:set(V, NewMin, AI);
        true -> ok
    end,
    vstep(V+1, N, AImin1, AI, G).


minimal_path(V, AImin1, G, OldMin) ->
    case myarray:get(V, G) of
        ?UNDEFINED -> OldMin;
        Edges -> minimal_path_to_v(AImin1, Edges, OldMin)
    end.


minimal_path_to_v(_AImin1, [], OldMin) -> OldMin;
minimal_path_to_v(AImin1, [{W, FromWCosts} | Rest], OldMin) -> 
    case myarray:get(W, AImin1) of
        ?INFINITY -> NewMin=OldMin;
        ToWCosts -> NewMin= ToWCosts + FromWCosts
    end,
    minimal_path_to_v(AImin1, Rest, erlang:min(OldMin, NewMin)).


cycle_check(V, N, _A, _G) when V > N -> ok;
cycle_check(V, N, A, G) ->
    OldMin=myarray:get(V, A),
    NewMin=minimal_path(V, A, G, OldMin),
    if
        NewMin < OldMin -> neg_cycle;
        true -> cycle_check(V+1, N, A, G)
    end.