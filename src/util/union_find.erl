-module(union_find).

-export([new/1,find/2,union/3,count/1]).

-export_type([union_find/0]).

-define(ROOT_MARKER, 'root').
-define(SET_COUNT, 'count').

-opaque union_find() :: ets:tid().


new(Set) ->
    UF=ets:new(?MODULE, ['ordered_set']),
    ets:insert(UF, {?SET_COUNT, 0}),
    make(Set, UF).


make([], UF) -> UF;
make([Head|Tail], UF) ->
    EC=ets:lookup(UF, Head),
    if
        EC == [] ->
            ets:insert(UF, {Head, {?ROOT_MARKER, 0}}),
            [{?SET_COUNT,OldCount}]=ets:lookup(UF, ?SET_COUNT),
            ets:insert(UF, {?SET_COUNT, OldCount+1});
        true -> true
    end,
    make(Tail, UF).


union(A, B, UF) ->
    AP=find(A,UF),
    BP=find(B,UF),
    
    if
        AP == BP -> false;
        true -> 
    
        [{AP,{?ROOT_MARKER,ARank}}]= ets:lookup(UF, AP),
        [{BP,{?ROOT_MARKER,BRank}}]= ets:lookup(UF, BP),
        
        if
            ARank == BRank ->
                ets:insert(UF, {BP, {AP, BRank}}),
                ets:insert(UF, {AP, {?ROOT_MARKER, BRank+1}});
            ARank < BRank ->
                ets:insert(UF, {AP, {BP, ARank}});
            ARank > BRank ->
                ets:insert(UF, {BP, {AP, BRank}})
        end,
        
        [{?SET_COUNT,OldCount}]=ets:lookup(UF, ?SET_COUNT),
        ets:insert(UF, {?SET_COUNT, OldCount-1})
    end.
        

find(E, UF) ->
    case ets:lookup(UF, E) of
        [] -> false;
        [{E,{?ROOT_MARKER,_Rank}}] -> E;
        [{E,{P,_Rank}}] -> find(P,UF)
    end.


-spec count(UF :: union_find()) -> Count :: integer().
count(UF) -> 
    [{?SET_COUNT,Count}]=ets:lookup(UF, ?SET_COUNT),
    Count.
    