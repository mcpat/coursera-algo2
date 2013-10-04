-module(q2).
-export([cluster/0]).

read_points() -> 
    {Ok,Device} = file:open("ass02/clustering_big.txt", [read]),
    {Ok,[_NumberOfPoints,PointBits]} = io:fread(Device, "", "~d ~d"),
    try get_all_points(Device,string:copies("~d ", PointBits))
        after file:close(Device)
    end.

get_all_points(Device,Pattern) ->
    case io:fread(Device, "", Pattern) of
        {ok,Bits} -> [get_point(Bits) | get_all_points(Device, Pattern)];
        eof -> []
    end.

get_point(Bits) ->
    get_point(Bits,0,0).
get_point([], Value, _Count) -> Value;
get_point([B | Rest], Value, Count) ->
    get_point(Rest, (Value bsl 1) + B, Count + B).


merge_clusters([], _UF) -> true;
merge_clusters([First | Rest], UF) ->
    process_candidates(First, expand(First), UF),
    merge_clusters(Rest, UF).


expand(Value) ->
    expand1(Value,0) ++ expand2(Value,1,0).

expand1(_Value,24) -> [];
expand1(Value,Bit) ->
    NewValue= (Value bxor (1 bsl Bit)),
    
    Rest=expand1(Value, Bit+1),
    if
        NewValue < Value -> [NewValue | Rest];
        true -> Rest
    end.


expand2(_Value,24,23) -> [];
expand2(Value,A,B) ->
    NewValue=(Value bxor ((1 bsl A) bor (1 bsl B))),
    
    NewA= (A + 1) rem 24,
    
    if
        NewA == 0 -> 
            NewB= B + 1,
            Rest= expand2(Value, NewB+1, NewB);
        true ->
            Rest= expand2(Value, NewA, B)
    end,
    
    if
        NewValue < Value -> [NewValue | Rest];
        true -> Rest
    end.


process_candidates(_E, [], _UF) -> true;
process_candidates(E, [First | Rest], UF) ->
    FP= union_find:find(First, UF),
    
    if
        FP =/= false ->
            union_find:union(E, FP, UF);
        true -> true
    end,
    process_candidates(E, Rest, UF).


cluster() ->
    Points=read_points(),
    UF=union_find:new(Points),
    merge_clusters(Points,UF),
    union_find:count(UF).