-module(knapsack).
-export([q1/0, q2/0, test/0]).

-record(item,{
    value :: integer(),
    weight :: integer()
}).

load_problem(Name) ->
    {Ok,Device} = file:open(string:concat("ass03/",Name), [read]),
    {Ok,[W, _NumItems]} = io:fread(Device, "", "~d ~d"),
    try
        Items=get_all_items(Device),
        {W, Items}
        after file:close(Device)
    end.

get_all_items(Device) ->
    case io:fread(Device, "", "~d ~d") of
        {ok,[Value,Weight]} ->
            [#item{value=Value,weight=Weight} | get_all_items(Device)]; 
        eof -> []
    end.


knapsack({Size, Items}) ->
    Result=itemloop(Size, Items, [{0,0}]),
    {_Idx, Value}= lists:last(Result),
    Value.

itemloop(_, [], A) -> A;
itemloop(Size, [#item{value=V,weight=W} | Rest], A) ->
    NewA= xloop(W, V, Size, A, A),
    itemloop(Size, Rest, NewA).


xloop(_W, _V, _Max, Copy, []) -> Copy;

xloop(W, _V, Max, Copy, [{LIdx,_} | _]) when LIdx + W > Max -> Copy;

xloop(W, V, Max, [], [{LIdx, LVal} | LRest]) ->
    [{LIdx + W, LVal + V} | xloop(W, V, Max, [], LRest)];


xloop(W, V, Max, Copy=[{CIdx, CVal} | _], [{LIdx, LVal} | LRest]) when (CIdx =< LIdx + W) and (CVal >= LVal + V) ->
    xloop(W, V, Max, Copy, LRest);

xloop(W, V, Max, [{CIdx, CVal} | CRest], Lookup=[{LIdx, LVal} | _]) when (CIdx >= LIdx + W) and (CVal =< LVal + V) ->
    xloop(W, V, Max, CRest, Lookup);

xloop(W, V, Max, [CF={CIdx, _} | CRest], Lookup=[{LIdx, _} | _]) when CIdx < LIdx + W ->
    [CF | xloop(W, V, Max, CRest, Lookup)];


xloop(W, V, Max, [{CIdx, CVal} | CRest], [{LIdx, LVal} | LRest]) when (CIdx =:= LIdx + W) ->
    [{CIdx, erlang:max(CVal, LVal + V)} | xloop(W, V, Max, CRest, LRest)];


xloop(W, V, Max, Copy, [{LIdx, LVal} | LRest]) ->
    [{LIdx + W, LVal + V} | xloop(W, V, Max, Copy, LRest)].
    
    
q1() ->
    Problem=load_problem("knapsack1.txt"),
    knapsack(Problem).


q2() ->
    Problem=load_problem("knapsack_big.txt"),
    knapsack(Problem).

test() ->
    Problem={6, [#item{value=3,weight=4}, #item{value=2,weight=3},#item{value=4,weight=2},#item{value=4,weight=3}]},
    knapsack(Problem).