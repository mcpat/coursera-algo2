-module(knapsack).
-export([q1/0, q2/0, test/0]).

-record(item,{
    value :: integer(),
    weight :: integer()
}).

load_problem(Name) ->
    {Ok,Device} = file:open(string:concat("ass03/",Name), [read]),
    {Ok,[W, NumItems]} = io:fread(Device, "", "~d ~d"),
    try
        Items=get_all_items(Device),
        {W, Items, NumItems}
        after file:close(Device)
    end.

get_all_items(Device) ->
    case io:fread(Device, "", "~d ~d") of
        {ok,[Value,Weight]} ->
            [#item{value=Value,weight=Weight} | get_all_items(Device)]; 
        eof -> []
    end.


knapsack({Size, Items, NumItems}) ->
    Result=itemloop(1, Size, Items, [{0,0,[]}]),
    reconstruct(lists:last(Result), lists:reverse(Items), NumItems).


reconstruct({_,_,Causes}, Items, NumItems) ->
    reconstruct(Causes, Items, NumItems);

reconstruct([], _, _) -> [];

reconstruct(C=[CIdx | _], [_ | Rest], ItemIndex) when ItemIndex > CIdx ->
    reconstruct(C, Rest, ItemIndex - 1);


reconstruct([ItemIndex | CRest], [First | IRest], ItemIndex) ->
    [First | reconstruct(CRest, IRest, ItemIndex - 1)].


itemloop(_I, _Max, [], A) -> A;
itemloop(I, Max, [#item{value=V,weight=W} | Rest], A) ->
    NewA= xloop(I, W, V, Max, A, A),
    itemloop(I+1, Max, Rest, NewA).


xloop(_I, _W, _V, _Max, Copy, []) -> Copy;

xloop(_I, W, _V, Max, Copy, [{LIdx,_,_} | _]) when LIdx + W > Max -> Copy;

xloop(I, W, V, Max, [], [{LIdx, LVal, Causes} | LRest]) ->
    [{LIdx + W, LVal + V, [I | Causes]} | xloop(I, W, V, Max, [], LRest)];


xloop(I, W, V, Max, Copy=[{CIdx, CVal, _} | _], [{LIdx, LVal, _} | LRest]) when (CIdx =< LIdx + W) and (CVal >= LVal + V) ->
    xloop(I, W, V, Max, Copy, LRest);

xloop(I, W, V, Max, [{CIdx, CVal, _} | CRest], Lookup=[{LIdx, LVal, _} | _]) when (CIdx >= LIdx + W) and (CVal =< LVal + V) ->
    xloop(I, W, V, Max, CRest, Lookup);

xloop(I, W, V, Max, [CF={CIdx, _, _} | CRest], Lookup=[{LIdx, _, _} | _]) when CIdx < LIdx + W ->
    [CF | xloop(I, W, V, Max, CRest, Lookup)];


xloop(I, W, V, Max, [CF={CIdx, CVal, _} | CRest], [{LIdx, LVal, Causes} | LRest]) when (CIdx =:= LIdx + W) ->
    NewVal= LVal + V,
    if
        NewVal > CVal -> Front={CIdx, NewVal, [I | Causes]};
        true -> Front=CF
    end,
    
    [Front | xloop(I, W, V, Max, CRest, LRest)];


xloop(I, W, V, Max, Copy, [{LIdx, LVal, Causes} | LRest]) ->
    [{LIdx + W, LVal + V, [I | Causes]} | xloop(I, W, V, Max, Copy, LRest)].
    
    
q1() ->
    Problem=load_problem("knapsack1.txt"),
    knapsack(Problem).


q2() ->
    Problem=load_problem("knapsack_big.txt"),
    knapsack(Problem).

test() ->
    Problem={6, [#item{value=3,weight=4}, #item{value=2,weight=3},#item{value=4,weight=2},#item{value=4,weight=3}], 4},
    knapsack(Problem).