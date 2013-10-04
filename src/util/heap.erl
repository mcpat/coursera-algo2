-module(heap).
-export([new/1,delete/1,size/1,insert/2,extract_min/1,walk/3,test/1]).
-export_type([comparator/0,heap/0]).

-record(heap,{
    comparator :: comparator(),
    repr :: ets:tab()
}).

-opaque heap() :: #heap{}.
-type comparator() :: fun((EA :: term(), EB :: term()) -> boolean()).


-spec new(comparator()) -> heap().
new(C) ->
    Hrepr= ets:new(?MODULE, ['ordered_set']),
    ets:insert(Hrepr, {'count', 0}),
    #heap{comparator=C,repr=Hrepr}.


-spec delete(heap()) -> boolean().
delete(H) -> ets:delete(H#heap.repr).


-spec size(heap()) -> pos_integer().
size(H) ->
    [{_,Count}]=ets:lookup(H#heap.repr, 'count'),
    Count.


-spec insert(term(), heap()) -> heap().
insert(E, H) ->
    Pos=heap:size(H),
    Count=Pos + 1,
    ets:insert(H#heap.repr, {Pos, E}),
    ets:insert(H#heap.repr, {'count', Count}),
    upheap(Pos, H),
    H.


-spec upheap(pos_integer(), heap()) -> boolean().
upheap(0, _) -> true;
upheap(Idx, H) ->
    [{_, Child}]=ets:lookup(H#heap.repr, Idx),
    Pdx=Idx div 2,
    [{_, Parent}]=ets:lookup(H#heap.repr, Pdx),
    case (H#heap.comparator)(Child, Parent) of
        true ->
            swap(Idx, Pdx, H),
            upheap(Pdx, H);
        false -> true
    end.


-spec downheap(pos_integer(), pos_integer(), heap()) -> boolean().
downheap(Idx, Count, H) when Idx < (Count div 2) ->
    LeftIdx= (Idx * 2) + 1,
    RightIdx= (Idx * 2) + 2,
    
    case get_swap_position(Idx, LeftIdx, RightIdx, Count, H) of
        {ok, SwpIdx} -> 
            swap(Idx, SwpIdx, H),
            downheap(SwpIdx, Count, H);
        none -> true
    end;
downheap(_,_,_) -> true.

get_swap_position(Idx, LeftIdx, RightIdx, Count, H) ->
    [{_, Current}]=ets:lookup(H#heap.repr, Idx),
    [{_, LeftChild}]=ets:lookup(H#heap.repr, LeftIdx),
    
    LeftCheck=(H#heap.comparator)(LeftChild, Current),
    if
        RightIdx < Count -> 
           [{_, RightChild}]=ets:lookup(H#heap.repr, RightIdx),
           RightCheck=(H#heap.comparator)(RightChild, Current);
        true -> RightCheck=false,RightChild=none
    end,
    
    if
        RightCheck and LeftCheck ->
            case (H#heap.comparator)(LeftChild, RightChild) of
                true -> {ok, LeftIdx};
                false -> {ok, RightIdx}
            end;
        RightCheck -> {ok, RightIdx};
        LeftCheck -> {ok, LeftIdx};
        true -> none
    end.


-spec swap(pos_integer(), pos_integer(), heap()) -> boolean().
swap(A, A, _) -> false;
swap(A, B, H) ->
    [{_, ElementA}]=ets:lookup(H#heap.repr, A),
    [{_, ElementB}]=ets:lookup(H#heap.repr, B),
    ets:insert(H#heap.repr, {A, ElementB}),
    ets:insert(H#heap.repr, {B, ElementA}),
    true.


-spec extract_min(heap()) -> term().
extract_min(H) ->
    remove_element(0, heap:size(H), H).


-spec walk(fun((term(), term()) -> term()), term(), heap()) -> term().
walk(F, Acc, H) ->
    {ResultAcc, ListAcc}=walk_internal(fun(NE, {OA,LA}) -> {F(NE, OA), [NE|LA]} end, {Acc, []}, H, heap:size(H)),
    % rebuild heap
    lists:foldl(fun heap:insert/2, H, ListAcc),
    ResultAcc.


walk_internal(F, Acc, _, 0) when is_function(F, 2) -> Acc;
walk_internal(F, Acc, H, Count) ->
    walk_internal(F, F(extract_min(H), Acc), H, Count-1).
    

-spec remove_element(pos_integer(), pos_integer(), heap()) -> term().
remove_element(Idx, Count, H) when Idx < Count ->
    [{_, Result}]=ets:lookup(H#heap.repr, Idx),
    NewCount= Count - 1,
    swap(Idx, NewCount, H),
    ets:insert(H#heap.repr, {'count', NewCount}),
    ets:delete(H#heap.repr, NewCount),
    downheap(Idx, NewCount, H),
    Result.


test(List) ->
    Heap=lists:foldl(fun heap:insert/2, heap:new(fun(A,B) -> A < B end), List),
    heap:walk(fun(E, _) -> io:fwrite("~w\n", [E]), none end, none, Heap).