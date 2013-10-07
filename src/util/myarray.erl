-module(myarray).
-export([get/2, new/1, set/3, remove/2, delete/1]).
-export_type([myarray/0]).

-record(myarray,{
    default :: term(),
    repr :: ets:tid()
}).

-opaque myarray() :: #myarray{}.

-spec new(Default :: term()) -> Array :: myarray().
new(Default) ->
    ARepr=ets:new(?MODULE, ['set']),
    #myarray{default=Default,repr=ARepr}.


-spec delete(Array :: array()) -> boolean().
delete(#myarray{default=_D, repr=R}) ->
    ets:delete(R).


-spec get(Index :: term(), Array :: myarray()) -> Value :: term().
get(Index, #myarray{default=D,repr=R}) ->
    case ets:lookup(R, Index) of
        [] -> D;
        [{Index, Value}] -> Value
    end.

-spec set(Index :: term(), Value :: term(), Array :: myarray()) -> ok.
set(Index, Value, #myarray{default=_D,repr=R}) ->
    ets:insert(R, {Index,Value}),
    true.

-spec remove(Index :: term(), Array :: myarray()) -> ok.
remove(Index, #myarray{default=_D,repr=R}) ->
    ets:delete(R, Index).