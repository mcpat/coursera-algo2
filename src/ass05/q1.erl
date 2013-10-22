-module(q1).
-export([tsp/0]).

-define(INFINITY, 16#FFFFFFFF).

load_tsp() ->
    {Ok,Device} = file:open("ass05/tsp.txt", [read]),
    {Ok,[NumCities]} = io:fread(Device, "", "~d"),
    try {NumCities, get_all_cities(Device, 1)}
        after file:close(Device)
    end.

get_all_cities(Device, Num) ->
    case io:fread(Device, "", "~f ~f") of
        {ok,[Start,End]} ->[{Num, Start,End} | get_all_cities(Device, Num bsl 1)];
        eof -> []
    end.


get_distance_matrix(Cities) ->
    A=myarray:new(?INFINITY),
    calc_distance_matrix(Cities, Cities, A),
    A.

calc_distance_matrix([], _, _) -> ok;
calc_distance_matrix([_First|Rest], [], A) ->
    calc_distance_matrix(Rest, Rest, A);
calc_distance_matrix(FromList=[{Num,_,_} | _], [{Num,_,_}|ToRest], A) ->
    calc_distance_matrix(FromList, ToRest, A);
calc_distance_matrix(FromList=[From={FN,_,_}|_], [To={TN,_,_}|ToRest], A) ->
    myarray:set(FN+TN, distance(From,To), A),
    calc_distance_matrix(FromList, ToRest, A).

distance({_ANum, AX, AY}, {_BNum, BX, BY}) ->
    math:sqrt(math:pow(AX - BX, 2) + math:pow(AY - BY, 2)).


perm(List, LSize, PSize, Fun) ->
    perm(List, LSize, PSize, [], 0, Fun).

perm(_List, _LSize, 0, Perm, Pid, Fun) -> Fun(Pid,Perm);
perm(_List, 0, _PSize, _Perm, _Pid, _Fun) -> ok;

perm(_List, LSize, PSize, _Perm, _Pid, _Fun) when LSize < PSize -> ok;

perm([OF={Num,_,_} | OR], LSize, PSize, Perm, Pid, Fun) ->
    perm(OR, LSize - 1, PSize - 1, [OF | Perm], Pid bor Num, Fun),
    perm(OR, LSize - 1, PSize, Perm, Pid, Fun).


get_min_distance(A, Sid, [Start | Rest], DM) -> 
    get_min_distance(A, Sid, Start, Rest, DM, ?INFINITY).

get_min_distance(_A, _Sid, _Start, [], _DM, Min) -> Min;
get_min_distance(A, Sid, Start={SNum,_,_}, [{JNum,_,_} | JRest], DM, Min) ->
    Check= myarray:get({Sid, JNum}, A) + get_distance(SNum, JNum, DM),
    get_min_distance(A, Sid, Start, JRest, DM, erlang:min(Check, Min)).


get_distance(Aid, Bid, DM) ->
    Index= Aid bor Bid,
    myarray:get(Index, DM).

% obtained the solution visually...
cheat(DM) ->
    calc_distance([1,5,8,4,3,7,9,13,14,16,24,25,20,17,21,23,22,18,19,15,12,11,10,6,2,1], 0, DM).

calc_distance([_|[]], D, _) -> D;
calc_distance([F,S|Rest], D, DM) ->
    Dist=get_distance((1 bsl (F - 1)), (1 bsl (S - 1)), DM),
    calc_distance([S|Rest], D + Dist, DM).

tsp() ->
    {Count,TSP}=load_tsp(),
    DM=get_distance_matrix(TSP),
%%     cheat(DM).
    A0=myarray:new(?INFINITY),
    myarray:set({1, 1}, 0, A0),
    AN=mloop(TSP, 2, 20, A0, DM),
    get_min_distance(AN, 16#FFFFF, TSP, DM).


mloop(_TSP, M, Count, A, _DM) when M > Count -> A;
mloop(TSP=[First | Rest], M, Count, A, DM) ->
    io:fwrite("M == ~w~n", [M]),
    NewA=myarray:new(?INFINITY),
    perm(Rest, Count-1, M-1,
         (fun(Pid, Perm) -> jloop(Pid bor 1, [First|Perm], A, NewA, DM) end)),
    myarray:delete(A),
    mloop(TSP, M+1, Count, NewA, DM).
    

jloop(Sid, S=[_Start|Rest], A, NewA, DM) ->
    jloop(Sid, Rest, S, A, NewA, DM).


jloop(_Sid, [], _S, _A, _NewA, _DM) -> ok;
jloop(Sid, [JCity={JNum,_,_} | JRest], S, A, NewA, DM) ->
    Min=kmin(Sid, JCity, S, A, DM, ?INFINITY),
    myarray:set({Sid, JNum}, Min, NewA),
    jloop(Sid, JRest, S, A, NewA, DM).


kmin(_Sid, _JCity, [], _A, _DM, Min) -> Min;
kmin(Sid, JCity={Num,_,_}, [{Num,_,_} | KRest], A, DM, Min) ->
    kmin(Sid, JCity, KRest, A, DM, Min);

kmin(Sid, JCity={JNum,_,_}, [KCity={KNum,_,_} | KRest], A, DM, Min) ->
    case myarray:get({Sid bxor JNum, KNum}, A) of
        ?INFINITY -> kmin(Sid, JCity, KRest, A, DM, Min);
        PathCost ->
            Check= PathCost + get_distance(JNum, KNum, DM),
            kmin(Sid, JCity, KRest, A, DM, erlang:min(Min, Check))
    end.