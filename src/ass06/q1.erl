-module(q1).
-export([q1/0]).

-record(clause,{
    a :: integer(),
    b :: integer()
}).

load_problem(Name) ->
    {Ok,Device} = file:open(string:concat("ass06/",Name), [read]),
    {Ok,[NumClauses]} = io:fread(Device, "", "~d"),
    Stat=myarray:new(0),
    try
        Clauses=get_all_clauses(Device, Stat),
        {NumClauses, Clauses, Stat}
        after file:close(Device)
    end.

get_all_clauses(Device, Stat) ->
    case io:fread(Device, "", "~d ~d") of
        {ok,[Lit1, Lit2]} ->
            myarray:set(Lit1, myarray:get(Lit1, Stat) + 1, Stat),
            myarray:set(Lit2, myarray:get(Lit2, Stat) + 1, Stat),
            [#clause{a=Lit1,b=Lit2} | get_all_clauses(Device, Stat)]; 
        eof -> []
    end.


preprocess(NumClauses, Clauses, Stat) ->
    {Changes, NewClauses}=preprocess(Clauses, Stat, [], 0),
    
    if
        Changes > 0 -> preprocess(NumClauses - Changes, NewClauses, Stat);
        true -> {NumClauses, Clauses}
    end.

preprocess([], _Stat, NewClauses, Changes) ->
    {Changes, NewClauses};

preprocess([#clause{a=A,b=B} | Rest], Stat, NewClauses, Changes) when A =:= -B ->
    AC=myarray:get(A, Stat),
    NAC=myarray:get(-A, Stat),
    
    if
        AC =< 1 -> myarray:remove(A, Stat);
        true -> myarray:set(A, AC - 1, Stat)
    end,
    if
        NAC =< 1 -> myarray:remove(-A, Stat);
        true -> myarray:set(-A, NAC - 1, Stat)
    end,
    preprocess(Rest, Stat, NewClauses, Changes+1);

preprocess([F=#clause{a=A,b=B} | Rest], Stat, NewClauses, Changes) ->
    NAC=myarray:get(-A, Stat),
    NBC=myarray:get(-B, Stat),
    
    if
        (NAC =:= 0) or (NBC =:= 0) ->
            AC=myarray:get(A, Stat),
            BC=myarray:get(B, Stat),
            if
                (AC =< 1) ->
                    myarray:remove(A, Stat);
                true ->
                    myarray:set(A, AC-1, Stat)
            end,
            if
                (BC =< 1) ->
                    myarray:remove(B, Stat);
                true ->
                    myarray:set(B, BC-1, Stat)
            end,
            preprocess(Rest, Stat, NewClauses, Changes+1);
        true ->
            preprocess(Rest, Stat, [F | NewClauses], Changes)
    end.
    

q1() ->
    Files=["2sat1.txt", "2sat2.txt", "2sat3.txt", "2sat4.txt", "2sat5.txt", "2sat6.txt"],
    q1(Files).


q1([]) -> done;
q1([First | Rest]) ->
    io:fwrite("loading problem ~s~n", [First]),
    {NumClauses, Clauses, Stat}=load_problem(First),
    io:fwrite("preprocess problem ~s with ~w clauses~n", [First,NumClauses]),
    {ReducedNum, ReducedClauses}=preprocess(NumClauses, Clauses, Stat),
    myarray:delete(Stat),
    
    io:fwrite("checking reduced problem ~s with ~w clauses", [First, ReducedNum]),
    Result=satisfiable(ReducedClauses),
    io:fwrite(" satisfiable? ~w~n", [Result]),
    q1(Rest).


satisfiable([]) -> true;
satisfiable([#clause{a=A, b=B} | CRest]) ->
    Ass=myarray:new(unknown),
    FixResult=fix_assignments(CRest, new_assignment(A, Ass)),
    myarray:delete(Ass),
    case FixResult of
        error ->
            NewAss=myarray:new(unknown),
            NewFixResult=fix_assignments(CRest, new_assignment(B, NewAss)),
            myarray:delete(NewAss),
            case NewFixResult of
                error -> false;
                RestClauses -> satisfiable(RestClauses)
            end;
        RestClauses -> satisfiable(RestClauses)
    end.


fix_assignments(Clauses, Ass) ->
    case fix_assignments(Clauses, Ass, [], 0) of
        {Changes, NewClauses} ->
            if
                Changes > 0 -> fix_assignments(NewClauses, Ass);
                true -> NewClauses
            end;
        error -> error
    end.

fix_assignments([], _, NewClauses, Changes) -> {Changes, NewClauses};
fix_assignments([#clause{a=A, b=A} | CRest], Ass, NewClauses, Changes) ->
    case get_assignment(A, Ass) of
        unknown -> fix_assignments(CRest, new_assignment(A, Ass), NewClauses, Changes+1);
        true -> fix_assignments(CRest, Ass, NewClauses, Changes+1);
        false -> error
    end;
fix_assignments([F=#clause{a=A, b=B} | CRest], Ass, NewClauses, Changes) ->
    case {get_assignment(A, Ass), get_assignment(B, Ass)} of
        {true, _} -> fix_assignments(CRest, Ass, NewClauses, Changes+1);
        {_, true} -> fix_assignments(CRest, Ass, NewClauses, Changes+1);
        {false, false} -> error;
        {unknown, false} -> fix_assignments(CRest, new_assignment(A, Ass), NewClauses, Changes+1);
        {false, unknown} -> fix_assignments(CRest, new_assignment(B, Ass), NewClauses, Changes+1);
        {unknown, unknown} -> fix_assignments(CRest, Ass, [F | NewClauses], Changes)
    end.


new_assignment(Num, Ass) when Num < 0 -> myarray:set(-Num, false, Ass), Ass;
new_assignment(Num, Ass) -> myarray:set(Num, true, Ass), Ass.


get_assignment(Num, Ass) when Num < 0 ->
    Result= get_assignment(-Num, Ass),
    
    if
        Result =:= unknown -> unknown;
        true -> (not Result)
    end;
get_assignment(Num, Ass) -> myarray:get(Num, Ass).
