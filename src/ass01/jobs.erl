-module(jobs).
-export([read_jobs/0, construct_heap/2, compare_difference/2, compare_ratio/2]).

read_jobs() -> 
    {Ok,Device} = file:open("jobs.txt", [read]),
    {Ok,_} = io:fread(Device, "", "~d"),
    try get_all_jobs(Device)
        after file:close(Device)
    end.

get_all_jobs(Device) ->
    case io:fread(Device, "", "~d ~d") of
        {ok,[JobWeight,JobLength]} -> [{job, JobWeight, JobLength} | get_all_jobs(Device)];
        eof -> []
    end.


compare_difference({job, W1, T1}, {job, W2, T2}) ->
    Diff= (W2 - T2) - (W1 - T1),
    if
        Diff == 0 -> W1 > W2;
        true -> Diff < 0
    end.

compare_ratio({job, W1, T1}, {job, W2, T2}) ->
    Diff= (W2 / T2) - (W1 / T1),
    Diff < 0.


construct_heap(JobList, CompareFunc) -> internal_construct_heap(JobList, heap:new(CompareFunc)).
internal_construct_heap([], Heap) -> Heap;
internal_construct_heap([Job | Tail], Heap) -> internal_construct_heap(Tail, heap:insert(Job,Heap)).