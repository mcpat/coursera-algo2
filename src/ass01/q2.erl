-module(q2).
-export([weighted_completion_times/0]).

weighted_completion_times() ->
    JobList=jobs:read_jobs(),
    JobHeap=jobs:construct_heap(JobList, fun(A,B) -> jobs:compare_ratio(A,B) end),
    sum_up_times(JobHeap, 0, 0).

sum_up_times({heap, _, 0, _}, _, CompletionSum) -> CompletionSum;
sum_up_times(Heap, CurrentTime, CompletionSum) ->
    {{job, Weight, Time}, ModifiedHeap}=heap:extract_min(Heap),
    sum_up_times(ModifiedHeap, CurrentTime + Time, CompletionSum + ((CurrentTime + Time) * Weight)).