-module(solution_part1).
-export([solve/1]).

solve(Lines) ->
    solve(Lines, 1, 0).

solve(Lines, Index, Acc) ->
    if 
        (Index > length(Lines)) -> Acc;
        true ->
            Line = lists:nth(Index, Lines),
            NewDigit = process_line(Line),
            solve(Lines, Index + 1, Acc + NewDigit)
    end.

% Overall idea is to find the smallest and highest index for each number
% (both in letter and numerical form), then find the smallest/highest index
% to get the first and last number.
process_line(Line) ->
    SmallestIndexes = [
        {1, find_smallest_index(Line, "one")},
        {2, find_smallest_index(Line, "two")},
        {3, find_smallest_index(Line, "three")},
        {4, find_smallest_index(Line, "four")},
        {5, find_smallest_index(Line, "five")},
        {6, find_smallest_index(Line, "six")},
        {7, find_smallest_index(Line, "seven")},
        {8, find_smallest_index(Line, "eight")},
        {9, find_smallest_index(Line, "nine")},
        {1, find_smallest_index(Line, "1")},
        {2, find_smallest_index(Line, "2")},
        {3, find_smallest_index(Line, "3")},
        {4, find_smallest_index(Line, "4")},
        {5, find_smallest_index(Line, "5")},
        {6, find_smallest_index(Line, "6")},
        {7, find_smallest_index(Line, "7")},
        {8, find_smallest_index(Line, "8")},
        {9, find_smallest_index(Line, "9")}
    ],
    SmallestIndexesButNotZero = lists:filter(fun({_, A}) -> A > 0 end, SmallestIndexes),
    SortedSmallestIndexes = lists:sort(fun({_, A}, {_, B}) -> A < B end, SmallestIndexesButNotZero),
    {FirstNumber, _}= lists:nth(1, SortedSmallestIndexes),

    HighestIndexes = [
        {1, find_highest_index(Line, "one", 0)},
        {2, find_highest_index(Line, "two", 0)},
        {3, find_highest_index(Line, "three", 0)},
        {4, find_highest_index(Line, "four", 0)},
        {5, find_highest_index(Line, "five", 0)},
        {6, find_highest_index(Line, "six", 0)},
        {7, find_highest_index(Line, "seven", 0)},
        {8, find_highest_index(Line, "eight", 0)},
        {9, find_highest_index(Line, "nine", 0)},
        {1, find_highest_index(Line, "1", 0)},
        {2, find_highest_index(Line, "2", 0)},
        {3, find_highest_index(Line, "3", 0)},
        {4, find_highest_index(Line, "4", 0)},
        {5, find_highest_index(Line, "5", 0)},
        {6, find_highest_index(Line, "6", 0)},
        {7, find_highest_index(Line, "7", 0)},
        {8, find_highest_index(Line, "8", 0)},
        {9, find_highest_index(Line, "9", 0)}
    ],
    HighestIndexesSorted = lists:sort(fun({_, A}, {_, B}) -> A > B end, HighestIndexes),
    {LastNumber, _} = lists:nth(1, HighestIndexesSorted),
    FirstNumber*10 + LastNumber.

find_index(Hay, Needle) ->
    case string:str(Hay, Needle) of
        0 -> 0;
        Index -> Index
    end.

find_smallest_index(Haystack, Needle) ->
    find_index(Haystack, Needle).

find_highest_index(Haystack, Needle, LastIndex) ->
    TrimmedHaystack = string:slice(Haystack, LastIndex),
    NewIndex = find_index(TrimmedHaystack, Needle),
    case NewIndex of
        0 -> LastIndex;
        _ -> find_highest_index(Haystack, Needle, LastIndex+NewIndex)
    end.
