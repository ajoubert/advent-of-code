-module(solution_part1).
-export([solve/1]).

solve(Lines) ->
    solve(Lines, 1, 0).

solve(Lines, Index, Acc) ->
    if 
        (Index > length(Lines)) -> Acc;
        true ->
            Line = lists:nth(Index, Lines),
            Digits = find_digits(Line),
            First = lists:nth(1, Digits),
            First_int = First - $0,
            [Last | _] = lists:reverse(Digits),
            Last_int = Last - $0,
            solve(Lines, Index + 1, Acc + First_int*10 + Last_int)
    end.

find_digits(String) ->
    lists:filter(fun(Char) ->
        Char >= $0 andalso Char =< $9
    end, String).
