-module(formatter).
-export([format/1]).

format(Lines) ->
    format(Lines, 1, 0).

format(Lines, Index, Acc) ->
    if 
        (Index > length(Lines)) -> Acc;
        true ->
            Line = lists:nth(Index, Lines),
            Digits = find_digits(Line),
            %erlang:display(Digits),
            First = lists:nth(1, Digits),
            First_int = First - $0,
            %erlang:display(First_int),
            [Last | _] = lists:reverse(Digits),
            Last_int = Last - $0,
            %erlang:display(Last_int),
            format(Lines, Index + 1, Acc + First_int*10 + Last_int)
    end.

find_digits(String) ->
    lists:filter(fun(Char) ->
        Char >= $0 andalso Char =< $9
    end, String).
