-module(solution_part1).
-export([solve/1]).
%-define(DOT_UNICODE, 46). % 46 is the unicode value for a dot

solve(Lines) ->
    [Coordinates, FoundNumbers, MaxX, MaxY] = parse_lines(Lines, 1, [#{}, #{}]),
    Result = get_valid_sum(Coordinates, FoundNumbers, MaxX, MaxY),
    erlang:display(Result),
    Lines.

% Transform the unusable input into two maps of the following format:
% #{ {X, Y} => Char }, #{ id => [Number, {StartX, EndX, Y}] }
% where X and Y are the coordinates of the map and Char is the character
% coordinates
% and id is the identifier for a number (good enough as y--x which should
% be unique), Number is the identified number, StartX and EndX are the
% X positions of that number
% to get a specific coordinate, use maps:get({X, Y}, Map)
parse_lines(Lines, Y, [Coordinates, FoundNumbers]) ->
    if (Y > length(Lines)) ->
        % MaxX is at -1 due to the extra \n at the end of each line
        [Coordinates, FoundNumbers, length(lists:nth(1, Lines))-1, length(Lines)];
    true ->
       [NewCoordinates, NewFoundNumbers] = parse_line(lists:nth(Y, Lines), Y, [Coordinates, FoundNumbers]),
       parse_lines(Lines, Y + 1, [NewCoordinates, NewFoundNumbers])
    end.

parse_line(LineWithEndline, Y, [Coordinates, FoundNumbers]) ->
    % Get current Line and displays it for debug
    Line = string:strip(LineWithEndline, right, $\n),

    ParsedInfo = handle_char(Line, {Y, 1}, #{}),
    NewCoordinates = maps:merge(Coordinates, maps:get(coordinates, ParsedInfo)),
    NewFoundNumbers = maps:merge(FoundNumbers, maps:get(numbers, ParsedInfo, #{})),
    [NewCoordinates, NewFoundNumbers].

%ParsedInfo = #{numbers => #{}, currentlyParsingNumber => #{}, coordinates => #{}},
handle_char(Line, {Y, X}, ParsedInfo) ->
    if (X > length(Line)) ->
        % If we are still parsing a number, complete it
        % else do nothing
        CurrentlyParsing = maps:get(currentlyParsingNumber, ParsedInfo, #{}),
        ID = maps:get(id, CurrentlyParsing, "none"),
        case ID of
            "none" -> ParsedInfo;
            _ ->
                Id = maps:get(id, CurrentlyParsing),
                Number = maps:get(number, CurrentlyParsing),
                StartX = maps:get(startX, CurrentlyParsing),
                EndX = X - 1,
                NewNumbers = maps:put(Id, [Number, {StartX, EndX, Y}], maps:get(numbers, ParsedInfo, #{})),
                NewParsedInfo = maps:put(numbers, NewNumbers, ParsedInfo),
                NewParsedInfo
        end;
    true ->
        Char = lists:nth(X, Line),
        NewCoordinates = maps:put({X, Y}, Char, maps:get(coordinates, ParsedInfo, #{})),
        OnlyCoordinatesInfo = maps:put(coordinates, NewCoordinates, #{}),
        % check if Chat is a number
        % if it is, print it
        Numbers = maps:get(numbers, ParsedInfo, #{}),
        CurrentlyParsing = maps:get(currentlyParsingNumber, ParsedInfo, #{}),
        case Char of
            $0 -> [NewNumbers, NewCurrentlyParsing] = handle_char_is_number([Y, X], [Numbers, CurrentlyParsing], Char);
            $1 -> [NewNumbers, NewCurrentlyParsing] = handle_char_is_number([Y, X], [Numbers, CurrentlyParsing], Char);
            $2 -> [NewNumbers, NewCurrentlyParsing] = handle_char_is_number([Y, X], [Numbers, CurrentlyParsing], Char);
            $3 -> [NewNumbers, NewCurrentlyParsing] = handle_char_is_number([Y, X], [Numbers, CurrentlyParsing], Char);
            $4 -> [NewNumbers, NewCurrentlyParsing] = handle_char_is_number([Y, X], [Numbers, CurrentlyParsing], Char);
            $5 -> [NewNumbers, NewCurrentlyParsing] = handle_char_is_number([Y, X], [Numbers, CurrentlyParsing], Char);
            $6 -> [NewNumbers, NewCurrentlyParsing] = handle_char_is_number([Y, X], [Numbers, CurrentlyParsing], Char);
            $7 -> [NewNumbers, NewCurrentlyParsing] = handle_char_is_number([Y, X], [Numbers, CurrentlyParsing], Char);
            $8 -> [NewNumbers, NewCurrentlyParsing] = handle_char_is_number([Y, X], [Numbers, CurrentlyParsing], Char);
            $9 -> [NewNumbers, NewCurrentlyParsing] = handle_char_is_number([Y, X], [Numbers, CurrentlyParsing], Char);
            _ -> [NewNumbers, NewCurrentlyParsing] = handle_char_is_not_number([Y, X], [Numbers, CurrentlyParsing])
        end,
        NumbersAndCoordinatesInfo = maps:put(numbers, NewNumbers, OnlyCoordinatesInfo),
        NewParsedInfo = maps:put(currentlyParsingNumber, NewCurrentlyParsing, NumbersAndCoordinatesInfo),
        handle_char(Line, {Y, X + 1}, NewParsedInfo)
    end.

% #{ id => [Number, {StartX, EndX, Y}] }
% # [NumberSoFar, [StartX, CurrentX, Y], id]
handle_char_is_number([Y, X], [Numbers, CurrentlyParsing], Char) ->
    %io:format("Found number at ~p~n", [X]),
    % if currently parsing a number, add to it
    % else start parsing a new number
    if CurrentlyParsing == #{} ->
        Id = integer_to_list(Y) ++ "--" ++ integer_to_list(X),
        NewCurrentlyParsing = #{
                                id=> Id,
                                number=> [Char],
                                startX=> X,
                                y=> Y
                               };
    true ->
        CurrentNumber = maps:get(number, CurrentlyParsing, []),
        NewNumber = CurrentNumber ++ [Char],
        NewCurrentlyParsing = maps:put(number, NewNumber, CurrentlyParsing)
    end,
    [Numbers, NewCurrentlyParsing].

handle_char_is_not_number([Y, X], [Numbers, CurrentlyParsing]) ->
    % if currently parsing a number, complete it
    % else do nothing
    if CurrentlyParsing == #{} ->
        [Numbers, CurrentlyParsing];
    true ->
        Id = maps:get(id, CurrentlyParsing),
        Number = maps:get(number, CurrentlyParsing),
        StartX = maps:get(startX, CurrentlyParsing),
        EndX = X - 1,
        NewNumbers = maps:put(Id, [Number, {StartX, EndX, Y}], Numbers),
        [NewNumbers, #{}]
    end.

get_valid_sum(Coordinates, FoundNumbers, MaxX, MaxY) ->
    NumbersIds = maps:keys(FoundNumbers),
    ValidNumbers = lists:map(fun(Id) ->
        Number = maps:get(Id, FoundNumbers),
        validate_number(Number, Coordinates, MaxX, MaxY)
    end, NumbersIds),
    lists:foldl(fun(X, Acc) -> X + Acc end, 0, ValidNumbers).


validate_number([Number, {StartX, EndX, Y}], Coordinates, MaxX, MaxY) ->
    % First let's build a list of coordinates to check
    case StartX of 
        1 -> StartXCheck = 1;
        _ -> StartXCheck = StartX - 1
    end,

    case EndX of
        MaxX -> EndXCheck = MaxX;
        _ -> EndXCheck = EndX + 1
    end,

    case Y of
        1 -> StartYCheck = 1;
        _ -> StartYCheck = Y - 1
    end,

    case Y of
        MaxY -> EndYCheck = MaxY;
        _ -> EndYCheck = Y + 1
    end,

    CoordinatesToCheck = lists:foldl(fun(X, Acc) ->
        lists:foldl(fun(CurY, Acc2) ->
            lists:append(Acc2, [{X, CurY}])
        end, Acc, lists:seq(StartYCheck, EndYCheck))
    end, [], lists:seq(StartXCheck, EndXCheck)),

    % Now let's check if any of the coordinates are valid
    ValidCoordinates = lists:filter(fun({X, CurY}) ->
        case maps:get({X, CurY}, Coordinates) of
            $0 -> false;
            $1 -> false;
            $2 -> false;
            $3 -> false;
            $4 -> false;
            $5 -> false;
            $6 -> false;
            $7 -> false;
            $8 -> false;
            $9 -> false;
            $. -> false;
            _ -> true
        end
    end, CoordinatesToCheck),

    % If there are no valid coordinates, return 0
    if ValidCoordinates == [] ->
        Result = 0;
    true ->
        % Else return the number
        {Result, _} = string:to_integer(Number)
    end,
    Result.
