-module(solution_part2).
-export([solve/1]).

solve(Lines) ->
    [Coordinates, FoundNumbers] = parse_lines(Lines, 1, [#{}, #{}]),
    Result = get_valid_sum(Coordinates, FoundNumbers, length(lists:nth(1, Lines))-1, length(Lines)),
    erlang:display(Result),
    Result.

% Transform the unusable input into two maps of the following format:
% Coordinates: #{ {X, Y} => Char }
% where X and Y are the coordinates of the map and Char is the character
% coordinates
% FoundNumbers: #{ id => [Number, {StartX, EndX, Y}] }
% where id is the identifier for a number (Y--StartX is good enough),
% Number is the identified number, StartX and EndX are the X positions of
% the number, Y is its Y position
parse_lines(Lines, Y, [Coordinates, FoundNumbers]) ->
    if (Y > length(Lines)) ->
        [Coordinates, FoundNumbers];
    true ->
       [NewCoordinates, NewFoundNumbers] = parse_line(lists:nth(Y, Lines), Y, [Coordinates, FoundNumbers]),
       parse_lines(Lines, Y + 1, [NewCoordinates, NewFoundNumbers])
    end.

% Goal here is to update teh Coordinates and FoundNumbers maps for the
% current line
parse_line(LineWithEndline, Y, [Coordinates, FoundNumbers]) ->
    % All lines end with a newline character, so we need to remove it
    Line = string:strip(LineWithEndline, right, $\n),
    % Get the `ParsedInfos` object for the current line (see method for
    % details)
    ParsedInfos = handle_char(Line, {Y, 1}, #{}),
    % Merge the output of the parsing with the current Coordinates and
    % FoundNumbers maps
    NewCoordinates = maps:merge(Coordinates, maps:get(coordinates, ParsedInfos)),
    NewFoundNumbers = maps:merge(FoundNumbers, maps:get(numbers, ParsedInfos, #{})),
    % We're done, return and move onto the next line
    [NewCoordinates, NewFoundNumbers].

% This method's purpose is to generate the ParsedInfo object which contains:
% - coordinates: a map of the coordinates of the current line
% - numbers: a map of the numbers found on the current line
% - currentlyParsingNumber: a map of the number currently being parsed
% CurrentlyParsingNumber is used to keep track of the number being parsed
% as we move from digit to digit
handle_char(Line, {Y, X}, ParsedInfos) ->
    if (X > length(Line)) ->
        % If we are still parsing a number, complete it
        % else do nothing
        CurrentlyParsing = maps:get(currentlyParsingNumber, ParsedInfos, #{}),
        Id = maps:get(id, CurrentlyParsing, "none"),
        case Id of
            "none" -> ParsedInfos;
            _ ->
                Number = maps:get(number, CurrentlyParsing),
                StartX = maps:get(startX, CurrentlyParsing),
                AlreadyParsedToExtend = maps:get(numbers, ParsedInfos, #{}),
                EndX = X - 1,
                NewNumbers = maps:put(Id, [Number, {StartX, EndX, Y}], AlreadyParsedToExtend),
                NewParsedInfo = maps:put(numbers, NewNumbers, ParsedInfos),
                NewParsedInfo
        end;
    true ->
        Char = lists:nth(X, Line),
        % First let's update the coordinates map
        NewCoordinates = maps:put({X, Y}, Char, maps:get(coordinates, ParsedInfos, #{})),
        OnlyCoordinatesInfo = maps:put(coordinates, NewCoordinates, #{}),

        % Now let's update the other two maps
        case Char of
            $0 -> [NewNumbers, NewCurrentlyParsing] = handle_char_is_number([Y, X], ParsedInfos, Char);
            $1 -> [NewNumbers, NewCurrentlyParsing] = handle_char_is_number([Y, X], ParsedInfos, Char);
            $2 -> [NewNumbers, NewCurrentlyParsing] = handle_char_is_number([Y, X], ParsedInfos, Char);
            $3 -> [NewNumbers, NewCurrentlyParsing] = handle_char_is_number([Y, X], ParsedInfos, Char);
            $4 -> [NewNumbers, NewCurrentlyParsing] = handle_char_is_number([Y, X], ParsedInfos, Char);
            $5 -> [NewNumbers, NewCurrentlyParsing] = handle_char_is_number([Y, X], ParsedInfos, Char);
            $6 -> [NewNumbers, NewCurrentlyParsing] = handle_char_is_number([Y, X], ParsedInfos, Char);
            $7 -> [NewNumbers, NewCurrentlyParsing] = handle_char_is_number([Y, X], ParsedInfos, Char);
            $8 -> [NewNumbers, NewCurrentlyParsing] = handle_char_is_number([Y, X], ParsedInfos, Char);
            $9 -> [NewNumbers, NewCurrentlyParsing] = handle_char_is_number([Y, X], ParsedInfos, Char);
            _ -> [NewNumbers, NewCurrentlyParsing] = handle_char_is_not_number([Y, X], ParsedInfos)
        end,
        NumbersAndCoordinatesInfo = maps:put(numbers, NewNumbers, OnlyCoordinatesInfo),
        NewParsedInfo = maps:put(currentlyParsingNumber, NewCurrentlyParsing, NumbersAndCoordinatesInfo),
        % Moving onto the next character
        handle_char(Line, {Y, X + 1}, NewParsedInfo)
    end.

% If the character is a number, either we create the currentlyParsingNumber
% or we update it
handle_char_is_number([Y, X], ParsedInfos, Char) ->
    Numbers = maps:get(numbers, ParsedInfos, #{}),
    CurrentlyParsing = maps:get(currentlyParsingNumber, ParsedInfos, #{}),
    % Case where we start parsing a new number
    if CurrentlyParsing == #{} ->
        Id = integer_to_list(Y) ++ "--" ++ integer_to_list(X),
        NewCurrentlyParsing = #{
                                id=> Id,
                                number=> [Char],
                                startX=> X,
                                y=> Y
                               };
    % Case where we continue parsing the current number
    true ->
        CurrentNumber = maps:get(number, CurrentlyParsing, []),
        NewNumber = CurrentNumber ++ [Char],
        NewCurrentlyParsing = maps:put(number, NewNumber, CurrentlyParsing)
    end,
    [Numbers, NewCurrentlyParsing].

% If the character isn't a number, either we complete the
% currentlyParsingNumber into a found number, or we do nothing
handle_char_is_not_number([Y, X], ParsedInfos) ->
    Numbers = maps:get(numbers, ParsedInfos, #{}),
    CurrentlyParsing = maps:get(currentlyParsingNumber, ParsedInfos, #{}),
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

% Now that we have two maps we can work with, let's find the sum
% as it's the goal of the exercise after all
get_valid_sum(Coordinates, FoundNumbers, MaxX, MaxY) ->

    NumbersWithStar = get_numbers_and_stars(Coordinates, FoundNumbers, MaxX, MaxY),
    NumbersGroupedByStars = group_numbers_by_stars(NumbersWithStar),
    sum_all_star_groups(NumbersGroupedByStars).

% Finally, let's product all the numbers and sum the products
sum_all_star_groups(NumbersGroupedByStars) ->
    lists:foldl(fun({_, [A,B]}, Acc) ->
        Acc + A * B
    end
    , 0, NumbersGroupedByStars).

% This method will return a list of numbers sitting near a * character
% along with the coordinates of the one or multiple * character(s)
get_numbers_and_stars(Coordinates, FoundNumbers, MaxX, MaxY) ->
    NumbersIds = maps:keys(FoundNumbers),
    NumbersWithStarsAndOk = lists:map(fun(Id) ->
        Number = maps:get(Id, FoundNumbers),
        check_star_proximity(Number, Coordinates, MaxX, MaxY)
    end, NumbersIds),
    % Filter out ok from the list
    lists:filter(fun(X) ->
        case X of
            ok -> false;
            _ -> true
        end
    end, NumbersWithStarsAndOk).

group_numbers_by_stars(NumbersWithStars) ->
    AllGroups = lists:foldl(fun group_numbers_by_stars/2, #{}, NumbersWithStars),
    % Filter groups to only those with exactly two numbers
    lists:filter(fun({_, Numbers}) ->
        length(Numbers) == 2
    end, maps:to_list(AllGroups)).

group_numbers_by_stars(NumbersWithStars, Acc) ->
    Number = maps:get(number, NumbersWithStars),
    Stars = maps:get(stars, NumbersWithStars),
    lists:foldl(fun(Star, Acc2) -> add_number_to_star_group(Number, Star, Acc2) end, Acc, Stars).

add_number_to_star_group(Number, Star, Acc) ->
    case maps:is_key(Star, Acc) of
        true ->
            CurrentNumbers = maps:get(Star, Acc),
            NewNumbers = CurrentNumbers ++ [Number],
            maps:put(Star, NewNumbers, Acc);
        false ->
            maps:put(Star, [Number], Acc)
    end.

% This is where the actual check of the exercise is done. If the number is
% surrounded by any single non-dot non-number character, it is considered
% valid and be added to the list
check_star_proximity([Number, {StartX, EndX, Y}], Coordinates, MaxX, MaxY) ->
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

    ValidCoordinates = lists:filter(fun({X, CurY}) ->
        case maps:get({X, CurY}, Coordinates) of
            $* -> true;
            _ -> false
        end
    end, CoordinatesToCheck),


    if ValidCoordinates == [] ->
        ok;
    true ->
        {Result, _} = string:to_integer(Number),
        #{stars=> ValidCoordinates, number=> Result}
    end.
