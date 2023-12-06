-module(solution_part1).
-export([solve/1]).

-define(LIMIT_RED, 12).
-define(LIMIT_GREEN, 13).
-define(LIMIT_BLUE, 14).


solve(Lines) ->
    solve(Lines, 1, 0).

solve(Lines, Index, Acc) ->
    if Index > length(Lines) ->
        Acc;
    true ->
        Line = lists:nth(Index, Lines),
        solve(Lines, Index + 1, Acc + process_line(Line))
    end.

process_line(Line) ->
    [GameTitle, GameContentWithEndline] = string:split(Line, ": "),
    [_, GameNumberAsString] = string:split(GameTitle, " "),
    GameContent = string:substr(GameContentWithEndline, 1, string:len(GameContentWithEndline) - 1),
    {GameNumber, _} = string:to_integer(GameNumberAsString),
    GameDraws = string:split(GameContent, "; ", all),
    [MaxRed, MaxGreen, MaxBlue] = find_max_draws(GameDraws),
    % If any max is higher than the limit, return 0, else return GameNumber
    if MaxRed > ?LIMIT_RED; MaxGreen > ?LIMIT_GREEN; MaxBlue > ?LIMIT_BLUE ->
        0;
    true ->
        GameNumber
    end.

find_max_draws(Draws) ->
    find_max_draws(Draws, 1, [0,0,0]).

find_max_draws(Draws, Index, [MaxRed, MaxGreen, MaxBlue]) ->
    if Index > length(Draws) ->
        [MaxRed, MaxGreen, MaxBlue];
    true ->
        Draw = lists:nth(Index, Draws),
        DrawColors = string:split(Draw, ", ", all),
        [Red, Green, Blue] = process_draw_colors(DrawColors),
        find_max_draws(Draws, Index + 1, [max(MaxRed, Red), max(MaxGreen, Green), max(MaxBlue, Blue)])
    end.

process_draw_colors(DrawColors) ->
    process_draw_colors(DrawColors, 1, [0,0,0]).

process_draw_colors(DrawColors, Index, [Red, Green, Blue]) ->
    if Index > length(DrawColors) ->
        [Red, Green, Blue];
    true ->
           DrawColor = lists:nth(Index, DrawColors),
           [BallAmountAsString, BallColor] = string:split(DrawColor, " "),
           % My LSP doesn't analyze the type of to_integer properly, have
           % to push this to a different function for it to behave
           BallAmount = get_color(BallAmountAsString),
           case BallColor of
               "red" ->
                   process_draw_colors(DrawColors, Index + 1, [Red + BallAmount, Green, Blue]);
               "green" ->
                   process_draw_colors(DrawColors, Index + 1, [Red, Green + BallAmount, Blue]);
               "blue" ->
                   process_draw_colors(DrawColors, Index + 1, [Red, Green, Blue + BallAmount])
           end
    end.

get_color(NumberAsString) ->
    {Number, _} = string:to_integer(NumberAsString),
    Number.
