-module (main).
-export ([part1/1, part2/1]).

part1 (Args) -> 
    %% Assuming the first element is the file name
    FileName = hd (Args),
    Lines = input_parser:read_file(FileName),
    Result = solution_part1:solve(Lines),
    Result.

part2 (Args) -> 
    %% Assuming the first element is the file name
    FileName = hd (Args),
    Lines = input_parser:read_file(FileName),
    Result = solution_part2:solve(Lines),
    erlang:display(Result).
