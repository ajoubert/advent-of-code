-module(parser).
-export([read_file/1, print_lines/1, print_file/1]).

% Reads a file and stores its contents line by line in a list
read_file(FileName) ->
    {ok, Device} = file:open(FileName, [read]),
    Lines = read_lines(Device, []),
    file:close(Device),
    lists:reverse(Lines),
    Lines.

% Helper function to read lines recursively
read_lines(Device, Accum) ->
    case file:read_line(Device) of
        {ok, Line} -> read_lines(Device, [Line | Accum]);
        eof -> Accum;
        {error, _} = Error -> Error
    end.

% Prints a list of lines
print_lines([]) -> ok;
print_lines([Line | Lines]) ->
    io:format("~s", [Line]),
    print_lines(Lines).

% Prints the contents of a file
print_file(FileName) ->
    Lines = read_file(FileName),
    print_lines(Lines).
