#!/usr/bin/env escript
%%! -smp enable -sname main

-module (main).
-export ([main/1]).

main (Args) -> 
    %% Assuming the first element is the file name
    FileName = hd (Args),
    Lines = parser:read_file(FileName),
    parser:print_lines(Lines).
