-module(main).
-export([run_tests/0]).

run_tests() ->    
    cover:start(),
    {ok,refactor_me} = cover:compile_beam(refactor_me),
    Res = eunit:test(refactor_me),
    {ok,Answer} = cover:analyse(refactor_me,line),
    Coverage = calculate_coverage(Answer),
    io:format("COVERAGE:~p ALLPASSING:~p~n",[Coverage,Res==ok]),
    init:stop().

calculate_coverage(Result) ->
    Covered = length([1||{_,{1,0}}<-Result]),
    Covered/length(Result).
