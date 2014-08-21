-module(refactor_me_tests).
-include_lib("eunit/include/eunit.hrl").

refactor_me_test() ->
    Config = [{parent,self()},
	      {max_timeout,500},
	      {max_fails,4}
	     ],
    %% test that this can start
    ?assertEqual(ok, refactor_me:start(Config)),
    receive {port,Port} -> ok end,    
    receive {progress,X} -> ok end,
    ?assertEqual(listening,X),
    io:format(user,"> can start - OK~n",[]),
    %% we are connected
    {ok, Sock} = gen_tcp:connect("localhost",Port,[],500),
    receive {progress,Y} -> ok end,
    ?assertEqual(connected,Y),
    io:format(user,"> can receive connections - OK~n",[]),
    %% now close and test timeout
    gen_tcp:close(Sock),
    receive {progress,A} -> ok end,
    ?assertEqual(closed,A),
    io:format(user,"> can see when other end closes - OK~n",[]),
    {ok, Sock2} = gen_tcp:connect("localhost",Port,[],500),
    receive {progress,B} -> ok end,
    ?assertEqual(connected, B),
    timer:sleep(700),
    receive {progress,Z} -> ok end,	    
    ?assertEqual(timeout_on_receive, Z),
    io:format(user,"> can detect timeouts on receive - OK~n",[]),
    gen_tcp:close(Sock2),
    %% test failure limit for closing and reopening (already failed once!)
    lists:foreach(
      fun(_) ->
	      {ok, Sock_N} = gen_tcp:connect("localhost",Port,[],500),
	      receive {progress,D} -> ok end,
	      ?assertEqual(connected, D),
	      timer:sleep(700),
	      receive {progress,Z2} -> ok end,	    
	      ?assertEqual(timeout_on_receive, Z2),
	      gen_tcp:close(Sock_N)
      end, [1,2,3]),
    receive {progress,Q} -> ok end,
    ?assertEqual(reopening,Q),
    receive {port,Port2} -> ok end,
    ?assert(Port2 =/= Port),
    receive {progress,X2} -> ok end,
    ?assertEqual(listening,X2),
    Res = gen_tcp:connect("localhost",Port,[],500),
    ?assertEqual({error,econnrefused},Res),
    io:format(user,"> can close and reopen on max failures reached - OK~n",[]),
    %% test the history of what has happened
    {ok, Sock3} = gen_tcp:connect("localhost",Port2,[{active,false}],500),
    receive {progress,Y2} -> ok end,
    ?assertEqual(connected,Y2),
    ok = gen_tcp:send(Sock3,"history"),
    {ok,Data} = gen_tcp:recv(Sock3,0),
    ?assertEqual([history,connected,listening,reopening,timeout_on_receive,
		  connected,timeout_on_receive,connected,timeout_on_receive,
		  connected,timeout_on_receive,connected,closed,connected,
		  listening],
		 [ list_to_atom(S) || S <-string:tokens(Data,"\n")]),
    io:format(user,"> can keep track of history and return it - OK~n",[]).
