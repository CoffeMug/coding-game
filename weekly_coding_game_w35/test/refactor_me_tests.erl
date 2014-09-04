-module(refactor_me_tests).
-include_lib("eunit/include/eunit.hrl").

-define(Config, [{parent,self()},
 	         {max_timeout,500},
 	         {max_fails,4}]).

refactor_me_test_() ->
    {inorder, [fun can_start_/0,
	       fun can_open_port_/0,
	       fun can_recieve_connections_/0,
	       fun can_see_close_/0,
	       fun can_detect_timeout_/0,
	       fun can_limit_connections_/0,
	       fun can_return_history_/0
	      ]}.

can_start_() ->
    ets:new(tb, [set,public,named_table]),
    ?assertEqual(ok, refactor_me:start(?Config)),
    io:format(user,"Can start - OK~n",[]).

can_open_port_() ->
    receive {port, Port} -> ok end,    
    receive {progress,X} -> ok end,
    ets:insert(tb, {port, Port}),
    ?assertEqual(listening,X),
    io:format(user,"Can open a port - OK~n",[]).

can_recieve_connections_()->
    [{port, Port}]=ets:lookup(tb, port),
    {ok, Sock} = gen_tcp:connect("localhost", Port,[],500),
    receive {progress, Y} -> ok end,
    ?assertEqual(connected, Y),
    ets:insert(tb, {sock, Sock}),
    io:format(user,"Can receive connections - OK~n",[]).

can_see_close_() ->
    [{sock, Sock}] = ets:lookup(tb, sock),
    gen_tcp:close(Sock),
    receive {progress,A} -> ok end,
    ?assertEqual(closed,A),
    io:format(user,"Can see when other end closes - OK~n",[]).

can_detect_timeout_() ->
    [{port, Port}]=ets:lookup(tb, port),
    {ok, Sock} = gen_tcp:connect("localhost",Port,[],500),
    receive {progress, _} -> ok end,
    timer:sleep(700),
    receive {progress, Z} -> ok end,
    ?assertEqual(timeout_on_receive, Z),
    gen_tcp:close(Sock),
    io:format(user,"Can detect timeouts on receive - OK~n",[]).

can_limit_connections_() ->    
     [{port, Port}] = ets:lookup(tb, port),
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
    ets:insert(tb, {port, Port2}),
    ?assert(Port2 =/= Port),
    receive {progress,X2} -> ok end,
    ?assertEqual(listening,X2),
    Res = gen_tcp:connect("localhost",Port,[],500),
    ?assertEqual({error,econnrefused},Res),
    io:format(user,"Can close and reopen on max failures reached - OK~n",[]).

can_return_history_() ->
    [{port, Port}] = ets:lookup(tb, port),
    {ok, Sock} = gen_tcp:connect("localhost",Port,[{active,false}],500),
    ok = gen_tcp:send(Sock,"history"),
    {ok,Data} = gen_tcp:recv(Sock,0),
    ?assertEqual([history,connected,listening,reopening,timeout_on_receive,
                  connected,timeout_on_receive,connected,timeout_on_receive,
                  connected,timeout_on_receive,connected,closed,connected,
                  listening],
                  [ list_to_atom(S) || S <-string:tokens(Data,"\n")]),
    io:format(user,"Can keep track of history and return it - OK~n",[]).






