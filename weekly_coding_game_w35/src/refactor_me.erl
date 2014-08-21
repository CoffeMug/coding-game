-module(refactor_me).
-export([start/1]).

start(Config) ->
    Parent = proplists:get_value(parent,Config),
    MaxFailures = proplists:get_value(max_fails,Config),
    Timeout = proplists:get_value(max_timeout,Config),
    Loop = fun(F,undefined,0) ->		   
		   {ok,LSock} = gen_tcp:listen(0,[{active,false}]),
		   inform_and_remember(Parent,listening),
		   Parent ! {port,element(2,inet:port(LSock))},
		   F(F,LSock,0);
	      (F,LSock,Failures) when Failures >= MaxFailures ->
		   gen_tcp:close(LSock),
		   inform_and_remember(Parent,reopening),
		   F(F,undefined,0);
	      (F,LSock,Failures) ->
		   {ok, ASock} = gen_tcp:accept(LSock),
		   inform_and_remember(Parent,connected),
		   NewFailures = case gen_tcp:recv(ASock, 0, Timeout) of
				     {ok,"history"} ->
					 remember(history),
					 ok = gen_tcp:send(ASock,history_as_string()),
					 Failures;
				     {error,closed} ->
					 inform_and_remember(Parent,closed),
					 Failures;
				     {error,timeout} ->
					 inform_and_remember(Parent,timeout_on_receive),
					 Failures + 1
				 end,
		   gen_tcp:close(ASock),
		   F(F,LSock,NewFailures)
	   end,
    spawn(fun() -> Loop(Loop, undefined, 0) end),
    ok.

history() ->
    case get(history) of
	undefined -> [];
	H -> H
    end.

inform_and_remember(Parent,Event) ->
    Parent ! {progress,Event},
    remember(Event).

remember(Event) ->
    put(history,[Event|history()]).
    
history_as_string() ->
    string:join([ atom_to_list(X) || X <- history() ],"\n").
