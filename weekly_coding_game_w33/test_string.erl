-module(test_string).
-export([username_not_in_passwd/1]).

-define(ALPHA, "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ").

username_not_in_passwd(ChosenPassword) ->
    case lists:all(fun (X) -> lists:member(X,?ALPHA) end, ChosenPassword ) 
    of
         true -> ok;
         false ->
                 {error,bad_name}
    end.

