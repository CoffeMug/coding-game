-module(refactor_me).
-export([register_user/6,
         get_user_data/3]).

-define(ALPHA_CHARS, "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ").
-define(NON_ALPHA_CHARS, "!@#$%^&*()_-\\|\"\'?/.,<>:;[]{}<>~`").


%% --------------------------------------------------------------------------------
%% Handles User Registration 
%%
%% This is the only module which touches the DataBase and should always be the 
%% ONLY module which does so. 
%% A user tries to register and we check that the WorldRegion is OKAY, and that 
%% the username is valid, and that the password is long enough and that 
%% someone with the same email does not already exist in the database.
%%
%% register_user(DataBase :: [{FirstName, ChosenPassword, EmailAddress, UserName, 
%%                             WorldRegion}],
%%               FirstName :: The first name of the person registering
%%               ChosenPassword :: The chosen password of the person registering
%%               EmailAddress :: The email address of the person registering
%%               UserName :: The username of the person registering
%%               WorldRegion :: The world region, allowed values are
%%                                   1 - WEST EUROPE
%%                                   2 - EAST EUROPE
%%                                   3 - NORTH AMERICA
%%                                   didn't we add 4 for CENTRAL AMERICA? 2009-08-01
%%                                   Yes we did! 2014-03-01
%%              ) -> Result
%% Result :: ok | {error,bad_region} | {error, bad_password} | {error,bad_username} |
%%           these errors added on 2013-07-04!
%%          
%% --------------------------------------------------------------------------------

register_user(_, _, _, _, _, WorldRegion) when WorldRegion < 1; WorldRegion > 6 -> 
    {error,bad_region};

register_user(_, FirstName, _, _, _, _) when length(FirstName) == 0 ->
    {error, bad_name};

register_user(_, _, ChosenPassword, _, _, _) when length(ChosenPassword) < 6; 
                                                  length(ChosenPassword) > 20 ->
    {error, bad_password_length};

register_user(DataBase, FirstName, ChosenPassword, EmailAddress, UserName, WorldRegion) ->
    put(db, DataBase),
    case check_params(FirstName, ChosenPassword, EmailAddress) of
        ok -> case user_name_not_taken_in_region(UserName, EmailAddress) of
                  ok ->
                      {ok,[{FirstName,
                            ChosenPassword,
                            EmailAddress,
                            UserName,
                            WorldRegion}|get(db)]};
                  {error,_} = Err ->
                      Err
              end;
        Err -> Err
    end.       

%% --------------------------------------------------------------------------------
%% Interface get_user_data
%%
%% --------------------------------------------------------------------------------

get_user_data(DataBase,EmailAddress,UserName) ->
    L = [ Entry || Entry <- DataBase,
                   UserName == element(4,Entry) andalso 
		       EmailAddress == element(3,Entry) ],
    case L of
        [Entry] ->
            [Entry];
        _ ->
            []
    end.

%% --------------------------------------------------------------------------------
%% Helper functions
%%
%% --------------------------------------------------------------------------------
check_params(FirstName, ChosenPassword, EmailAddress) ->
    case passwd_valid(ChosenPassword) of
        ok -> case first_name_not_in_passwd(FirstName) of
                  ok -> case db_not_empty(FirstName) of
                            ok -> case email_valid(EmailAddress) of 
                                      ok -> ok;
                                      Err -> Err
                                  end;
                            Err -> Err
                        end;
		  Err -> Err
              end;
        Err -> Err
    end.

user_name_not_taken_in_region(KeyUserName, KeyEmailAddress) ->
    L = [ 1 || {_,
		_,
		EmailAddress,
		UserName,
		_} <- get(db),
	       KeyUserName == UserName andalso 
	       KeyEmailAddress == EmailAddress ],
    case L of
	[] ->
	    ok;
	_ ->
	    {error,duplicate}
    end.

passwd_valid(ChosenPassword) ->
    NonAlphaNums = [ X || X <- ChosenPassword, 
			  lists:member(X,?NON_ALPHA_CHARS)],
    case NonAlphaNums of
        [] ->
            check_pass(ChosenPassword);
        _ ->
            {error, bad_password}
    end.

check_pass(ChosenPassword) ->
    case [ D || D <- [$0,$1,$2,$3,$4,$5,$6,$7,$8,$9], 
		lists:member(D, ChosenPassword) ] of
        [] ->
            {error, bad_password};
        _ ->
            case [ L || L <- ?ALPHA_CHARS, 
                        lists:member(L, ChosenPassword) ] of
                [] ->
                    {error, bad_password};
                _ -> 
                    ok
            end
    end.

email_valid(EmailAddress) ->
    case string:tokens(EmailAddress,"@") of
        [EmailAddress] ->
            {error,bad_email};
        [_] ->
            {error,bad_email};
        [_,SecondHalf] ->
            case string:tokens(SecondHalf,".") of
                [SecondHalf] ->
                    {error,bad_email};
                [_,_|_] -> ok
            end
    end.

first_name_not_in_passwd(FirstName) ->
    case lists:all(fun(X) -> lists:member(X,?ALPHA_CHARS) end, FirstName) of
        true ->
            ok;
        false ->
            {error,bad_name}
    end.

db_not_empty(FirstName) ->
    case lists:any(fun(X) -> lists:member(X,?ALPHA_CHARS) end, FirstName) of
        true ->
            ok;
        false ->
            {error,bad_name}
    end.
