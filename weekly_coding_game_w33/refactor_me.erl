-module(refactor_me).
-export([register_user/6,
         get_user_data/3]).

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

register_user(_, WorldRegion) when WorldRegion < 1, WorldRegion > 6 -> 
    {error,bad_region}.

register_user(DataBase, FirstName, ChosenPassword, EmailAddress, UserName, WorldRegion) -> 
    check_params(WorldRegion, FirstName, ChosenPassword, UserName, EmailAddress),
    put(db,DataBase).

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

check_params(_, FirstName, _, _, _, _) when length(FirstName) == 0 ->
    {error, bad_name};
check_params(_, _, ChosenPassword, _, _, _) when length(ChosenPassword) < 6, 
                                                 length(ChosenPassword) > 20 ->
    {error, bad_password_length};
check_params(_, _, ChosenPassword, _, _, _) ->
    NonAlphaNums = [ X || X <- ChosenPassword, 
                     lists:member(X,"!@#$%^&*()_-\\|\"\'?/.,<>:;[]{}<>~`")],
    case NonAlphaNums of
        [] ->
            check_pass(ChosenPassword);
        _ ->
            {error,bad_password}
    end.
check_pass(ChosenPassword) ->
    case [ D || D <- [$0,$1,$2,$3,$4,$5,$6,$7,$8,$9], 
           lists:member(D, ChosenPassword) ] of
        [] ->
            {error, bad_password};
        _ ->
            case [ L || L <- "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ", 
                        lists:member(L,ChosenPassword) ] of
                [] ->
                    {error,bad_password};
                _ -> 
                    username_not_in_passwd(ChosenPassword)
            end,
    end.

username_not_in_passwd(ChosenPassword) ->
    case all(fun (X) -> 
             lists:member(X,"abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ " end, 
             ChosenPassword) of
        true ->
               %% FIXME 
        false ->
            {error,bad_name}
    end.

register_user4([],WorldRegion, UserName, Email_address) ->
    {error,bad_name};
%% todo: 
%%    ensure that the username also has the same check 2009-03-03
register_user4([C|R], WorldRegion, UserName, Email_address) ->
    case lists:member(C,"abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ") of
        true ->
            ok;
        false ->
            register_user4(R,  WorldRegion, UserName, Email_address)
    end.

check_username_not_in_pass()
                            %% check that the password does not contain usernames
                                    case register_user4(FirstName,WorldRegion, UserName, EmailAddress) of
                                        ok ->
                                            case string:tokens(EmailAddress,"@") of
                                                [EmailAddress] ->
                                                    {error,bad_email};
                                                [_] ->
                                                    {error,bad_email};
                                                [_,SecondHalf] ->
                                                    case string:tokens(SecondHalf,".") of
                                                        [SecondHalf] ->
                                                            {error,bad_email};
                                                        [_,_|_] ->
                                                            Res = register_user(WorldRegion),
                                                            if Res == ok ->
                                                                    %% check that the user and email is not
                                                                    %% already registered in the selected region.
                                                                    %% this would lead to collissions.
                                                                    Res2 = already_taken_in_region(UserName,EmailAddress),
                                                                    case Res2 of
                                                                        ok ->
                                                                            {ok,[{FirstName,
                                                                                  ChosenPassword,
                                                                                  EmailAddress,
                                                                                  UserName,
                                                                                  WorldRegion}|get(db)]};
                                                                        {error,_} = Err ->
                                                                            Err
                                                                    end;
                                                               true ->
                                                                    {error,bad_region}
                                                            end;
                                                        _ ->
                                                            {error,bad_email}
                                                    end
                                            end;
                                        {error,bad_name} ->
                                            {error,bad_name}
                                    end;
                                {error,bad_name} ->
                                    {error,bad_name} 
                            end
                    end;
                {error,bad_password} -> 
                    {error,bad_password}
            end;
    end.

username_not_in_passwd([], _) ->
    ok;
username_not_in_passwd([C|R], _) ->
    case lists:member(C,"abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ ") of
        true ->
            username_not_in_passwd(R, _);
        false ->
            {error,bad_name}
    end.

already_taken_in_region(KeyUserName, KeyEmailAddress) ->
    L = [ 1 || {FirstName,
                ChosenPassword,
                EmailAddress,
                UserName,
                WorldRegion} <- get(db),
               KeyUserName == UserName andalso 
                   KeyEmailAddress == EmailAddress ],
    case L of
        [] ->
            ok;
        _ ->
            {error,duplicate}
    end.
