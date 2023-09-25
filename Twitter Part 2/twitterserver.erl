-module(twitterserver).
-import(maps, []).
-export[start_server/0].

start_server() ->
    io:fwrite("\n\n Hello!!!! This is a Twitter Clone \n\n"),
    %Table = ets:new(t, [ordered_set]),
    Table = ets:new(messages, [ordered_set, named_table, public]),
    Client_Socket_Mapping = ets:new(clients, [ordered_set, named_table, public]),
    All_Clients = [],
    Map = maps:new(),
    {ok, ListenSocket} = gen_tcp:listen(1204, [binary, {keepalive, true}, {reuseaddr, true}, {active, false}]),
    waiting_for_connections(ListenSocket, Table, Client_Socket_Mapping).

waiting_for_connections(Listen, Table, Client_Socket_Mapping) ->
    {ok, Socket} = gen_tcp:accept(Listen),
    ok = gen_tcp:send(Socket, "Success"),
    spawn(fun() -> waiting_for_connections(Listen, Table, Client_Socket_Mapping) end),
    %connection_loop(Socket).
    perform_receive(Socket, Table, [], Client_Socket_Mapping).

perform_receive(Socket, Table, Bs, Client_Socket_Mapping) ->
    io:fwrite("Receiving...\n\n"),
    case gen_tcp:recv(Socket, 0) of
        {ok, Data1} ->
            
            Data = re:split(Data1, ","),
            Type = binary_to_list(lists:nth(1, Data)),

            io:format("\n\ndata: ~p\n\n ", [Data]),
            io:format("\n\ntype: ~p\n\n ", [Type]),

            if 
                Type == "register" ->
                    UserName = binary_to_list(lists:nth(2, Data)),
                    PID = binary_to_list(lists:nth(3, Data)),
                    io:format("\nPID:~p\n", [PID]),
                    io:format("\nSocket:~p\n", [Socket]),
                    io:format("Type: ~p\n", [Type]),
                    io:format("\n~p wants to register a twitter account\n", [UserName]),
                    
                    Output = ets:lookup(Table, UserName),
                    io:format("Output: ~p\n", [Output]),
                    if
                        Output == [] ->

                            ets:insert(Table, {UserName, [{"followers", []}, {"tweets", []}]}),      
                            ets:insert(Client_Socket_Mapping, {UserName, Socket}),                
                            Temp_List = ets:lookup(Table, UserName),
                            io:format("~p", [lists:nth(1, Temp_List)]),

                          
                            ok = gen_tcp:send(Socket, "New User has been registered"), % RESPOND BACK - YES/NO
                            io:fwrite("We can go ahead, as key is not present in the database\n");
                        true ->
                            ok = gen_tcp:send(Socket, "Username is already taken! Please register with a new username"),
                            io:fwrite("Duplicate key!\n")
                    end,
                    perform_receive(Socket, Table, [UserName], Client_Socket_Mapping);

                Type == "tweet" ->
                    UserName = binary_to_list(lists:nth(2, Data)),
                    Tweet = binary_to_list(lists:nth(3, Data)),
                    io:format("\n ~p sent the following tweet: ~p", [UserName, Tweet]),
                    
                    % {ok, Val} = maps:find(UserName, Map),
                    Val = ets:lookup(Table, UserName),
                    io:format("Output: ~p\n", [Val]),
                    Val3 = lists:nth(1, Val),
                    Val2 = element(2, Val3),
                    Val1 = maps:from_list(Val2),
                    {ok, CurrentFollowers} = maps:find("followers",Val1),                         
                    {ok, CurrentTweets} = maps:find("tweets",Val1),

                    NewTweets = CurrentTweets ++ [Tweet],
                    io:format("~p~n",[NewTweets]),
                    
                    ets:insert(Table, {UserName, [{"followers", CurrentFollowers}, {"tweets", NewTweets}]}),

                    Output_After_Tweet = ets:lookup(Table, UserName),
                    io:format("\nThe output after tweeting is: ~p\n", [Output_After_Tweet]),
                  
                    sending_message(Socket, Client_Socket_Mapping, Tweet, CurrentFollowers, UserName),
                    ok = gen_tcp:send(Socket, "Server has processed the tweet\n"),
                    perform_receive(Socket, Table, [UserName], Client_Socket_Mapping);

                Type == "retweet" ->
                    Person_UserName = binary_to_list(lists:nth(2, Data)),
                    UserName = binary_to_list(lists:nth(3, Data)),
                    Sub_User = string:strip(Person_UserName, right, $\n),
                    io:format("Enter the username of the user we want to retweet from : ~p\n", [Sub_User]),
                    Tweet = binary_to_list(lists:nth(4, Data)),
                    Out = ets:lookup(Table, Sub_User),
                    if
                        Out == [] ->
                            io:fwrite("User does not exist in the database!\n");
                        true ->
                            % Current User
                            Out1 = ets:lookup(Table, UserName),
                            Val3 = lists:nth(1, Out1),
                            Val2 = element(2, Val3),
                            Val1 = maps:from_list(Val2),
                            % User we are retweeting from
                            Val_3 = lists:nth(1, Out),
                            Val_2 = element(2, Val_3),
                            Val_1 = maps:from_list(Val_2),
                            % current user
                            {ok, CurrentFollowers} = maps:find("followers",Val1),
                            % user we are retweeting from
                            {ok, CurrentTweets} = maps:find("tweets",Val_1),
                            io:format("Tweet which is to be re-posted: ~p\n", [Tweet]),
                            CheckTweet = lists:member(Tweet, CurrentTweets),
                            if
                                CheckTweet == true ->
                                    NewTweet = string:concat(string:concat(string:concat("re:",Sub_User),"->"),Tweet),
                                    sending_message(Socket, Client_Socket_Mapping, NewTweet, CurrentFollowers, UserName);
                                true ->
                                    io:fwrite("Tweet does not exist in the database!\n")
                            end     
                    end,
                    io:format("\n ~p wants to retweet \n", [UserName]),
                    ok = gen_tcp:send(Socket, "Server processed retweet successfully\n"),
                    perform_receive(Socket, Table, [UserName], Client_Socket_Mapping);

                Type == "subscribe" ->
                    UserName = binary_to_list(lists:nth(2, Data)),
                    SubscribedUserName = binary_to_list(lists:nth(3, Data)),
                    Sub_User = string:strip(SubscribedUserName, right, $\n),
                    Output1 = ets:lookup(Table, Sub_User),
                    if
                        Output1 == [] ->
                            io:fwrite("The username entered doesn't exist in the database. so, please try again!!! \n");
                        true ->

                            Val = ets:lookup(Table, Sub_User),
                            Val3 = lists:nth(1, Val),
                            Val2 = element(2, Val3),

                            Val1 = maps:from_list(Val2),                            
                            {ok, CurrentFollowers} = maps:find("followers",Val1),
                            {ok, CurrentTweets} = maps:find("tweets",Val1),

                            NewFollowers = CurrentFollowers ++ [UserName],
                            io:format("~p~n",[NewFollowers]),
                        
                            ets:insert(Table, {Sub_User, [{"followers", NewFollowers}, {"tweets", CurrentTweets}]}),

                            Output2 = ets:lookup(Table, Sub_User),
                            io:format("\nThe output after subscribing: ~p\n", [Output2]),

                            ok = gen_tcp:send(Socket, "Subscribed!"),

                            perform_receive(Socket, Table, [UserName], Client_Socket_Mapping)
                    end,
                    io:format("\n ~p wants to subscribe to ~p\n", [UserName, Sub_User]),
                    
                    ok = gen_tcp:send(Socket, "Server processed subscription. Subscribed!"),
                    perform_receive(Socket, Table, [UserName], Client_Socket_Mapping);

                Type == "query" ->
                    Option = binary_to_list(lists:nth(3, Data)),
                    UserName = binary_to_list(lists:nth(2, Data)),
                    io:format("Query: Current user is -> ~p\n", [UserName]),
                    % Query = binary_to_list(lists:nth(3, Data)),
                    if
                        Option == "1" ->
                            io:fwrite("Performing Search for My mentions\n"),
                            MyUserName = binary_to_list(lists:nth(4, Data)),
                            Sub_UserName = ets:first(Table),
                            Sub_User = string:strip(Sub_UserName, right, $\n),
                            io:format("Sub_UserName: ~p\n", [Sub_User]),
                            Tweets = searching_all_tweets("@", Table, Sub_User, MyUserName , []),
                            ok = gen_tcp:send(Socket, Tweets);
                        Option == "2" ->
                            io:fwrite("Performing Hashtag Search\n"),
                            Hashtag = binary_to_list(lists:nth(4, Data)),
                            Sub_UserName = ets:first(Table),
                            Sub_User = string:strip(Sub_UserName, right, $\n),
                            io:format("Sub_UserName: ~p\n", [Sub_User]),
                            Tweets = searching_all_tweets("#", Table, Sub_User, Hashtag , []),
                            ok = gen_tcp:send(Socket, Tweets);
                        true ->
                            io:fwrite("Performing Subscribed User Search\n"),
                            % Sub_UserName = binary_to_list(lists:nth(4, Data)),
                            Sub_UserName = ets:first(Table),
                            Sub_User = string:strip(Sub_UserName, right, $\n),
                            io:format("Subcribed_UserName: ~p\n", [Sub_User]),
                            Val = ets:lookup(Table, Sub_User),
                            % io:format("~p~n",[Val]),
                            Val3 = lists:nth(1, Val),
                            Val2 = element(2, Val3),
                            Val1 = maps:from_list(Val2),                            
                            {ok, CurrentTweets} = maps:find("tweets",Val1),
                            io:format("\n ~p : ", [Sub_User]),
                            io:format("~p~n",[CurrentTweets]),
                            searching_whole_table(Table, Sub_User, UserName),
                            ok = gen_tcp:send(Socket, CurrentTweets)
                    end,
                    io:format("\n ~p wants to perform query", [UserName]),
                    perform_receive(Socket, Table, [UserName], Client_Socket_Mapping);
                true ->
                    io:fwrite("\n Choose the correct command!!!")
            end;

        {error, closed} ->
            {ok, list_to_binary(Bs)};
        {error, Reason} ->
            io:fwrite("Error!!!!"),
            io:fwrite(Reason)
    end.

searching_all_tweets(Symbol, Table, Key, Word, Found) ->
    Search = string:concat(Symbol, Word),
    io:format("The word which is searched for: ~p~n", [Search]),
    if
        Key == '$end_of_table' ->
            io:fwrite("The tweets which we found are: ~p~n", [Found]),
            Found;
        true ->
            io:fwrite("The current key: ~p~n", [Key]),
            Val = ets:lookup(Table, Key),
            Val3 = lists:nth(1, Val),
            Val2 = element(2, Val3),
            Val1 = maps:from_list(Val2),                              
            {ok, CurrentTweets} = maps:find("tweets",Val1),
            io:fwrite("Current Tweets: ~p~n", [CurrentTweets]),
            FilteredTweets = [S || S <- CurrentTweets, string:str(S, Search) > 0],
            io:fwrite("Filtered Tweets: ~p~n", [FilteredTweets]),
            Found1 = Found ++ FilteredTweets,
            CurrentRow_Key = ets:next(Table, Key),
            searching_all_tweets(Symbol, Table, CurrentRow_Key, Word, Found1)
    end.


searching_whole_table(Table, Key, UserName) ->
    CurrentRow_Key = ets:next(Table, Key),
    Val = ets:lookup(Table, CurrentRow_Key),
    Val3 = lists:nth(1, Val),
    Val2 = element(2, Val3),
    Val1 = maps:from_list(Val2),                            
    {ok, CurrentFollowers} = maps:find("followers",Val1),
    IsMember = lists:member(UserName, CurrentFollowers),
    if
        IsMember == true ->
            {ok, CurrentTweets} = maps:find("tweets",Val1),
            io:format("\n ~p : ", [CurrentRow_Key]),
            io:format("~p~n",[CurrentTweets]),
            searching_whole_table(Table, CurrentRow_Key, UserName);
        true ->
            io:fwrite("\n No more tweets are available!\n")
    end,
    io:fwrite("\n Searching the whole table in the database!\n").

sending_message(Socket, Client_Socket_Mapping, Tweet, Subscribers, UserName) ->
    if
        Subscribers == [] ->
            io:fwrite("\nNo followers!!\n");
        true ->
           
            [Client_To_Send | Remaining_List ] = Subscribers,
            io:format("Client to send: ~p\n", [Client_To_Send]),
            io:format("\nRemaining List: ~p~n",[Remaining_List]),
            Client_Socket_Row = ets:lookup(Client_Socket_Mapping,Client_To_Send),
            Val3 = lists:nth(1, Client_Socket_Row),
            Client_Socket = element(2, Val3),
            io:format("\nClient Socket: ~p~n",[Client_Socket]),
            
            ok = gen_tcp:send(Client_Socket, ["New tweet received!\n",UserName,":",Tweet]),
            ok = gen_tcp:send(Socket, "Your tweet has been sent\n"),
            
            sending_message(Socket, Client_Socket_Mapping, Tweet, Remaining_List, UserName)
    end,
    io:fwrite("send the message!\n").


printing_the_user_map(Map) ->
    io:fwrite("**************\n"),
    List1 = maps:to_list(Map),
    io:format("~s~n",[tuplelist_to_string(List1)]),
    io:fwrite("**************\n").

tuplelist_to_string(L) ->
    tuplelist_to_string(L,[]).

tuplelist_to_string([],Acc) ->
    lists:flatten(["[",
           string:join(lists:reverse(Acc),","),
           "]"]);
tuplelist_to_string([{X,Y}|Rest],Acc) ->
    S = ["{\"x\":\"",X,"\", \"y\":\"",Y,"\"}"],
    tuplelist_to_string(Rest,[S|Acc]).

connection_loop(Socket) ->
    io:fwrite("Yeahhh!!! Someone is trying to connect to me!\n\n"),
    receive
        {tcp, Socket, Data} ->
            io:fwrite("...."),
            io:fwrite("\n ~p \n", [Data]),
            if 
                Data == <<"register_account">> ->
                    io:fwrite("Client wants to register an account"),
                    ok = gen_tcp:send(Socket, "username"), % RESPOND BACK - YES/NO
                    io:fwrite("is now registered");
                true -> 
                    io:fwrite("true..")
            end,
            connection_loop(Socket);
            
        {tcp_closed, Socket} ->
            io:fwrite("Sorry!!! I am not available!"),
            closed
    end.

