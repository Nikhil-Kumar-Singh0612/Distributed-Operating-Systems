-module(twitterclient).
-export[start_client/0, get_and_parse_user_input/2, perform_loop/1].

start_client() ->
    io:fwrite("\n\n This is a new client \n\n"),
    PortNumber = 1204,
    IPAddress = "localhost",
    {ok, Sock} = gen_tcp:connect(IPAddress, PortNumber, [binary, {packet, 0}]),
    io:fwrite("\n\n Please send my request to the server\n\n"),
    spawn(client, get_and_parse_user_input, [Sock, "_"]),
    perform_loop(Sock).

perform_loop(Sock) ->
    receive
        {tcp, Sock, Data} ->
            io:fwrite("The message which is received from server\n"),
            io:fwrite(Data),
            perform_loop(Sock);
        {tcp, closed, Sock} ->  
            io:fwrite("Client cannot connect to the server : TCP Connection Closed")
        end.

get_and_parse_user_input(Sock, UserName) ->
    {ok, [CommandType]} = io:fread("\nEnter the command which we need to perform: ", "~s\n"),
    io:fwrite(CommandType),
    if 
        CommandType == "register" ->
            UserName1 = register_account(Sock);
        CommandType == "tweet" ->
            if
                UserName == "_" ->
                    io:fwrite("Please register first!\n"),
                    UserName1 = get_and_parse_user_input(Sock, UserName);
                true ->
                    send_tweet(Sock,UserName),
                    UserName1 = UserName
            end;
        CommandType == "retweet" ->
            if
                UserName == "_" ->
                    io:fwrite("Please register first!\n"),
                    UserName1 = get_and_parse_user_input(Sock, UserName);
                true ->
                    re_tweet(Sock, UserName),
                    UserName1 = UserName
            end;
        CommandType == "subscribe" ->
            if
                UserName == "_" ->
                    io:fwrite("Please register first!\n"),
                    UserName1 = get_and_parse_user_input(Sock, UserName);
                true ->
                    subscribe_to_user(Sock, UserName),
                    UserName1 = UserName
            end;
        CommandType == "query" ->
            if
                UserName == "_" ->
                    io:fwrite("Please register first!\n"),
                    UserName1 = get_and_parse_user_input(Sock, UserName);
                true ->
                    query_tweet(Sock, UserName),
                    UserName1 = UserName
            end;
        CommandType == "logout" ->
            if
                UserName == "_" ->
                    io:fwrite("Please register first!\n"),
                    UserName1 = get_and_parse_user_input(Sock, UserName);
                true ->
                    UserName1 = "_"
            end;
        CommandType == "login" ->
            UserName1 = signin_account();
        true ->
            io:fwrite("Invalid command!, Please Enter another command!\n"),
            UserName1 = get_and_parse_user_input(Sock, UserName)
    end,
    get_and_parse_user_input(Sock, UserName1).


register_account(Sock) ->
    % Input user-name
    {ok, [UserName]} = io:fread("\nEnter the User Name: ", "~s\n"),
    % send the server request
    io:format("SELF: ~p\n", [self()]),
    ok = gen_tcp:send(Sock, [["register", ",", UserName, ",", pid_to_list(self())]]),
    io:fwrite("\nAccount has been Registered\n"),
    UserName.

signin_account() ->
    % Input user-name
    {ok, [UserName]} = io:fread("\nEnter the User Name: ", "~s\n"),
    io:format("SELF: ~p\n", [self()]),
    io:fwrite("\nAccount has been Signed in\n"),
    UserName.

send_tweet(Sock,UserName) ->
    Tweet = io:get_line("\nEnter the tweet you want to share:"),
    ok = gen_tcp:send(Sock, ["tweet", "," ,UserName, ",", Tweet]),
    io:fwrite("\nTweet is successfully Sent\n").

re_tweet(Socket, UserName) ->
    {ok, [Person_UserName]} = io:fread("\nEnter the User Name for re-tweet: ", "~s\n"),
    Tweet = io:get_line("\nEnter the tweet that you want to repost: "),
    ok = gen_tcp:send(Socket, ["retweet", "," ,Person_UserName, ",", UserName,",",Tweet]),
    io:fwrite("\nRetweeted successfully\n").

subscribe_to_user(Sock, UserName) ->
    SubscribeUserName = io:get_line("\nEnter the user whom you want to subscribe to?:"),
    ok = gen_tcp:send(Sock, ["subscribe", "," ,UserName, ",", SubscribeUserName]),
    io:fwrite("\nSubscribed successfully!\n").

query_tweet(Sock, UserName) ->
    io:fwrite("\n Querying Options Are:\n"),
    io:fwrite("\n 1. My Mentions (@)\n"),
    io:fwrite("\n 2. Hashtag Search \n"),
    io:fwrite("\n 3. Subscribed Users Tweets\n"),
    {ok, [Option]} = io:fread("\nSpecify the query number you want to perform: ", "~s\n"),
    if
        Option == "1" ->
            ok = gen_tcp:send(Sock, ["query", "," ,UserName, ",", "1", ",", UserName]);
        Option == "2" ->
            {ok, [Hashtag]} = io:fread("\nSEnter the hahstag which you want to search: ", "~s\n"),
            % Hashtag = io:get_line("\nEnter the hahstag you want to search: "),
            ok = gen_tcp:send(Sock, ["query", "," ,UserName, ",","2",",", Hashtag]);
        true ->
            {ok, [Sub_UserName]} = io:fread("\nEnter the username whose tweets you want? ", "~s\n"),
            % Sub_UserName = io:get_line("\nWhose tweets do you want? "),
            ok = gen_tcp:send(Sock, ["query", "," ,UserName, ",", "3",",",Sub_UserName])
    end.

% subscribe <user_name>
% INPUT