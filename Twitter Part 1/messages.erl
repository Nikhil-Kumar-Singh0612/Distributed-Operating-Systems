-module(messages).

-export([sendingTweetToServer/1,getTweetFromUser/1,hashTagTweetMap/1,parseTheTweet/5,receiveTweetFromUser/0,
sendingTweetToAllSubscribers/4,myMentionsList/0,query_Hash_Tag/1,printTweets/2,get_Subscribed_Tweets/0]).

sendingTweetToServer(Tweet)->
    try persistent_term:get("SignedIn")
    catch 
    error:X ->
        io:format("~p~n",[X])
    end,  
    SignedIn=persistent_term:get("SignedIn"),
    if
        SignedIn==true-> 
            RemoteServerId=persistent_term:get("ServerId"),
            RemoteServerId!{persistent_term:get("UserName"),Tweet,self(),tweet},
            receive
                {Registered}->
                    io:format("~s~n",[Registered])  
            end;
        true->
            io:format("You should sign in to send tweets Call twitterMain:registerAndSignin() to complete signin~n")
    end. 


getTweetFromUser(UserTweetMap)->
    receive
        {UserName,Tweet,Pid,RemoteNodePid}->
            ListTweets=maps:find(UserName,UserTweetMap),
            if
                ListTweets==error->
                    Pid ! {"User Not present in Server Database",RemoteNodePid},
                    getTweetFromUser(UserTweetMap); 
                true ->
                    {ok,Tweets}=ListTweets,
                    io:format("~s~n",[Tweet]),
                    io:format("~p~n",[Tweets]),
                    Tweets1=lists:append(Tweets,[Tweet]),
                    io:format("~p~n",[Tweets1]),
                    NewUserTweetMap=maps:put(UserName,Tweets1,UserTweetMap), 
                    Pid ! {"Tweet Posted",RemoteNodePid},  
                    TweetSplitList=string:split(Tweet," ",all),
                    io:format("~p~n",[TweetSplitList]),
                    parseTheTweet(TweetSplitList,1,Tweet,UserName,"#"),
                    parseTheTweet(TweetSplitList,1,Tweet,UserName,"@"),
                    subscribeToUser ! {UserName,self()},
                    receive
                        {Subscribers}->
                          io:format("Subscribers are ~p~n",[Subscribers]),
                          spawn(messages,sendingTweetToAllSubscribers,[Subscribers,1,Tweet,UserName])
                    end,                  
                    getTweetFromUser(NewUserTweetMap)  
            end;
         {UserName}->
            NewUserTweetMap=maps:put(UserName,[],UserTweetMap),
            getTweetFromUser(NewUserTweetMap);
         {UserName1,Pid}->
            {UserName}=UserName1,
            ListTweets=maps:find(UserName,UserTweetMap),
            io:format("UserName=~p~n",[UserTweetMap]),
            if
                ListTweets==error->
                    Pid ! {[]};
                true ->
                    {ok,Tweets}=ListTweets,
                    Pid ! {Tweets}
            end,
            getTweetFromUser(UserTweetMap); 
         {UserName,Pid,RemoteNodePid}->
            ListTweets=maps:find(UserName,UserTweetMap),
            if
                ListTweets==error->
                    Pid ! {[],RemoteNodePid};
                true ->
                    {ok,Tweets}=ListTweets,
                    io:format("length= ~p~n",[length(Tweets)]),
                    Pid ! {Tweets,RemoteNodePid}
            end,
            getTweetFromUser(UserTweetMap)

    end. 


hashTagTweetMap(HashTagTweetMap)->
   receive
    {HashTag,Tweet,UserName,addnewhashTag}->
        io:format("~s~n",[Tweet]),
        ListTweets=maps:find(HashTag,HashTagTweetMap),
        if
            ListTweets==error->
                NewHashTagTweetMap=maps:put(HashTag,[{Tweet,UserName}],HashTagTweetMap),
                hashTagTweetMap(NewHashTagTweetMap); 
            true ->
                {ok,Tweets}=ListTweets,
                io:format("~p~n",[Tweets]),
                Tweets1=lists:append(Tweets,[{Tweet,UserName}]),
                io:format("~p~n",[Tweets1]),
                NewHashTagTweetMap=maps:put(HashTag,Tweets1,HashTagTweetMap),
                % io:format("~p",NewUserTweetMap),                
                hashTagTweetMap(NewHashTagTweetMap)  
        end;
     {HashTag,Pid,RemoteNodePid}->
        ListTweets=maps:find(HashTag,HashTagTweetMap),
        if
            ListTweets==error->
                Pid ! {[],RemoteNodePid};
            true ->
                {ok,Tweets}=ListTweets,
                Pid ! {Tweets,RemoteNodePid}
        end,
        hashTagTweetMap(HashTagTweetMap)
    end. 
parseTheTweet(SplitTweet,Index,Tweet,UserName,Tag)->
    if
        Index==length(SplitTweet)+1 ->
         ok;
        true ->
            CurrentString=string:find(lists:nth(Index,SplitTweet),Tag,trailing),
            io:format("~s~n",[CurrentString]),
            if
                CurrentString==nomatch ->
                  ok;  
                true ->
                    if
                        Tag=="@" ->
                            Username=string:sub_string(CurrentString,2,length(CurrentString)),
                            userProcessIdMap!{Username,Tweet};
                        true ->
                            ok
                    end,
                    hashTagMap ! {CurrentString,Tweet,UserName,addnewhashTag}  
            end,
            parseTheTweet(SplitTweet,Index+1,Tweet,UserName,Tag)
    end.

sendingTweetToAllSubscribers(Subscribers,Index,Tweet,UserName)->
 if
    Index>length(Subscribers)->
            ok;
    true->
        {Username1,_}=lists:nth(Index,Subscribers),
        % io:format("~p~n",[Pid]),
        userProcessIdMap!{Username1,Tweet},
        sendingTweetToAllSubscribers(Subscribers,Index+1,Tweet,UserName)
 end.       

receiveTweetFromUser()->
    receive
     {Message,UserName}->
        CurrentMessage=UserName++" : "++Message,
        io:format("~s~n",[CurrentMessage]),
        receiveTweetFromUser()
    end.
myMentionsList()->
    RemoteServerId=persistent_term:get("ServerId"),
    UserId="@"++persistent_term:get("UserName"),
    RemoteServerId!{querying,UserId,self(),tweet},
    receive
        {Tweets}->
            printTweets(Tweets,1) 
    end.
query_Hash_Tag(Tag)->
    RemoteServerId=persistent_term:get("ServerId"),
    RemoteServerId!{querying,Tag,self(),tweet},
    receive
        {Tweets}->
            printTweets(Tweets,1)  
    end.
printTweets(Tweets,Index)->
    if
        Index>length(Tweets) ->
            ok;
        true ->
            {Tweet,UserName}=lists:nth(Index,Tweets),
            io:format("~p : ~p ~n",[list_to_atom(UserName),list_to_atom(Tweet)]),
            printTweets(Tweets,Index+1)
    end.    
get_Subscribed_Tweets()->
    RemoteServerId=persistent_term:get("ServerId"),
    RemoteServerId!{queryingSubscribedTweets,persistent_term:get("UserName"),self(),tweet},
    receive
        {Tweets}->
            io:format("~p~n",[Tweets]) 
    end.




       



    



 