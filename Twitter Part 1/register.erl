-module(register).

-export([registerUser/0,recieveMessage/1,signInUser/0,get_Users_List/0,userSubscriberMap/2,subscribeToUser/1,userProcessIdMap/1
,signOutUser/0]).

registerUser()->
    {ok,[UserName]}=io:fread("Enter Username\n","~ts"),
    {ok,[PassWord]}=io:fread("Enter Password\n","~ts"),
    {ok,[Email]}=io:fread("Enter Email\n","~ts"),
    ServerConnectionId=spawn(list_to_atom("mainserver@Nikhils-MacBook-Air"),twitterMain,signInBuffer,[]),
    ServerConnectionId ! {UserName,PassWord,Email,self(),register},
    receive
        {Registered}->
            io:format("~s~n",[Registered])    
    end.

recieveMessage(UserPasswordMap)->
    receive
        % This function is for Registeration
        {UserName,PassWord,_,Pid,RemoteNodePid}->
            User=maps:find(UserName,UserPasswordMap),
            if
                User==error->
                    NewUserMap=maps:put(UserName,PassWord,UserPasswordMap), 
                    receiveTweet ! {UserName},
                    Pid ! {"Registered",RemoteNodePid},                  
                    recieveMessage(NewUserMap);
                true ->
                    Pid ! {"Issue Occured While Registring",RemoteNodePid},
                    recieveMessage(UserPasswordMap) 
            end;
        {UserName,PasswordAndProcess,Pid,RemoteNodePid}->
            UserPassword=maps:find(UserName,UserPasswordMap),
            [Pass,Process]=PasswordAndProcess,
            ListPassWord={ok,Pass},
            if
                UserPassword==ListPassWord-> 
                   userProcessIdMap!{UserName,Process,"Morning Mate"}, 
                   Pid ! {"Signed In",RemoteNodePid}; 
                true ->
                    Pid ! {"Wrong UserName or Password",RemoteNodePid} 
            end,
            recieveMessage(UserPasswordMap);
        {UserName,Pid}->
            User=maps:find(UserName,UserPasswordMap),
            if
                User==error->
                    Pid ! {"ok"};
                true ->
                    Pid ! {"not ok"}     
            end,
            recieveMessage(UserPasswordMap);
        {Pid,RemoteNodePid,_}->
            UserList=maps:to_list(UserPasswordMap),
            Pid ! {UserList,RemoteNodePid},
            recieveMessage(UserPasswordMap) 
    end.
signInUser()->
    {ok,[UserName]}=io:fread("Enter Username\n","~ts"),
    {ok,[PassWord]}=io:fread("Enter Password\n","~ts"),
    ServerConnectionId=spawn(list_to_atom("mainserver@Nikhils-MacBook-Air"),twitterMain,signInBuffer,[]),
    persistent_term:put("ServerId", ServerConnectionId),
    register(receiveTweetFromUser,spawn(messages,receiveTweetFromUser,[])),

    ServerConnectionId!{UserName,[PassWord,whereis(receiveTweetFromUser)],self()},   
    receive
        {Registered}->
            if
                Registered=="Signed In"->
                    persistent_term:put("UserName",UserName),
                    persistent_term:put("SignedIn",true);
                true->
                    persistent_term:put("SignedIn",false)      
            end,
            io:format("~s~n",[Registered])  
    end.

get_Users_List()->
    SignedIn=persistent_term:get("SignedIn"),
    if
        SignedIn==true-> 
            RemoteServerId=persistent_term:get("ServerId"),
            RemoteServerId!{self()},   
            receive
                {UserList}->
                    % io:format("~p~n",[Registered])  
                    printList(UserList,1)
            end;
        true->
            io:format("You should sign in to send tweets Call twitterMain:registerAndSignin() to complete signin~n")
    end.

userSubscriberMap(UserSubscriberMap,SubscribersUserMap)->
    receive
    {UserName,CurrentUserName,CurrentUserPid,Pid,RemoteNodePid}->
        ListSubscribedTo=maps:find(CurrentUserName,SubscribersUserMap),
        ListSubscribers=maps:find(UserName,UserSubscriberMap),
        if
            ListSubscribers==error->
                NewUserSubscriberMap=maps:put(UserName,[{CurrentUserName,CurrentUserPid}],UserSubscriberMap),
                Pid ! {"Subscribed",RemoteNodePid},
                if
                    ListSubscribedTo==error ->
                        NewSubscriberUserMap=maps:put(CurrentUserName,[{UserName}],SubscribersUserMap), 

                        userSubscriberMap(NewUserSubscriberMap,NewSubscriberUserMap);    
                    true ->
                        {ok,SubscribersTo}=ListSubscribedTo,
                        SubscribersTo1=lists:append(SubscribersTo,[{UserName}]),
                        io:format("~p~n",[SubscribersTo1]),
                        NewSubscriberUserMap=maps:put(CurrentUserName,SubscribersTo1,SubscribersUserMap),
                        userSubscriberMap(NewUserSubscriberMap,NewSubscriberUserMap) 
                end;
            true ->
                {ok,Subscribers}=ListSubscribers,
                Subscribers1=lists:append(Subscribers,[{CurrentUserName,CurrentUserPid}]),
                NewUserSubscriberMap=maps:put(UserName,Subscribers1,UserSubscriberMap),
                Pid ! {"Subscribed",RemoteNodePid},
                if
                    ListSubscribedTo==error ->
                        NewSubscriberUserMap=maps:put(CurrentUserName,[{UserName}],SubscribersUserMap),                       
                        userSubscriberMap(NewUserSubscriberMap,NewSubscriberUserMap);    
                    true ->
                        {ok,SubscribersTo}=ListSubscribedTo,
                        SubscribersTo1=lists:append(SubscribersTo,[{UserName}]),
                        io:format("~p~n",[SubscribersTo1]),
                        NewSubscriberUserMap=maps:put(CurrentUserName,SubscribersTo1,SubscribersUserMap),
                        userSubscriberMap(NewUserSubscriberMap,NewSubscriberUserMap) 
                end 
        end;
    {UserName,Pid}->
        ListSubscribers=maps:find(UserName,UserSubscriberMap),
        if
            ListSubscribers==error->
                Pid !{[]};
            true->
                {ok,Subscribers}=ListSubscribers,
                Pid ! {Subscribers}     
        end,         
        userSubscriberMap(UserSubscriberMap,SubscribersUserMap);
    {UserName,Pid,RemoteNodePid,tweet}->
        ListSubscribersTo=maps:find(UserName,SubscribersUserMap),
        io:format("I am here"),
        if
            ListSubscribersTo==error->
                Pid !{[]};
            true->
                {ok,SubscribersTo}=ListSubscribersTo,
                io:format("~p~n",[SubscribersTo]),
                formAllTheSubscribedTweets(UserSubscriberMap,SubscribersUserMap,
                SubscribersTo,[],1,Pid,RemoteNodePid)
        end,         
        userSubscriberMap(UserSubscriberMap,SubscribersUserMap)        
    end.  
formAllTheSubscribedTweets(UserSubscriberMap,SubscribersUserMap,SubscribersTo,AllTweets,Index,Pid,RemoteNodePid)->
    if
        Index>length(SubscribersTo) ->
            Pid ! {AllTweets,RemoteNodePid}; 
        true ->
            CurrentUserName=lists:nth(Index,SubscribersTo),
            receiveTweet ! {CurrentUserName,self()},
            receive
                {Tweets}->
                    AppendTweet=[{CurrentUserName,Tweets}],
                    io:format("~p~n",[AppendTweet]),
                    AllTweets1=lists:append(AllTweets,AppendTweet),
                    formAllTheSubscribedTweets(UserSubscriberMap,SubscribersUserMap,SubscribersTo,AllTweets1,Index+1,Pid,RemoteNodePid)
            end       
     end.


printList(UserList,Index)->
    if
        Index>length(UserList)->
            ok;
        true->
            {UserName,_}=lists:nth(Index,UserList),
            io:format("~s~n",[UserName]),
            printList(UserList,Index+1)
    end.
subscribeToUser(UserName)->
    SignedIn=persistent_term:get("SignedIn"),
    if
        SignedIn==true-> 
            RemoteServerId=persistent_term:get("ServerId"),
            RemoteServerId!{UserName,persistent_term:get("UserName"),self(),whereis(receiveTweetFromUser)},   
            receive
                {Registered}->
                    io:format("~p~n",[Registered])  
            end;
        true->
            io:format("You should sign in to send tweets Call twitterMain:registerAndSignin() to complete signin~n")
    end.
userProcessIdMap(UserProcessIdMap)->
    receive
    {UserName,CurrentUserPid,_}->
        NewUserProcessIdMap=maps:put(UserName,CurrentUserPid,UserProcessIdMap),  
        io:format("~p~n",[NewUserProcessIdMap]),              
        userProcessIdMap(NewUserProcessIdMap); 
    {UserName,RemoteNodePid,Pid,_}->
        ListSubscribers=maps:find(UserName,UserProcessIdMap),
        if
            ListSubscribers==error->
                Pid ! {"",RemoteNodePid},
                userProcessIdMap(UserProcessIdMap); 
            true ->
                NewUserProcessIdMap=maps:remove(UserName,UserProcessIdMap),  
                Pid ! {"SignedOut",RemoteNodePid},    

                userProcessIdMap(NewUserProcessIdMap)     
        end;  
    {UserName,Tweet}->
        ListSubscribers=maps:find(UserName,UserProcessIdMap),
        if
            ListSubscribers==error->
                ok;
            true->
                {ok,ProcessId}=ListSubscribers,
                ProcessId ! {Tweet,UserName}   
        end,         
        userProcessIdMap(UserProcessIdMap)     
    end.  
signOutUser()->
    SignedIn=persistent_term:get("SignedIn"),
    if
        SignedIn==true-> 
            RemoteServerId=persistent_term:get("ServerId"),
            RemoteServerId!{[persistent_term:get("UserName"),self()],signOut},
            receive
                {Registered}->
                    persistent_term:erase("UserName"),
                    io:format("~s~n",[Registered])  
            end;
        true->
            io:format("You should sign in to send tweets Call twitterMain:registerAndSignin() to complete signin~n")    
    end.        








