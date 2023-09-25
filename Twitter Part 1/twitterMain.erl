-module(twitterMain).

-export([registerAndSignin/0,startingTheTwitter/0,signInBuffer/0,sendingTweet/0,get_Users_List/0,subscribeTheTweet/0,signingIn/0,
myMentionsList/0,query_Hash_Tag/0,get_Subscribed_Tweets/0]).

registerAndSignin()->
    io:format("~s~n",["Welcome to Twitter"]),
    {ok,[SignIn]}=io:fread("To Sign in type S and for Registration type R \n","~ts"),
    if
        (SignIn=="S")->
            register:signInUser();
        true->
            register:registerUser()
    end.
signInBuffer()->
    receive
        % for SignIn
        {UserName,PasswordAndProcess,Pid}->
            userregister ! {UserName,PasswordAndProcess,self(),Pid};
        % for Registeration    
        {UserName,PassWord,Email,Pid,register}->
            userregister ! {UserName,PassWord,Email,self(),Pid};
        % For receiving user's tweets and quering them        
        {UserName,Tweet,Pid,tweet}->
            if
                UserName==querying ->
                    hashTagMap!{Tweet,self(),Pid}; 
                UserName==queryingSubscribedTweets->
                    % Tweet is UserName
                    subscribeToUser!{Tweet,self(),Pid,tweet}; 
                true ->
                 receiveTweet !{UserName,Tweet,self(),Pid} 
            end;
        {UserName,Pid}->
            if 
                Pid==signOut->
                    [UserName1,RemoteNodePid]=UserName,
                    userProcessIdMap!{UserName1,RemoteNodePid,self(),randomShitAgain};
                true->
                 receiveTweet !{UserName,self(),Pid}
            end;     
        {Pid}->
            userregister ! {self(),Pid,"Good Morning Everyone\n"};    
        {UserName,CurrrentUserName,Pid,PidOfReceive}->
            subscribeToUser ! {UserName,CurrrentUserName,PidOfReceive,self(),Pid}
    end,
    receive
        {Message,Pid1}->
            Pid1 ! {Message},
            signInBuffer()        
    end.    
startingTheTwitter()->
    List1 = [{"a","sample"}],
    List2=[{"Nikhil",["hi"]}],
    List3=[{"#Messi","The Greatest footballer of all time is #Messi"}],
    List4=[{"a",[]}],
    List5=[{"Il","Random"}],
    Map1 = maps:from_list(List1),
    Map2 = maps:from_list(List2),
    Map3= maps:from_list(List3),
    Map4=maps:from_list(List4),
    Map6=maps:from_list(List4),
    Map5=maps:from_list(List5),
    register(userregister,spawn(list_to_atom("mainserver@Nikhils-MacBook-Air"),register,recieveMessage,[Map1])),
    register(receiveTweet,spawn(list_to_atom("mainserver@Nikhils-MacBook-Air"),messages,getTweetFromUser,[Map2])),
    register(hashTagMap,spawn(list_to_atom("mainserver@Nikhils-MacBook-Air"),messages,hashTagTweetMap,[Map3])),
    register(subscribeToUser,spawn(list_to_atom("mainserver@Nikhils-MacBook-Air"),register,userSubscriberMap,[Map4,Map6])),
    register(userProcessIdMap,spawn(list_to_atom("mainserver@Nikhils-MacBook-Air"),register,userProcessIdMap,[Map5])).
sendingTweet()->
    Tweet1=io:get_line("Enter the Tweet \n"),
    Tweet=lists:nth(1,string:tokens(Tweet1,"\n")),
    try messages:sendingTweetToServer(Tweet)
    catch 
    error:_ -> 
      io:format("User Not Signed in~n") 
    end.   
get_Users_List()->
    spawn(register,get_Users_List,[]).  
subscribeTheTweet()->
    UserName1=io:get_line("Enter the username u want to subscribe to\n"),
    UserName=lists:nth(1,string:tokens(UserName1,"\n")),
    register:subscribeToUser(UserName).
signingIn()->
    register:signOutUser().
myMentionsList()->
    messages:myMentionsList().
query_Hash_Tag()->
    HashTag=io:get_line("Enter HashTag(#) for querying\n"),
    HashTag1=lists:nth(1,string:tokens(HashTag,"\n")),
    messages:query_Hash_Tag(HashTag1).
get_Subscribed_Tweets()->
    messages:get_Subscribed_Tweets().   




