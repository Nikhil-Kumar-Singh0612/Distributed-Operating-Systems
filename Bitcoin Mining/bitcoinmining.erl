-module(bitcoinmining).
-import(string,[to_lower/1]).
-import(string,[sub_string/3]).
-export([initiate_server/0, server/1, extractCoin/1, worker/1, create_worker/2, spawning_the_slaves/2,calculate_hash/2]).


get_random_string(Length, AllowedChars) ->
    lists:foldl(fun(_, Acc) ->
                        [lists:nth(rand:uniform(length(AllowedChars)),
                                   AllowedChars)]
                            ++ Acc
                end, [], lists:seq(1, Length)).

addingZeroesToPrefix(0) -> "";
addingZeroesToPrefix(N) -> 
    "0"++addingZeroesToPrefix(N-1).

extractCoin(K) ->
    receive
        {mine, From, SNode} ->
            UpdatedId = "nikhilkumarsingh"++get_random_string(20,"ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789;!@#$%^&*()+-_="),
            Hash = to_lower(integer_to_list(binary:decode_unsigned(crypto:hash(sha256,UpdatedId)),16)),
            HashLength = string:len(Hash),
            if 
                HashLength =< (64-K) ->
                    io:format("Mined a Coin ~n"),
                    client ! got,
                    {From, SNode} ! {got_coin,{UpdatedId, addingZeroesToPrefix(K)++Hash}};
                true ->
                    spawn(bitcoinmining, extractCoin,[K]) ! {mine, From, SNode}
            end
    end.

server(K)->
    receive
        hello ->
            io:format("someonesays Hello~n");
        {i_am_worker, WorkerPid} ->
            io:format("Server Received a Worker~n"),
            io:format("Worker Node ~p ~n",[WorkerPid]),
            WorkerPid ! hello;
        {got_coin, {Coin, Hash}} ->
            io:format("Coin : Hash Value ---> ~p  :  ~p~n",[Coin,Hash]);
        {mine, WPid} ->
            WPid ! {zcount, K};
        {time,CPU,REAL, RATIO} ->
            io:format("CPU TIME : ~p~n REAL TIME : ~p~n PARALLELISM  RATIO :~p~n",[CPU,REAL, RATIO]);
        terminate ->
            exit("Exited")
    end,
    server(K).

worker(SNode) ->
    
    {serverPid, SNode} ! {mine, self()},
    receive
        {zcount, K} ->
            spawn(bitcoinmining, extractCoin,[K]) ! {mine, serverPid, SNode}
    end.

spawning_the_slaves(1, SNode) ->
    spawn(bitcoinmining, worker, [SNode]);
    
spawning_the_slaves(N, SNode) ->
    spawn(bitcoinmining, worker, [SNode]),
    spawning_the_slaves(N-1, SNode).

calculate_hash(S,C) ->
    register(client,spawn(bitcoinmining,create_worker,[S,C])).

create_worker(SNode,C) ->
    {_,_}=statistics(runtime),
    {_,_}=statistics(wall_clock),
    io:format("Creating Worker Nodes~n"),
    %{ok, C} = io:read("Enter a number: "),
    spawning_the_slaves(C, SNode),
    listen(1,SNode).

listen(N,SNode) ->
    receive 
        got ->
            io:format("Bitcoin : ~p" , [N]), 
            if 
                N == 5 ->
                    {_,CPU}=statistics(runtime),
                    {_,REAL}=statistics(wall_clock),
                    {serverPid,SNode} ! {time,CPU, REAL, CPU/REAL};
                true -> 
                    listen(N+1,SNode)
            end
    end.

initiate_server() ->
    {ok, K} = io:read("Input number of leading zeroes: "),
    io:format("Entered No.of leading zeroes : ~p~n",[K]),
    register(serverPid,spawn(bitcoinmining, server,[K])),
    {_,_}=statistics(runtime),
    {_,_}=statistics(wall_clock).


