-module(main).
-export([start/0,start_server/3,tracker_server/3]). 
-import(gossip,[gossipWorker/2]).
-import(pushsum,[push_sum_worker/4]).
-import(topology,[grid_connected/3, line_connected/3, fully_connected/2, imp2D_connected/3]).
-import(lists,[nth/2]).
-import(math,[sqrt/1, pow/2]).

% starting the server
start_server(N,Topp,Algorithm)->
    receive
        hello ->
            A = birth_workers(N, Algorithm),
            io:format("Worker pids is ~p~n",[A]),
            case Topp of
                "full" -> fully_connected([],A); 
                "line" -> line_connected(N,N,A);
                "2D" -> grid_connected(N, round(sqrt(N)), A);
                "imp3D" -> imp2D_connected(N, round(sqrt(N)), A)
            end,
            [FirstPid|Tail] = A,
            register(fpid,FirstPid);
        startrumor ->
          statistics(wall_clock),
          case Algorithm of
            "gossip" -> fpid ! {rumor, "Rumor"};
            "push-sum" -> fpid ! {rumor, 0, 1}
          end;
        done ->
            {_, Time2} = statistics(wall_clock),
            io:format("Time of convergence is ~p~n",[Time2]),
            exit(self(),normal)
    end,
        start_server(N,Topp,Algorithm).


% start Tracker Server 
tracker_server(TotalNodes,HeardNodes,ReadyNodes) ->
    receive
        heard -> 
            if
                HeardNodes+1==TotalNodes ->
                    io:format("Gossip Algorithm is Converged~n"),
                    spid ! done;
                true ->
                    tracker_server(TotalNodes,HeardNodes+1,ReadyNodes)
            end;
        {heard, Ratio} ->
            io:format("Pushsum Algorithm is Converged and the ratio is ~p~n",[Ratio]),
            spid ! done;
        iamready ->
            if
                ReadyNodes+1==TotalNodes ->
                     spid ! startrumor;
                true ->
                    tracker_server(TotalNodes,HeardNodes,ReadyNodes+1)
            end
    end,
    tracker_server(TotalNodes,HeardNodes,ReadyNodes).


birth_workers(0, Algorithm) -> [];
birth_workers(N, Algorithm) ->
  Pid = case Algorithm of
    "gossip" -> spawn(gossip, gossipWorker, [0,[]]);
    "push-sum" -> spawn(pushsum, push_sum_worker,[0, N, 1,[]])
  end,
  [Pid|birth_workers(N-1, Algorithm)].


start()->
    {ok, [N]} = io:fread("\nEnter the number of nodes: ", "~d\n"),
    {ok, [Topp]} = io:fread("\nEnter Topology: ", "~s\n"),
    {ok, [Algorithm]} = io:fread("\nEnter the Algorithm (Gossip or PushSum): ", "~s\n"),
    io:format("No of workers: ~p~n",[N]),
    FinalNodeCount = case Topp of
                     "full" -> N;
                     "line" -> N;
                     "2D" -> round(pow(ceil(sqrt(N)),2));
                     "imp3D" -> round(pow(ceil(sqrt(N)),2))
                   end,
    io:format("Final No of workers after rounding: ~p~n",[FinalNodeCount]),
    register(tpid,spawn(main,tracker_server,[FinalNodeCount,0,0])),
    register(spid,spawn(main,start_server,[FinalNodeCount,Topp,Algorithm])),
    spid ! hello.
