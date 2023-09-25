-module(chordprotocol).
-export([running_the_server/2, starting_the_network/2, task_is_completed/2, wait_node/1, node/4]).


get_number_of_nodes(NumNodes) ->
    trunc(math:ceil(math:log2(NumNodes)))
.


random_Node(Node_id, []) -> Node_id;
random_Node(_, ExistingNodes) -> lists:nth(rand:uniform(length(ExistingNodes)), ExistingNodes).


node_chord(ChordNodes, TotalNodes, M, NetworkState) ->
    RemainingHashes = lists:seq(0, TotalNodes - 1, 1) -- ChordNodes,
    Hash = lists:nth(rand:uniform(length(RemainingHashes)), RemainingHashes),
    Pid = spawn(chordprotocol, node, [Hash, M, ChordNodes, dict:new()]),
    % io:format("~n ~p ~p ~n", [Hash, Pid]),
    [Hash, dict:store(Hash, Pid, NetworkState)]
.


get_forward_distance(Key, Key, _, Distance) ->
    Distance;
get_forward_distance(Key, NodeId, M, Distance) ->
    get_forward_distance(Key, (NodeId + 1) rem trunc(math:pow(2, M)), M, Distance + 1)
.

get_closest(_, [], MinNode, _, _) ->
    MinNode;
get_closest(Key, FingerNodeIds, MinNode, MinVal, State) ->
    [First| Rest] = FingerNodeIds,
    Distance = get_forward_distance(Key, First, dict:fetch(m, State), 0),
    if
        Distance < MinVal ->
            get_closest(Key, Rest, First, Distance, State);
        true -> 
            get_closest(Key, Rest, MinNode, MinVal, State)
    end
.

get_closest_node(Key, FingerNodeIds, State) ->
    case lists:member(Key, FingerNodeIds) of
        true -> Key;
        _ -> get_closest(Key, FingerNodeIds, -1, 10000000, State)
    end

.


is_in_range(From, To, Key, M) ->
    if 
        From < To -> 
            (From =< Key) and (Key =< To);
        trunc(From) == trunc(To) ->
            trunc(Key) == trunc(From);
        From > To ->
            ((Key >= 0) and (Key =< To)) or ((Key >= From) and (Key < trunc(math:pow(2, M))))
    end
.

closest_preceding_finger(_, NodeState, 0) -> NodeState;
closest_preceding_finger(Id, NodeState, M) -> 
    MthFinger = lists:nth(M, dict:fetch(finger_table, NodeState)),
    
    case is_in_range(dict:fetch(id, NodeState), Id, dict:fetch(node ,MthFinger), dict:fetch(m, NodeState)) of
        true -> 

            dict:fetch(pid ,MthFinger) ! {state, self()},
            receive
                {statereply, FingerNodeState} ->
                    FingerNodeState
            end,
            FingerNodeState;

        _ -> closest_preceding_finger(Id, NodeState, M - 1)
    end
.

find_predecessor(Id, NodeState) ->
    case 
        is_in_range(dict:fetch(id, NodeState) + 1, dict:fetch(id, dict:fetch(successor, NodeState)), Id, dict:fetch(m, NodeState)) of 
            true -> NodeState;
            _ -> find_predecessor(Id, closest_preceding_finger(Id, NodeState, dict:fetch(m, NodeState)))
    end
.

find_successor(Id, NodeState) ->
    PredicessorNodeState = find_predecessor(Id, NodeState),
    dict:fetch(successor, PredicessorNodeState)
.


wait_node(NodeState) ->
    Hash = dict:fetch(id, NodeState),
    receive
            
        {lookup, Id, Key, HopsCount, _Pid} ->

                NodeVal = get_closest_node(Key, dict:fetch_keys(dict:fetch(finger_table ,NodeState)), NodeState),
                UpdatedState = NodeState,
                %io:format("Lookup::: ~p  For Key ~p  ClosestNode ~p ~n", [Hash, Key, NodeVal]),
                if 
                    
                    (Hash == Key) -> 
                        taskcompletionmonitor ! {completed, Hash, HopsCount, Key};
                    (NodeVal == Key) and (Hash =/= Key) -> 
                        taskcompletionmonitor ! {completed, Hash, HopsCount, Key};
                    
                    true ->
                        dict:fetch(NodeVal, dict:fetch(finger_table, NodeState)) ! {lookup, Id, Key, HopsCount + 1, self()}
                end
                ;
        {kill} ->
            UpdatedState = NodeState,
            exit("received exit signal");
        {state, Pid} -> Pid ! NodeState,
                        UpdatedState = NodeState;
        {get_successor, Id, Pid} ->
                        FoundSeccessor = find_successor(Id, NodeState),
                        UpdatedState = NodeState,
                        {Pid} ! {get_successor_reply, FoundSeccessor};

        
        {fix_fingers, FingerTable} -> 
            % io:format("Received Finger for ~p ~p", [Hash, FingerTable]),
            UpdatedState = dict:store(finger_table, FingerTable, NodeState)
    end, 
    wait_node(UpdatedState).


node(Hash, M, ChordNodes, _NodeState) -> 
    %io:format("Node is spawned with hash ~p",[Hash]),
    FingerTable = lists:duplicate(M, random_Node(Hash, ChordNodes)),
    NodeStateUpdated = dict:from_list([{id, Hash}, {predecessor, nil}, {finger_table, FingerTable}, {next, 0}, {m, M}]),
    wait_node(NodeStateUpdated)        
.


process_nodes(ChordNodes, _, _, 0, NetworkState) -> 
    [ChordNodes, NetworkState];
process_nodes(ChordNodes, TotalNodes, M, NumNodes, NetworkState) ->
    [Hash, NewNetworkState] = node_chord(ChordNodes, TotalNodes,  M, NetworkState),
    process_nodes(lists:append(ChordNodes, [Hash]), TotalNodes, M, NumNodes - 1, NewNetworkState)
.



get_ith_successor(Hash, NetworkState, I,  M) -> 
    case dict:find((Hash + I) rem trunc(math:pow(2, M)), NetworkState) of
        error ->
             get_ith_successor(Hash, NetworkState, I + 1, M);
        _ -> (Hash + I) rem trunc(math:pow(2, M))
    end
.

fineger_table_data(_, _, M, M,FingerList) ->
    FingerList;
fineger_table_data(Node, NetworkState, M, I, FingerList) ->
    Hash = element(1, Node),
    Ith_succesor = get_ith_successor(Hash, NetworkState, trunc(math:pow(2, I)), M),
    fineger_table_data(Node, NetworkState, M, I + 1, FingerList ++ [{Ith_succesor, dict:fetch(Ith_succesor, NetworkState)}] )
.


fingtables(_, [], FTDict,_) ->
    FTDict;

fingtables(NetworkState, NetList, FTDict,M) ->
    [First | Rest] = NetList,
    FingerTables = fineger_table_data(First, NetworkState,M, 0,[]),
    fingtables(NetworkState, Rest, dict:store(element(1, First), FingerTables, FTDict), M)
.



send_finger_tables_nodes([], _, _) ->
    ok;
send_finger_tables_nodes(NodesToSend, NetworkState, FingerTables) ->
    [First|Rest] = NodesToSend,
    Pid = dict:fetch(First ,NetworkState),
    Pid ! {fix_fingers, dict:from_list(dict:fetch(First, FingerTables))},
    send_finger_tables_nodes(Rest, NetworkState, FingerTables)
.


send_finger_tables(NetworkState,M) ->
    FingerTables = fingtables(NetworkState, dict:to_list(NetworkState), dict:new(),M),
    % io:format("~n~p~n", [FingerTables]),
    send_finger_tables_nodes(dict:fetch_keys(FingerTables), NetworkState, FingerTables)
.

get_node_pid(Hash, NetworkState) -> 
    case dict:find(Hash, NetworkState) of
        error -> nil;
        _ -> dict:fetch(Hash, NetworkState)
    end
.

send_message_to_node(_, [], _) ->
    ok;
send_message_to_node(Key, ChordNodes, NetworkState) ->
    [First | Rest] = ChordNodes,
    Pid = get_node_pid(First, NetworkState),
    Pid ! {lookup, First, Key, 0, self()},
    send_message_to_node(Key, Rest, NetworkState)
.

send_messages_all_nodes(_, 0, _, _) ->
    ok;
send_messages_all_nodes(ChordNodes, NumRequest, M, NetworkState) ->
    timer:sleep(1000),
    Key = lists:nth(rand:uniform(length(ChordNodes)), ChordNodes),
    send_message_to_node(Key, ChordNodes, NetworkState),
    send_messages_all_nodes(ChordNodes, NumRequest - 1, M, NetworkState)
.

kill_all_nodes([], _) ->
    ok;
kill_all_nodes(ChordNodes, NetworkState) -> 
    [First | Rest] = ChordNodes,
    get_node_pid(First, NetworkState) ! {kill},
    kill_all_nodes(Rest, NetworkState).

getTotalHops() ->
    receive
        {totalhops, HopsCount} ->
            HopsCount
        end.


task_is_completed(0, HopsCount) ->
    start_process ! {totalhops, HopsCount}
;

task_is_completed(NumRequests, HopsCount) ->
    receive 
        {completed, _Pid, HopsCountForTask, _Key} ->
            % io:format("received completion from ~p, Number of Hops ~p, For Key ~p", [Pid, HopsCountForTask, Key]),
            task_is_completed(NumRequests - 1, HopsCount + HopsCountForTask)
    end
.


send_kill(ChordNodes, NumNodes, NumRequest, M, NetworkState) ->
    register(taskcompletionmonitor, spawn(chordprotocol, task_is_completed, [NumNodes * NumRequest, 0])),

    send_messages_all_nodes(ChordNodes, NumRequest, M, NetworkState),

    TotalHops = getTotalHops(),
    
    {ok, File} = file:open("./statistics.txt", [append]),
    io:format(File, "~n Average Number of Hops = ~p   Total Number of Hops = ~p    Number Of Nodes = ~p    Number Of Requests = ~p  ~n", [TotalHops/(NumNodes * NumRequest), TotalHops, NumNodes , NumRequest]),
    io:format("~n Average Number of Hops = ~p   Total Number of Hops = ~p    Number Of Nodes = ~p    Number Of Requests = ~p  ~n", [TotalHops/(NumNodes * NumRequest), TotalHops, NumNodes , NumRequest]),
    kill_all_nodes(ChordNodes, NetworkState)
.

starting_the_network(Num_Nodes, Num_Request) ->
    M = get_number_of_nodes(Num_Nodes),
    [CNodes, NetState] = process_nodes([], round(math:pow(2, M)), M, Num_Nodes, dict:new()),
    
    send_finger_tables(NetState,M),
    send_kill(CNodes, Num_Nodes, Num_Request, M, NetState)
.


running_the_server(Nodes, Requests) ->
    register(start_process, spawn(chordprotocol, starting_the_network, [Nodes, Requests]))
.
