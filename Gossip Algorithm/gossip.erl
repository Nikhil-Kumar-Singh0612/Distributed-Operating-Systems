-module(gossip).
-author("Nikhil").
-import(timer,[send_after/3]).

-export([gossipWorker/2, gossip_spreader/2]).

gossip_spreader(Rumor,[]) -> "nothing";
gossip_spreader(Rumor, Neighbors) ->
  Dest = get_random_neighbor(Neighbors),
  send_after(100, Dest, {rumor,Rumor}),
  gossip_spreader(Rumor, Neighbors).

get_random_neighbor(Neighbors) ->
  lists:nth(rand:uniform(length(Neighbors)),Neighbors).

gossipWorker(N, Neighbors) ->
  receive
    hello -> io:format("In gossip algorithm~n");
    {setup, NeighborList} ->
      tpid ! iamready,
      gossipWorker(N, NeighborList);

    {rumor, Rumor} ->
      if
        N+1 == 1 ->
          tpid ! heard,
          spawn(gossip, gossip_spreader, [Rumor, Neighbors]),
          gossipWorker(N+1, Neighbors);
        N < 12 ->
          gossipWorker(N+1, Neighbors);
        N >= 12 ->
          exit(self(),normal)
      end
  end,
  gossipWorker(N, Neighbors).

