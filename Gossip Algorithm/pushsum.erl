-module(pushsum).
-author("nikhil").

-export([push_sum_worker/4]).

get_random_neighbor(Neighbors) ->
  lists:nth(rand:uniform(length(Neighbors)),Neighbors).

push_sum_worker(N, ExistingS, ExistingW, Neighbors) ->
  Threshold = 0.0000000001,
  receive
    hello ->
      io:format("In pushsum~n");
    {setup, NeighborList} ->
      io:format("The neighbors of ~p are: ~p~n", [self(), NeighborList]),
      tpid ! iamready,
      push_sum_worker(N, ExistingS, ExistingW, NeighborList);
    {rumor, S, W} ->
      NewS = ExistingS + S,
      NewW = ExistingW + W,
      HalfS = NewS/2,
      HalfW = NewW/2,
      Diff = abs(NewS/NewW - ExistingS/ExistingW),
      if
        (Diff < Threshold) and (N >= 3) ->
          tpid ! {heard, NewS/NewW};
        (Diff < Threshold) and (N < 3) ->
          get_random_neighbor(Neighbors) ! {rumor, HalfS, HalfW},
          push_sum_worker(N+1, HalfS, HalfW, Neighbors);
        Diff >= Threshold->
          get_random_neighbor(Neighbors) ! {rumor, HalfS, HalfW},
          push_sum_worker(0, HalfS, HalfW, Neighbors)
      end
  end,
  push_sum_worker(N, ExistingS, ExistingW, Neighbors).