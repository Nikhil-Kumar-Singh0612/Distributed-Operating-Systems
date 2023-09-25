-module(topology).
-author("Nikhil").
-import(lists,[nth/2, member/2]).
-export([grid_connected/3, line_connected/3, fully_connected/2, imp2D_connected/3]).

grid_connected(1, RowLength, Pids) ->
  nth(1, Pids) ! {setup, [nth(2, Pids), nth(RowLength+1, Pids), nth(RowLength + 2, Pids)]};
grid_connected(I, RowLength, Pids) ->
  RowCount = ceil(I/RowLength),
  Remainder = I rem RowLength,
  if
    Remainder == 0 -> ColumnCount = RowLength;
    true -> ColumnCount = Remainder
  end,
  if
    I == RowLength ->
      Neigh = [nth(I-1, Pids), nth(I+RowLength, Pids), nth(I-1+RowLength, Pids)];
    I == RowLength*RowLength ->
      Neigh = [nth(I-1, Pids), nth(I-RowLength, Pids), nth(I-RowLength-1, Pids)];
    I == RowLength*(RowLength-1)+1 ->
      Neigh = [nth(I+1, Pids), nth(I-RowLength, Pids), nth(I-RowLength+1, Pids)];
    (RowCount == RowLength) ->
      Neigh = [nth(I-RowLength-1 , Pids), nth(I-RowLength, Pids), nth(I-RowLength+1, Pids),
        nth(I+1, Pids), nth(I-1, Pids)];
    (ColumnCount == RowLength) ->
      Neigh = [nth(I-RowLength-1 , Pids), nth(I-RowLength , Pids), nth(I-1, Pids),
        nth(I+RowLength-1, Pids), nth(I+RowLength, Pids)];
    (RowCount == 1) ->
      Neigh = [nth(I+RowLength-1, Pids), nth(I-1, Pids), nth(I+1 , Pids),
        nth(I+RowLength, Pids), nth(I+RowLength+1, Pids)];
    (ColumnCount == 1) ->
      Neigh = [nth(I-RowLength , Pids), nth(I-RowLength+1, Pids), nth(I+1, Pids),
        nth(I+RowLength, Pids), nth(I+RowLength+1, Pids)];
    true ->
      Neigh = [nth(I-RowLength-1, Pids), nth(I-RowLength, Pids), nth(I-RowLength+1, Pids),
        nth(I-1, Pids), nth(I+1, Pids),
        nth(I+RowLength-1, Pids), nth(I+RowLength, Pids), nth(I+RowLength+1, Pids)]
  end,
  nth(I, Pids) ! {setup, Neigh},
  grid_connected(I-1, RowLength, Pids).


line_connected(1, N, Pids) ->
  nth(1, Pids) ! {setup, [nth(2, Pids)]};
line_connected(I, N, Pids) ->
  if
    I == N ->
      Neighs = [nth(I-1, Pids)];
    true ->
      Neighs = [nth(I-1, Pids), nth(I+1, Pids)]
  end,
  nth(I, Pids) ! {setup, Neighs},
  line_connected(I-1, N, Pids).

fully_connected(Prelist,[])-> "Completed";
fully_connected(Prelist,[Head|Tail])->
  Neighs = Prelist ++ Tail,
  Head ! {setup,Neighs},
  fully_connected([Head|Prelist],Tail).

get_random(Pids, ExcludeList) ->
  RandomValue = nth(rand:uniform(length(Pids)), Pids),
  case member(RandomValue, ExcludeList) of
    true -> get_random(Pids, ExcludeList);
    false -> RandomValue
  end.

  imp2D_connected(1, RowLength, Pids) ->
  Neigh = [nth(2, Pids), nth(RowLength+1, Pids), nth(RowLength + 2, Pids)],
  nth(1, Pids) ! {setup, [get_random(Pids, Neigh)|Neigh]};
  imp2D_connected(I, RowLength, Pids) ->
  RowCount = ceil(I/RowLength),
  Remainder = I rem RowLength,
  if
    Remainder == 0 -> ColumnCount = RowLength;
    true -> ColumnCount = Remainder
  end,
  if
    I == RowLength ->
      Neigh = [nth(I-1, Pids), nth(I+RowLength, Pids), nth(I-1+RowLength, Pids)];
    I == RowLength*RowLength ->
      Neigh = [nth(I-1, Pids), nth(I-RowLength, Pids), nth(I-RowLength-1, Pids)];
    I == RowLength*(RowLength-1)+1 ->
      Neigh = [nth(I+1, Pids), nth(I-RowLength, Pids), nth(I-RowLength+1, Pids)];
    (RowCount == RowLength) ->
      Neigh = [nth(I-RowLength-1 , Pids), nth(I-RowLength, Pids), nth(I-RowLength+1, Pids),
        nth(I+1, Pids), nth(I-1, Pids)];
    (ColumnCount == RowLength) ->
      Neigh = [nth(I-RowLength-1 , Pids), nth(I-RowLength , Pids), nth(I-1, Pids),
        nth(I+RowLength-1, Pids), nth(I+RowLength, Pids)];
    (RowCount == 1) ->
      Neigh = [nth(I+RowLength-1, Pids), nth(I-1, Pids), nth(I+1 , Pids),
        nth(I+RowLength, Pids), nth(I+RowLength+1, Pids)];
    (ColumnCount == 1) ->
      Neigh = [nth(I-RowLength , Pids), nth(I-RowLength+1, Pids), nth(I+1, Pids),
        nth(I+RowLength, Pids), nth(I+RowLength+1, Pids)];
    true ->
      Neigh = [nth(I-RowLength-1, Pids), nth(I-RowLength, Pids), nth(I-RowLength+1, Pids),
        nth(I-1, Pids), nth(I+1, Pids),
        nth(I+RowLength-1, Pids), nth(I+RowLength, Pids), nth(I+RowLength+1, Pids)]
  end,
  nth(I, Pids) ! {setup, [get_random(Pids, Neigh)|Neigh]},
  imp2D_connected(I-1, RowLength, Pids).
