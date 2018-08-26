-module(splay_tree_bench).

%%----------------------------------------------------------------------------------------------------------------------
%% Exported API
%%----------------------------------------------------------------------------------------------------------------------
-export([
         bench/0,
         print_bench/0
        ]).

%%----------------------------------------------------------------------------------------------------------------------
%% Exported Functions
%%----------------------------------------------------------------------------------------------------------------------
-spec bench() -> [PerResult] when
      PerResult :: {KeyType, Method, Order, Case, module(), ElapsedMicroSeconds},
      KeyType   :: integer | binary | tuple,
      Order     :: sorted | random,
      Case      :: {1, 100000}  % {Size, LoopCount}
                 | {100, 1000}
                 | {10000, 10},
      Method    :: store | successful_find | successful_erase,
      ElapsedMicroSeconds :: non_neg_integer().
bench() ->
    Result0 = do_bench(integer, fun generate_sorted_integer_entries/1),
    %% Result1 = do_bench(binary, fun generate_sorted_binary_entries/1),
    %% Result2 = do_bench(tuple, fun generate_sorted_tuple_entries/1),
    lists:sort(Result0). % ++ Result1 ++ Result2).

-spec print_bench() -> ok.
print_bench() ->
    io:format("\n"),
    Result = bench(),

    io:format("| operation | input data order | tree size | iteration count | module | elapsed(us) |\n"),
    io:format("|-----------|------------------|----------:|----------------:|--------|------------:|\n"),
    lists:foreach(
      fun ({_KeyType, Method, Order, {Size, LoopCount}, Module, Elapsed}) ->
              io:format("| ~s | ~s | ~p | ~p | ~20s | ~p |\n",
                        [Method, Order, Size, LoopCount, Module, Elapsed])
      end,
      Result).

%%----------------------------------------------------------------------------------------------------------------------
%% Internal Functions
%%----------------------------------------------------------------------------------------------------------------------
do_bench(KeyType, GenerateFun) ->
    lists:flatmap(
      fun ({Size, LoopCount}) ->
              io:format(standard_error, "# [~s] ~p, ~p\n", [KeyType, Size, LoopCount]),

              Sorted = GenerateFun(Size),
              Random = shuffle(Sorted),
              lists:flatmap(
                fun (Module) ->
                        Empty = (method(Module, from_list))([]),
                        Map   = (method(Module, from_list))(Sorted),
                        [
                         {KeyType, store, sorted, {Size, LoopCount}, Module, times(LoopCount, Sorted, Empty, method(Module, store))},
                         {KeyType, store, random, {Size, LoopCount}, Module, times(LoopCount, Random, Empty, method(Module, store))},
                         {KeyType, successful_find, sorted, {Size, LoopCount}, Module, times(LoopCount, Sorted, Map, method(Module, find))},
                         {KeyType, successful_find, random, {Size, LoopCount}, Module, times(LoopCount, Random, Map, method(Module, find))},
                         {KeyType, successful_erase, sorted, {Size, LoopCount}, Module, times(LoopCount, Sorted, Map, method(Module, erase))},
                         {KeyType, successful_erase, random, {Size, LoopCount}, Module, times(LoopCount, Random, Map, method(Module, erase))}
                        ]
                end,
                modules())
      end,
      case_list()).

case_list() ->
    [
     {1, 100000},
     {100, 1000},
     {10000, 10}
    ].

-spec modules() -> [module()].
modules() ->
%%    [splay_tree, splay_tree_with_size].
    [splay_tree_with_size, splay_tree].

method(splay_tree, from_list) -> fun (List) -> splay_tree:from_list(shuffle(List)) end;
method(splay_tree, store)     -> fun ({K, V}, M) -> splay_tree:store(K, V, M) end;
method(splay_tree, find)      -> fun ({K, _}, M) -> element(2, splay_tree:find(K, M)) end;
method(splay_tree, erase)     -> fun ({K, _}, M) -> splay_tree:erase(K, M) end;

method(splay_tree_with_size, from_list) -> fun (List) -> splay_tree_with_size:from_list(shuffle(List)) end;
method(splay_tree_with_size, store)     -> fun ({K, V}, M) -> splay_tree_with_size:store(K, V, M) end;
method(splay_tree_with_size, find)      -> fun ({K, _}, M) -> element(2, splay_tree_with_size:find(K, M)) end;
method(splay_tree_with_size, erase)     -> fun ({K, _}, M) -> splay_tree_with_size:erase(K, M) end.

times(LoopCount, InputData, Map, Fun) ->
    true = garbage_collect(),
    {Elapsed, _} =
        timer:tc(
          fun () ->
                  loop(LoopCount, InputData, Map, Fun)
          end),
    Elapsed.

loop(0, _, _, _) ->
    ok;
loop(LoopCount, InputData, Map, Fun) ->
    _ = lists:foldl(fun (X, Acc) -> Fun(X, Acc) end, Map, InputData),
    loop(LoopCount - 1, InputData, Map, Fun).

generate_sorted_integer_entries(Count) ->
    [{X, X} || X <- lists:seq(0, Count - 1)].

%% generate_sorted_binary_entries(Count) ->
%%     generate_sorted_binary_entries(Count, []).

%% generate_sorted_binary_entries(Count, Acc) ->
%%     List0 =
%%         [begin
%%              X = crypto:strong_rand_bytes(rand:uniform(100)),
%%              {X, X}
%%          end || _ <- lists:seq(0, Count - 1)],
%%     List1 = lists:usort(List0),
%%     List2 = lists:umerge(Acc, List1),
%%     case (length(Acc) + Count) - length(List2) of
%%         0 -> List2;
%%         N -> generate_sorted_binary_entries(N, List2)
%%     end.

%% generate_sorted_tuple_entries(Count) ->
%%     lists:sort(
%%       lists:zip(shuffle(generate_sorted_integer_entries(Count)),
%%                 shuffle(generate_sorted_binary_entries(Count)))).

shuffle(List) ->
    [X||{_,X} <- lists:keysort(1, [{rand:uniform(), N} || N <- List])].
