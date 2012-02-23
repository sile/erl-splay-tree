%% TODO:  ディレクトリ移動
-module(splay_tree_test).

-export([test/0]).

-compile(export_all). % XX:

-define(DO_TEST(Name),
        begin
            io:format("% ~s ... ", [Name]),
            Name(),
            io:format("ok~n")
        end).

-define(ENTRIES, [{forth, 100},
                  {erlang, 10},
                  {lisp, 3},
                  {ruby, 4},
                  {python, 30},
                  {lisp, 20},
                  {java, 0}]).

-define(SORTED_UNIQUE_ENTRIES, [{erlang,10},
                                {forth,100},
                                {java,0},
                                {lisp,20},
                                {python,30},
                                {ruby,4}]).

test() ->
    ?DO_TEST(test1),
    ?DO_TEST(test2),
    ?DO_TEST(test3),
    ?DO_TEST(test4),
    ?DO_TEST(test5),
    ?DO_TEST(test6),
    ?DO_TEST(test7),
    ?DO_TEST(test8),
    ?DO_TEST(test9),
    ok.

test1() ->
    Tree = splay_tree:from_list(?ENTRIES),
    ?SORTED_UNIQUE_ENTRIES = splay_tree:to_list(Tree).
    
test2() ->
    Tree = splay_tree:from_list(?ENTRIES ++ ?ENTRIES),
    ?SORTED_UNIQUE_ENTRIES = splay_tree:to_list(Tree).

test3() ->
    Tree = splay_tree:from_list(?ENTRIES),

    lists:foreach(fun ({Key, Value}) ->
                          {{ok, Value}, _} =  splay_tree:find(Key, Tree)
                  end,
                  ?SORTED_UNIQUE_ENTRIES),
    
    lists:foldl(fun ({Key, Value}, Tree2) ->
                        {{ok, Value}, Tree3} = splay_tree:find(Key, Tree2),
                        Tree3
                end,
                Tree,
                ?SORTED_UNIQUE_ENTRIES).

test4() ->
    Size = length(?SORTED_UNIQUE_ENTRIES),
    Size = splay_tree:size(splay_tree:from_list(?ENTRIES)).

test5() ->
    lists:foldl(fun ({Key, _}, Tree) ->
                        Tree2 = splay_tree:erase(Key, Tree),
                        {error, Tree3} = splay_tree:find(Key, Tree2),
                        Tree3
                end,
                splay_tree:from_list(?ENTRIES),
                ?SORTED_UNIQUE_ENTRIES).

test6() ->
    Tree = lists:foldl(fun ({Key, Value}, Tree) ->
                               splay_tree:update(Key, fun (_) -> Value end, Value, Tree)
                       end,
                       splay_tree:new(),
                       ?ENTRIES),
    ?SORTED_UNIQUE_ENTRIES = splay_tree:to_list(Tree),
    
    Tree2 = lists:foldl(fun ({Key, _}, Tree2) ->
                                splay_tree:update(Key, fun (V) -> {V,V} end, null, Tree2)
                        end,
                        Tree,
                        ?SORTED_UNIQUE_ENTRIES),
    List = [{K,{V,V}} || {K,V} <- ?SORTED_UNIQUE_ENTRIES],
    List = splay_tree:to_list(Tree2).
    
test7() ->                                                        
    Tree = 
        splay_tree:filter(fun (_, V) -> V rem 2 =:= 0 end,
                          splay_tree:from_list(?ENTRIES)),
    
    List = [{K,V} || {K,V} <- ?SORTED_UNIQUE_ENTRIES, V rem 2 =:= 0],
    List = splay_tree:to_list(Tree).

test8() ->
    {Keys, Sum} =
        splay_tree:fold(fun (K, V, {Keys, Sum}) ->
                                {[K|Keys], Sum+V}
                        end,
                        {[], 0},
                        splay_tree:from_list(?ENTRIES)),
    
    {Keys, Sum} =
        lists:foldl(fun ({K,V}, {Keys2, Sum2}) ->
                            {[K|Keys2], Sum2+V}
                    end,
                    {[], 0},
                    ?SORTED_UNIQUE_ENTRIES).

test9() ->
    Tree = 
        splay_tree:map(fun (K, V) -> {K, V*V} end,
                       splay_tree:from_list(?ENTRIES)),
    
    List = 
        lists:map(fun ({K,V}) -> {K, {K, V*V}} end,
                  ?SORTED_UNIQUE_ENTRIES),
    
    List = splay_tree:to_list(Tree).
