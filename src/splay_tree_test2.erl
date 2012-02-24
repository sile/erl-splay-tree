-module(splay_tree_test2).

-compile(export_all).

loopN(0, _, Acc) -> Acc;
loopN(N, Fun, Acc) ->
    loopN(N-1, Fun, Fun(Acc)).

loopN2(0, _, _, Acc) -> Acc;
loopN2(N, I, Fun, Acc) ->
    loopN2(N-1, I+1, Fun, Fun(I, Acc)).
    
%% random, small
test1() ->
    lists:foldl(fun (N, Acc) ->
                        [timer:tc(
                           fun () ->
                                   splay_tree:size(
                                     loopN(N, 
                                           fun (Tree) ->
                                                   K = random:uniform(),
                                                   splay_tree:store(K, K, Tree)
                                           end,
                                           splay_tree:new()))
                           end, 
                           []) | Acc]
                end,
                [],
                lists:seq(1, 1000)).

test1_dict() ->
    lists:foldl(fun (N, Acc) ->
                        [timer:tc(
                           fun () ->
                                   dict:size(
                                     loopN(N, 
                                           fun (Tree) ->
                                                   K = random:uniform(),
                                                   dict:store(K, K, Tree)
                                           end,
                                           dict:new()))
                           end, 
                           []) | Acc]
                end,
                [],
                lists:seq(1, 1000)).      


%% seq, small
test2() ->
    lists:foldl(fun (N, Acc) ->
                        [timer:tc(
                           fun () ->
                                   splay_tree:size(
                                     loopN2(N, 
                                            0,
                                           fun (K, Tree) ->
                                                   splay_tree:store(K, K, Tree)
                                           end,
                                           splay_tree:new()))
                           end, 
                           []) | Acc]
                end,
                [],
                lists:seq(1, 1000)).

test2_dict() ->
    lists:foldl(fun (N, Acc) ->
                        [timer:tc(
                           fun () ->
                                   dict:size(
                                     loopN2(N, 
                                            0,
                                           fun (K, Tree) ->
                                                   dict:store(K, K, Tree)
                                           end,
                                           dict:new()))
                           end, 
                           []) | Acc]
                end,
                [],
                lists:seq(1, 1000)).      


%% random, big
test3() ->
    lists:foldl(fun (N, Acc) ->
                        io:format("# ~p~n", [N]),
                        [timer:tc(
                           fun () ->
                                   splay_tree:size(
                                     loopN(N*N, 
                                           fun (Tree) ->
                                                   K = random:uniform(),
                                                   splay_tree:store(K, K, Tree)
                                           end,
                                           splay_tree:new()))
                           end, 
                           []) | Acc]
                end,
                [],
                lists:seq(1, 500, 10)).

test3_dict() ->
    lists:foldl(fun (N, Acc) ->
                        io:format("# ~p~n", [N]),
                        [timer:tc(
                           fun () ->
                                   dict:size(
                                     loopN(N*N, 
                                           fun (Tree) ->
                                                   K = random:uniform(),
                                                   dict:store(K, K, Tree)
                                           end,
                                           dict:new()))
                           end, 
                           []) | Acc]
                end,
                [],
                lists:seq(1, 500, 10)).

%% random, big, erase
test5() ->
    lists:foldl(fun (N, Acc) ->
                        io:format("# ~p~n", [N]),
                        Tree = loopN(N*N, 
                                     fun (Tree) ->
                                             K = random:uniform(N*N*10),
                                             splay_tree:store(K, K, Tree)
                                           end,
                                     splay_tree:new()),
                        [timer:tc(
                           fun () ->
                                   splay_tree:size(
                                     loopN(N*N, 
                                           fun (Tree2) ->
                                                   K = random:uniform(N*N*10),
                                                   splay_tree:erase(K, Tree2)
                                           end,
                                           Tree))
                           end,
                           []) | Acc]
                end,
                [],
                lists:seq(1, 500, 10)).

test5_dict() ->
    lists:foldl(fun (N, Acc) ->
                        io:format("# ~p~n", [N]),
                        Tree = loopN(N*N, 
                                     fun (Tree) ->
                                             K = random:uniform(N*N*10),
                                             dict:store(K, K, Tree)
                                           end,
                                     dict:new()),
                        [timer:tc(
                           fun () ->
                                   dict:size(
                                     loopN(N*N, 
                                           fun (Tree2) ->
                                                   K = random:uniform(N*N*10),
                                                   dict:erase(K, Tree2)
                                           end,
                                           Tree))
                           end,
                           []) | Acc]
                end,
                [],
                lists:seq(1, 500, 10)).

each_lines(Fun, Acc, Path) ->
    {ok, In} = file:open(Path, [read]),
    try
        each_lines_impl(Fun, Acc, In)
    after
        file:close(In)
    end.

each_lines_impl(Fun, Acc, In) ->
    case file:read_line(In) of
        eof -> Acc;
        {error, Reason} -> {error, Reason};
        {ok, Line} ->
            each_lines_impl(Fun, Fun(Line, Acc), In)
    end.

test6(Path) ->
    {Time, Tree} =
        timer:tc(
          fun () ->
                  each_lines(
                    fun (Line, Tree) ->
                            splay_tree:update(Line, fun (N) -> N+1 end, 1, Tree)
                    end,
                    splay_tree:new(),
                    Path)
          end,
          []),
    
    {Time, 
     splay_tree:size(Tree),
     lists:reverse(lists:ukeysort(2, splay_tree:to_list(Tree)))}.

test6_dict(Path) ->
    {Time, Tree} =
        timer:tc(
          fun () ->
                  each_lines(
                    fun (Line, Tree) ->
                            %% TODO: update_counter
                            dict:update(Line, fun (N) -> N+1 end, 1, Tree)
                    end,
                    dict:new(),
                    Path)
          end,
          []),
    
    {Time, 
     dict:size(Tree),
     lists:reverse(lists:ukeysort(2, dict:to_list(Tree)))}.
