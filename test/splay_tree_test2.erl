%% performance test module
-module(splay_tree_test2).

-export([test1/0, test1_dict/0,
         test2/0, test2_dict/0,
         test3/0, test3_dict/0,
         test4/0, test4_dict/0]).

%% auxiliary functions
loopN(0, _, Acc) -> Acc;
loopN(N, Fun, Acc) ->
    loopN(N-1, Fun, Fun(Acc)).

loopN2(0, _, _, Acc) -> Acc;
loopN2(N, I, Fun, Acc) ->
    loopN2(N-1, I+1, Fun, Fun(I, Acc)).
    
%% random, small, store
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

%% sequential, small, store
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


%% random, big, store
test3() ->
    lists:foldl(fun (N, Acc) ->
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
                lists:seq(0, 501, 50)).

test3_dict() ->
    lists:foldl(fun (N, Acc) ->
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
                lists:seq(0, 501, 50)).

%% random, big, erase
test4() ->
    lists:foldl(fun (N, Acc) ->
                        random:seed({1,1,1}),
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
                lists:seq(0, 501, 50)).

test4_dict() ->
    lists:foldl(fun (N, Acc) ->
                        random:seed({1,1,1}),
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
                lists:seq(0, 501, 50)).
