%% @copyright 2013 Takeru Ohta <phjgt308@gmail.com>
%%
%% @doc SplayTree
-module(splay_tree).

-compile(inline).

%%--------------------------------------------------------------------------------
%% Exported API
%%--------------------------------------------------------------------------------
-export([new/0, store/3, find/2, find_largest/1, find_smallest/1,
         take_largest/1, take_smallest/1,
         lookup/2, get_value/3, erase/2,
         size/1, is_empty/1, update/4, update/3, filter/2, map/2,
         keys/1, values/1,
         foldl/3, foldr/3, foldl_while/3, foldr_while/3, from_list/1, to_list/1, split/2]).

-export_type([tree/0, tree/2, key/0, value/0,
              update_fn/0, map_fn/0, fold_fn/0, fold_while_fn/0, pred_fn/0]).

%%--------------------------------------------------------------------------------
%% Types
%%--------------------------------------------------------------------------------
-type tree()             :: maybe_tree_node().
-type tree(_Key, _Vlaue) :: maybe_tree_node().

-type key()       :: any().
-type value()     :: any().
-type update_fn() :: fun((value()) -> value()).
-type map_fn()    :: fun((key(),value()) -> value()).
-type pred_fn()   :: fun((key(),value()) -> boolean()).
-type fold_fn()   :: fun((key(),value(),term()) -> term()).
-type fold_while_fn() :: fun ((key(), value(), term()) -> {boolean(), term()}).

-type maybe_tree_node() :: tree_node() | nil.
-type tree_node()       :: inner_node() | leaf_node().
-type inner_node()      :: {key(), value(), maybe_tree_node(), maybe_tree_node()}.
-type leaf_node()       :: {key(), value()}.
-type direction()       :: lft | rgt.

%%--------------------------------------------------------------------------------
%% Exported Functions
%%--------------------------------------------------------------------------------
-spec new() -> tree().
new() -> nil.

-spec size(tree()) -> non_neg_integer().
size(Tree) -> foldl(fun (_, _, Count) -> Count+1 end, 0, Tree).

-spec is_empty(tree()) -> boolean().
is_empty(nil) -> true;
is_empty(_)   -> false.

-spec store(key(), value(), tree()) -> tree().
store(Key, Value, Root) ->
    case path_to_node(Key, Root) of
        {nil,  Path} -> splay(leaf(Key,Value), Path);
        {Node, Path} -> splay(val(Node,Value), Path)
    end.

-spec update(key(), update_fn(), value(), tree()) -> tree().
update(Key, Fun, Initial, Root) ->
    case path_to_node(Key, Root) of
        {nil,  Path} -> splay(leaf(Key,Initial), Path);
        {Node, Path} -> splay(val(Node,Fun(val(Node))), Path)
    end.

-spec update(key(), update_fn(), tree()) -> tree()|error.
update(Key, Fun, Root) ->
    case path_to_node(Key, Root) of
        {nil, _Path} -> error;
        {Node, Path} -> splay(val(Node,Fun(val(Node))), Path)
    end.

-spec find(key(), tree()) -> {error,tree()} | {{ok,value()},tree()}.
find(Key, Root) ->
    case path_to_node(Key,Root) of
        {nil,  Path} -> {error,              splay(Path)};
        {Node, Path} -> {{ok,val(Node)}, splay(Node,Path)}
    end.

-spec find_largest(tree()) -> {error, tree()} | {{ok,key(),value()},tree()}.
find_largest(Tree) ->
    case move_largest_node_to_front(Tree) of
        nil  -> {error, nil};
        Node -> {{ok, key(Node), val(Node)}, Node}
    end.

-spec find_smallest(tree()) -> {error, tree()} | {{ok,key(),value()},tree()}.
find_smallest(Tree) ->
    case move_smallest_node_to_front(Tree) of
        nil  -> {error, nil};
        Node -> {{ok, key(Node), val(Node)}, Node}
    end.

-spec take_largest(tree()) -> {error, tree()} | {{ok,key(),value()},tree()}.
take_largest(Tree) ->
    case move_largest_node_to_front(Tree) of
        nil  -> {error, nil};
        Node -> {{ok, key(Node), val(Node)}, lft(Node)}
    end.

-spec take_smallest(tree()) -> {error, tree()} | {{ok,key(),value()},tree()}.
take_smallest(Tree) ->
    case move_smallest_node_to_front(Tree) of
        nil  -> {error, nil};
        Node -> {{ok, key(Node), val(Node)}, rgt(Node)}
    end.

-spec lookup(key(), tree()) -> error | {ok,value()}.
lookup(Key, Root) ->
    case lookup_node(Key,Root) of
        nil  -> error;
        Node -> {ok, val(Node)}
    end.

-spec get_value(key(), tree(), value()) -> value().
get_value(Key, Root, DefaultValue) ->
    case lookup_node(Key,Root) of
        nil  -> DefaultValue;
        Node -> val(Node)
    end.

-spec erase(key(), tree()) -> tree().
erase(Key, Root) ->
    case path_to_node(Key, Root) of
        {nil,  Path} -> splay(Path);
        {Node,   []} -> pop_front(Node);
        {Node, Path} -> case {pop_front(Node), hd(Path)} of
                            {C, {lft,P}} -> splay(lft(P, C), tl(Path));
                            {C, {rgt,P}} -> splay(rgt(P, C), tl(Path))
                        end
    end.

-spec split(key(), tree()) -> {tree(), tree()}.
split(BorderKey, Tree) ->
    {_, Tree2} = find(BorderKey, Tree),
    case Tree2 of
        nil -> {nil, nil};
        _   ->
            case key(Tree2) < BorderKey of
                true  -> {rgt(Tree2, nil), rgt(Tree2)};
                false -> {lft(Tree2), lft(Tree2, nil)}
            end
    end.

-spec to_list(tree()) -> [{key(),value()}].
to_list(Tree) -> foldr(fun (K, V, Acc) -> [{K,V}|Acc] end, [], Tree).

-spec keys(tree()) -> [key()].
keys(Tree) -> foldr(fun (K, _, Acc) -> [K|Acc] end, [], Tree).

-spec values(tree()) -> [value()].
values(Tree) -> foldr(fun (_, V, Acc) -> [V|Acc] end, [], Tree).

-spec from_list([{key(),value()}]) -> tree().
from_list(List) -> lists:foldl(fun ({K, V}, Tree) -> store(K, V, Tree) end, new(), List).

-spec map(map_fn(), tree()) -> tree().
map(Fun, Tree) -> map_node(Fun, Tree).

-spec foldl(fold_fn(), term(), tree()) -> term().
foldl(Fun, Acc0, Tree) -> foldl_node(Fun, Tree, Acc0).

-spec foldr(fold_fn(), term(), tree()) -> term().
foldr(Fun, Acc0, Tree) -> foldr_node(Fun, Tree, Acc0).

-spec foldl_while(fold_while_fn(), term(), tree()) -> term().
foldl_while(Fun, Acc0, Tree) ->
    try
        foldl_while_node(Fun, Tree, Acc0)
    catch
        throw:{?MODULE, break, AccFinal} -> AccFinal
    end.

-spec foldr_while(fold_while_fn(), term(), tree()) -> term().
foldr_while(Fun, Acc0, Tree) ->
    try
        foldr_while_node(Fun, Tree, Acc0)
    catch
        throw:{?MODULE, break, AccFinal} -> AccFinal
    end.

-spec filter(pred_fn(), tree()) -> tree().
filter(Pred, Tree) ->
    foldl(fun (Key, Value, AccTree) ->
                  case Pred(Key, Value) of
                      false -> AccTree;
                      true  -> store(Key, Value, AccTree)
                  end
          end,
          new(),
          Tree).

%%--------------------------------------------------------------------------------
%% Internal Functions
%%--------------------------------------------------------------------------------
-spec key(tree_node()) -> key().
key(Node) -> element(1, Node).

-spec val(tree_node()) -> value().
val(Node) -> element(2, Node).

-spec val(tree_node(), value()) -> tree_node().
val(Node, Value) -> setelement(2, Node, Value).

-spec lft(tree_node()) -> maybe_tree_node().
lft({_, _, Lft, _}) -> Lft;
lft({_, _})         -> nil.

-spec lft(tree_node(), maybe_tree_node()) -> tree_node().
lft({Key, Val, _, nil}, nil) -> {Key, Val};
lft({Key, Val, _, Rgt}, Lft) -> {Key, Val, Lft, Rgt};
lft({Key, Val}, nil)         -> {Key, Val};
lft({Key, Val}, Lft)         -> {Key, Val, Lft, nil}.

-spec rgt(tree_node()) -> maybe_tree_node().
rgt({_, _, _, Rgt}) -> Rgt;
rgt({_, _})         -> nil.

-spec rgt(tree_node(), maybe_tree_node()) -> tree_node().
rgt({Key, Val, nil, _}, nil) -> {Key, Val};
rgt({Key, Val, Lft, _}, Rgt) -> {Key, Val, Lft, Rgt};
rgt({Key, Val}, nil)         -> {Key, Val};
rgt({Key, Val}, Rgt)         -> {Key, Val, nil, Rgt}.

-spec lft_rgt(tree_node(), maybe_tree_node(), maybe_tree_node()) -> tree_node().
lft_rgt(Node, Lft, Rgt) -> {key(Node), val(Node), Lft, Rgt}.

-spec rgt_lft(tree_node(), maybe_tree_node(), maybe_tree_node()) -> tree_node().
rgt_lft(Node, Rgt, Lft) -> {key(Node), val(Node), Lft, Rgt}.

-spec leaf(key(), value()) -> tree_node().
leaf(Key, Value) -> {Key, Value}.

-spec pop_front(tree_node()) -> maybe_tree_node().
pop_front(Node) ->
    case move_largest_node_to_front(lft(Node)) of
        nil   -> rgt(Node);
        Front -> rgt(Front, rgt(Node))
    end.

-spec move_largest_node_to_front(maybe_tree_node()) -> maybe_tree_node().
move_largest_node_to_front(nil)  -> nil;
move_largest_node_to_front(Node) -> move_largest_node_to_front(Node, []).

-spec move_largest_node_to_front(tree_node(), [tree_node()]) -> tree_node().
move_largest_node_to_front(Node, Path) ->
    case rgt(Node) of
        nil -> lft(Node, lists:foldl(fun rgt/2, lft(Node), Path));
        Rgt -> move_largest_node_to_front(Rgt, [Node|Path])
    end.

-spec move_smallest_node_to_front(maybe_tree_node()) -> maybe_tree_node().
move_smallest_node_to_front(nil)  -> nil;
move_smallest_node_to_front(Node) -> move_smallest_node_to_front(Node, []).

-spec move_smallest_node_to_front(tree_node(), [tree_node()]) -> tree_node().
move_smallest_node_to_front(Node, Path) ->
    case lft(Node) of
        nil -> rgt(Node, lists:foldl(fun lft/2, rgt(Node), Path));
        Lft -> move_smallest_node_to_front(Lft, [Node|Path])
    end.

-spec path_to_node(key(), maybe_tree_node()) -> {maybe_tree_node(), [{direction(),tree_node()}]}.
path_to_node(Key, Root) ->
    path_to_node(Key, Root, []).

-spec path_to_node(key(), maybe_tree_node(), [{direction(),tree_node()}]) ->
                          {maybe_tree_node(), [{direction(),tree_node()}]}.
path_to_node(_Key, nil, Path) -> {nil, Path};
path_to_node(Key, Node, Path) ->
    case key(Node) of
        K when Key < K -> path_to_node(Key, lft(Node), [{lft,Node}|Path]);
        K when Key > K -> path_to_node(Key, rgt(Node), [{rgt,Node}|Path]);
        _              -> {Node, Path}
    end.

-spec lookup_node(key(), maybe_tree_node()) -> maybe_tree_node().
lookup_node(_Key, nil) -> nil;
lookup_node(Key, Node) ->
    case key(Node) of
        K when Key < K -> lookup_node(Key, lft(Node));
        K when Key > K -> lookup_node(Key, rgt(Node));
        _              -> Node
    end.

-spec splay([{direction(),tree_node()}]) -> maybe_tree_node().
splay([])              -> nil;
splay([{_,Node}|Path]) -> splay(Node, Path).

-spec splay(tree_node(), [{direction(),tree_node()}]) -> tree_node().
splay(X, []) -> X;
splay(X, [{Dir, P}]) ->                % zig
    case Dir of
        lft -> rgt(X, lft(P, rgt(X)));
        rgt -> lft(X, rgt(P, lft(X)))
    end;
splay(X, [{Dir,P}, {Dir,G} | Path]) -> % zig-zig
    splay(case Dir of
              lft -> rgt(X, rgt_lft(P, lft(G, rgt(P)), rgt(X)));
              rgt -> lft(X, lft_rgt(P, rgt(G, lft(P)), lft(X)))
          end,
          Path);
splay(X, [{Dir,P}, {_,G} | Path]) ->   % zig-zag
    splay(case Dir of
              lft -> rgt_lft(X, lft(P, rgt(X)), rgt(G, lft(X)));
              rgt -> lft_rgt(X, rgt(P, lft(X)), lft(G, rgt(X)))
          end,
          Path).

-spec map_node(map_fn(), maybe_tree_node()) -> maybe_tree_node().
map_node(_Fun, nil)                 -> nil;
map_node(Fun, {Key, Val})           -> {Key, Fun(Key, Val)};
map_node(Fun, {Key, Val, Lft, Rgt}) -> {Key, Fun(Key, Val), map_node(Fun, Lft), map_node(Fun, Rgt)}.

-spec foldl_node(fold_fn(), maybe_tree_node(), term()) -> term().
foldl_node(_Fun, nil, Acc)                 -> Acc;
foldl_node(Fun, {Key, Val}, Acc)           -> Fun(Key, Val, Acc);
foldl_node(Fun, {Key, Val, Lft, Rgt}, Acc) -> foldl_node(Fun, Rgt, Fun(Key, Val, foldl_node(Fun, Lft, Acc))).

-spec foldr_node(fold_fn(), maybe_tree_node(), term()) -> term().
foldr_node(_Fun, nil, Acc)                 -> Acc;
foldr_node(Fun, {Key, Val}, Acc)           -> Fun(Key, Val, Acc);
foldr_node(Fun, {Key, Val, Lft, Rgt}, Acc) -> foldr_node(Fun, Lft, Fun(Key, Val, foldr_node(Fun, Rgt, Acc))).

-define(MAYBE_BREAK(Result),
        case Result of
            {false, Value} -> throw({?MODULE, break, Value});
            {true,  Value} -> Value
        end).

-spec foldl_while_node(fold_while_fn(), maybe_tree_node(), term()) -> term().
foldl_while_node(_Fun, nil, Acc)                 -> Acc;
foldl_while_node(Fun, {Key, Val}, Acc)           -> ?MAYBE_BREAK(Fun(Key, Val, Acc));
foldl_while_node(Fun, {Key, Val, Lft, Rgt}, Acc) -> foldl_while_node(Fun, Rgt, ?MAYBE_BREAK(Fun(Key, Val, foldl_while_node(Fun, Lft, Acc)))).

-spec foldr_while_node(fold_while_fn(), maybe_tree_node(), term()) -> term().
foldr_while_node(_Fun, nil, Acc)                 -> Acc;
foldr_while_node(Fun, {Key, Val}, Acc)           -> ?MAYBE_BREAK(Fun(Key, Val, Acc));
foldr_while_node(Fun, {Key, Val, Lft, Rgt}, Acc) -> foldr_while_node(Fun, Lft, ?MAYBE_BREAK(Fun(Key, Val, foldr_while_node(Fun, Rgt, Acc)))).
