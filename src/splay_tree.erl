%% @copyright 2013 Takeru Ohta <phjgt308@gmail.com>
%%
%% @doc Splay Tree
%%
%% == Note ==
%%
%% The keys of the entries in a tree are compared using the `==' operator
%% (e.g., `1' and `1.0' are regarded as the same keys).
%%
%% == References ==
%%
%% <ul>
%%  <li>Splay tree(Wikiepdia): [https://en.wikipedia.org/wiki/Splay_tree]</li>
%% </ul>
-module(splay_tree).

-compile(inline).
-compile([{no_auto_import, size/1}]).

%%--------------------------------------------------------------------------------
%% Exported API
%%--------------------------------------------------------------------------------
-export([new/0, store/3, find/2, find_largest/1, find_smallest/1,
         take_largest/1, take_smallest/1,
         find_lower_bound/2, find_upper_bound/2,
         lookup/2, get_value/3, erase/2,
         size/1, update_size/1, is_empty/1, update/4, update/3, filter/2, map/2,
         keys/1, values/1,
         foldl/3, foldr/3, foldl_while/3, foldr_while/3, from_list/1, to_list/1, split/2]).

-export([index/2, at/2]).

-export_type([tree/0, tree/2, key/0, value/0,
              update_fn/0, map_fn/0, fold_fn/0, fold_while_fn/0, pred_fn/0]).

%%--------------------------------------------------------------------------------
%% Exported Types
%%--------------------------------------------------------------------------------
-opaque tree() :: maybe_tree_node().
%% A splay tree.

-type tree(_Key, _Value) :: tree().
%% A splay tree.

-type key() :: any().
%% The key of an entry in a splay tree.
%%
%% == Note ==
%%
%% The keys are compared using the `==' operator
%% (e.g., `1' and `1.0' are regarded as the same keys).

-type value() :: any().
%% The value of an entry in a splay tree.

-type update_fn() :: fun((value()) -> value()).
%% A function for updating the value of an entry in a splay tree.

-type map_fn() :: fun((key(), value()) -> value()).
%% A function for mapping a splay tree to another one.

-type pred_fn() :: fun((key(), value()) -> boolean()).
%% A predicate function that returns `true'
%% if the input entry (key and value) satisfies the expected condition.

-type fold_fn() :: fun((key(), value(), AccIn :: term()) -> AccOut :: term()).
%% A function that folds the entries in a splay tree.

-type fold_while_fn() :: fun ((key(), value(), AccIn :: term()) ->
                                     {Continue :: boolean(), AccOut :: term()}).
%% A function that folds the entries in a splay tree.
%%
%% If the value of `Continue' is `true', the folding will be broken and `AccOut' will be returned as the resulting value.

%%--------------------------------------------------------------------------------
%% Internal Types
%%--------------------------------------------------------------------------------
-type size()            :: non_neg_integer().
-type index()           :: pos_integer().

-type maybe_tree_node() :: tree_node() | nil.
-type tree_node()       :: {key(), value(), maybe_tree_node(), maybe_tree_node(), size() | -1}.
-type direction()       :: lft | rgt. % left | right

%%--------------------------------------------------------------------------------
%% Exported Functions
%%--------------------------------------------------------------------------------
%% @doc Makes an empty tree.
%%
%% == Example ==
%%
%% ```
%% Tree = splay_tree:new().
%% true = splay_tree:is_empty(Tree).
%% '''
-spec new() -> tree().
new() -> nil.

%% @doc Returns the number of entries in the tree.
%%
%% == Example ==
%%
%% ```
%% Tree0 = splay_tree:new().
%% 0 = splay_tree:size(Tree0).
%%
%% Tree1 = splay_tree:store(foo, bar, Tree0).
%% 1 = splay_tree:size(Tree1).
%% '''
-spec size(tree()) -> non_neg_integer().
size(Tree) -> {Size, _} = update_size(Tree), Size.

%% @doc Returns the number of entries in the tree, and updated tree optimised for the size query.
%% If you are using size/1 function frequently in your code
%% it is better to use update_size/1 instead as consecutive calls
%% to this function are match faster.
%%
%% == Example ==
%%
%% ```
%% Tree0 = splay_tree:from_list([{a,1},{b,2},{c,3}]).
%% {3, Tree1} = splay_tree:update_size(Tree0).
%% {3, Tree1} = splay_tree:update_size(Tree1).
%% '''
-spec update_size(tree()) -> {size(), tree()}.
update_size(nil) -> {0, nil};
update_size({_, _, _, _, Size} = Node) when Size >= 0 -> {Size, Node};
update_size({K, V, Lft, Rgt, -1}) ->
    {LftSize, UpdLft} = update_size(Lft),
    {RgtSize, UpdRgt} = update_size(Rgt),
    UpdSize = LftSize + RgtSize + 1,
    {UpdSize, {K, V, UpdLft, UpdRgt, UpdSize}}.

%% @doc Returns `true' if the tree is empty, otherwise `false'.
%%
%% == Example ==
%%
%% ```
%% Tree = splay_tree:new().
%% true = splay_tree:is_empty(Tree).
%% '''
-spec is_empty(tree()) -> boolean().
is_empty(nil) -> true;
is_empty(_)   -> false.

%% @doc Stores the entry in `Tree'.
%%
%% If there is an entry whose key is equal to `Key', its value will be replaced by `Value'.
%%
%% == Example ==
%%
%% ```
%% Tree0 = splay_tree:new().
%% Tree1 = splay_tree:store(foo, bar, Tree0).
%% Tree2 = splay_tree:store(111, 222, Tree1).
%%
%% [{111, 222}, {foo, bar}] = splay_tree:to_list(Tree2).
%% '''
-spec store(key(), value(), tree()) -> tree().
store(Key, Value, Tree) ->
    case path_to_node(Key, Tree) of
        {nil,  Path} -> splay(leaf(Key,Value), Path);
        {Node, Path} -> splay(val(Node,Value), Path)
    end.

%% @doc Updates the value of an entry in the tree.
%%
%% If there is an entry whose key is equal to `Key',
%% its value will be updated to `Fun(Key, CurrentValue)'.
%% Otherwise a new entry whose value is `Initial' will be inserted to the tree.
%%
%% == Example ==
%%
%% ```
%% Tree0 = splay_tree:from_list([{foo, bar}]).
%%
%% %% `foo' exists.
%% Tree1 = splay_tree:update(foo, fun (bar) -> baz end, qux, Tree0).
%% {{ok, baz}, _} = splay_tree:find(foo, Tree1).
%%
%% %% `111' does not exist.
%% Tree2 = splay_tree:update(111, fun (_) -> 222 end, 333, Tree1).
%% {{ok, 333}, _} = splay_tree:find(111, Tree2).
%% '''
-spec update(key(), update_fn(), value(), tree()) -> tree().
update(Key, Fun, Initial, Tree) ->
    case path_to_node(Key, Tree) of
        {nil,  Path} -> splay(leaf(Key,Initial), Path);
        {Node, Path} -> splay(val(Node,Fun(val(Node))), Path)
    end.

%% @doc Updates the value of an entry in the tree.
%%
%% If there is an entry whose key is equal to `Key',
%% its value will be updated to `Fun(Key, CurrentValue)'.
%% Otherwise this function will return `error'.
%%
%% == Example ==
%%
%% ```
%% Tree0 = splay_tree:from_list([{foo, bar}]).
%%
%% %% `foo' exists.
%% {ok, Tree1} = splay_tree:update(foo, fun (bar) -> baz end, Tree0).
%% {{ok, baz}, _} = splay_tree:find(foo, Tree1).
%%
%% %% `111' does not exist.
%% error = splay_tree:update(111, fun (_) -> 222 end, Tree1).
%% '''
-spec update(key(), update_fn(), tree()) -> {ok, tree()} | error.
update(Key, Fun, Tree) ->
    case path_to_node(Key, Tree) of
        {nil, _Path} -> error;
        {Node, Path} -> {ok, splay(val(Node,Fun(val(Node))), Path)}
    end.

%% @doc Finds the value of the entry whose key is equal to `Key' in the tree.
%%
%% Because splay tree is an amortized data structure,
%% this function partially rebalance `Tree' and returns the updated tree.
%%
%% == Example ==
%%
%% ```
%% Tree = splay_tree:from_list([{foo, bar}]).
%%
%% {{ok, bar}, _} = splay_tree:find(foo, Tree).
%% {error, _} = splay_tree:find(baz, Tree).
%% '''
-spec find(key(), tree()) -> {error, tree()} | {{ok, value()}, tree()}.
find(Key, Tree) ->
    case path_to_node(Key, Tree) of
        {nil,  Path} -> {error, splay(Path)};
        {Node, Path} -> {{ok,val(Node)}, splay(Node,Path)}
    end.

%% @doc Finds the entry which has the largest key in the tree.
%%
%% If `Tree' is empty, `{error, Tree}' will be returned.
%%
%% Because splay tree is an amortized data structure,
%% this function partially rebalance `Tree' and returns the updated tree.
%%
%% == Example ==
%%
%% ```
%% Tree = splay_tree:from_list([{333, 444}, {111, 222}]).
%% {{ok, 333, 444}, _} = splay_tree:find_largest(Tree).
%% '''
-spec find_largest(tree()) -> {error, tree()} | {{ok, key(), value()}, tree()}.
find_largest(Tree) ->
    case move_largest_node_to_front(Tree) of
        nil  -> {error, nil};
        Node -> {{ok, key(Node), val(Node)}, Node}
    end.

%% @doc Finds the entry which has the smallest key in the tree.
%%
%% If `Tree' is empty, `{error, Tree}' will be returned.
%%
%% Because splay tree is an amortized data structure,
%% this function partially rebalance `Tree' and returns the updated tree.
%%
%% == Example ==
%%
%% ```
%% Tree = splay_tree:from_list([{333, 444}, {111, 222}]).
%% {{ok, 111, 222}, _} = splay_tree:find_smallest(Tree).
%% '''
-spec find_smallest(tree()) -> {error, tree()} | {{ok, key(), value()}, tree()}.
find_smallest(Tree) ->
    case move_smallest_node_to_front(Tree) of
        nil  -> {error, nil};
        Node -> {{ok, key(Node), val(Node)}, Node}
    end.

%% @doc Takes the entry which has the largest key out from the tree.
%%
%% If `Tree' is empty, `{error, Tree}' will be returned.
%%
%% == Example ==
%%
%% ```
%% Tree0 = splay_tree:from_list([{333, 444}, {111, 222}]).
%% {{ok, 333, 444}, Tree1} = splay_tree:take_largest(Tree0).
%% {{ok, 111, 222}, Tree2} = splay_tree:take_largest(Tree1).
%% {error,          Tree2} = splay_tree:take_largest(Tree2).
%% '''
-spec take_largest(tree()) -> {error, tree()} | {{ok, key(), value()}, tree()}.
take_largest(Tree) ->
    case move_largest_node_to_front(Tree) of
        nil  -> {error, nil};
        Node -> {{ok, key(Node), val(Node)}, lft(Node)}
    end.

%% @doc Takes the entry which has the smallest key out from the tree.
%%
%% If `Tree' is empty, `{error, Tree}' will be returned.
%%
%% == Example ==
%%
%% ```
%% Tree0 = splay_tree:from_list([{333, 444}, {111, 222}]).
%% {{ok, 111, 222}, Tree1} = splay_tree:take_smallest(Tree0).
%% {{ok, 333, 444}, Tree2} = splay_tree:take_smallest(Tree1).
%% {error,          Tree2} = splay_tree:take_smallest(Tree2).
%% '''
-spec take_smallest(tree()) -> {error, tree()} | {{ok, key(), value()}, tree()}.
take_smallest(Tree) ->
    case move_smallest_node_to_front(Tree) of
        nil  -> {error, nil};
        Node -> {{ok, key(Node), val(Node)}, rgt(Node)}
    end.

%% @doc Lookups the value of the entry whose key is equal to `Key' in the tree.
%%
%% == Caution ==
%%
%% Unlike {@link find/2}, this function does not rebalance `Tree'.
%% So use of this function may cause performance degradation.
%%
%% == Example ==
%%
%% ```
%% Tree = splay_tree:from_list([{foo, bar}]).
%%
%% {ok, bar} = splay_tree:lookup(foo, Tree).
%% error = splay_tree:lookup(baz, Tree).
%% '''
-spec lookup(key(), tree()) -> error | {ok, value()}.
lookup(Key, Tree) ->
    case lookup_node(Key, Tree) of
        nil  -> error;
        Node -> {ok, val(Node)}
    end.

%% @doc Gets the value of the entry whose key is equal to `Key' in the tree.
%%
%% If there is no entry which has the key,
%% this function will return `DefaultValue' instead.
%%
%% == Caution ==
%%
%% Unlike {@link find/2}, this function does not rebalance `Tree'.
%% So use of this function may cause performance degradation.
%%
%% == Example ==
%%
%% ```
%% Tree = splay_tree:from_list([{foo, bar}]).
%%
%% bar = splay_tree:get_value(foo, Tree, qux).
%% qux = splay_tree:get_value(baz, Tree, qux).
%% '''
-spec get_value(key(), tree(), value()) -> value().
get_value(Key, Tree, DefaultValue) ->
    case lookup_node(Key, Tree) of
        nil  -> DefaultValue;
        Node -> val(Node)
    end.

%% @doc Erases the entry whose key is equal to `Key' from the tree.
%%
%% == Example ==
%%
%% ```
%% Tree0 = splay_tree:from_list([{foo, bar}]).
%%
%% Tree1 = splay_tree:erase(foo, Tree0).
%% error = splay_tree:lookup(foo, Tree1).
%%
%% Tree1 = splay_tree:erase(foo, Tree1).
%% '''
-spec erase(key(), tree()) -> tree().
erase(Key, Tree) ->
    case path_to_node(Key, Tree) of
        {nil,  Path} -> splay(Path);
        {Node,   []} -> pop_front(Node);
        {Node, Path} -> case {pop_front(Node), hd(Path)} of
                            {C, {lft,P}} -> splay(lft(P, C), tl(Path));
                            {C, {rgt,P}} -> splay(rgt(P, C), tl(Path))
                        end
    end.

%% @doc Splits `Tree' at the position specified by `BorderKey'.
%%
%% `LeftTree' contains the entries whose key is smaller than `BorderKey'.
%% `RightTree' contains the entries whose key is equal to or greater than `BorderKey'.
%%
%% == Example ==
%%
%% ```
%% Tree = splay_tree:from_list([{1, a}, {2, b}, {3, c}]).
%% {Left, Right} = splay_tree:split(2, Tree).
%%
%% [1] = splay_tree:keys(Left).
%% [2, 3] = splay_tree:keys(Right).
%% '''
-spec split(key(), tree()) -> {LeftTree :: tree(), RightTree :: tree()}.
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

%% @doc Finds the smallest entry among those whose key is equal to or greater than `Key'.
%%
%% Because splay tree is an amortized data structure,
%% this function partially rebalance `Tree' and returns the updated tree.
%%
%% == Example ==
%%
%% ```
%% Tree = splay_tree:from_list([{1, a}, {2, b}, {3, c}]).
%% {{ok, 2, b}, _} = splay_tree:find_lower_bound(2, Tree).
%% {{ok, 3, c}, _} = splay_tree:find_lower_bound(2.5, Tree).
%% {error, _}      = splay_tree:find_lower_bound(3.1, Tree).
%% '''
-spec find_lower_bound(key(), tree()) -> {error, tree()} | {{ok, key(), value()}, tree()}.
find_lower_bound(Key, Tree) ->
    {Left, Right} = split(Key, Tree),
    case Right of
        nil            -> {error, Left};
        {K, V, nil, _, _} -> {{ok, K, V}, lft(Right, Left)}
    end.

%% @doc Finds the smallest entry among those whose key is greater than `Key'.
%%
%% Because splay tree is an amortized data structure,
%% this function partially rebalance `Tree' and returns the updated tree.
%%
%% == Example ==
%%
%% ```
%% Tree = splay_tree:from_list([{1, a}, {2, b}, {3, c}]).
%% {{ok, 3, c}, _} = splay_tree:find_upper_bound(2, Tree).
%% {{ok, 3, c}, _} = splay_tree:find_upper_bound(2.5, Tree).
%% {error, _}      = splay_tree:find_upper_bound(3.1, Tree).
%% '''
-spec find_upper_bound(key(), tree()) -> {error, tree()} | {{ok, key(), value()}, tree()}.
find_upper_bound(Key, Tree) ->
    {Left, Right} = split(Key, Tree),
    case Right of
        nil                       -> {error, Left};
        {Key, Value, nil, Right2, _} ->
            Left2 = store(Key, Value, Left),
            case find_smallest(Right2) of
                {error, _}   -> {error, Left2};
                {Ok, Right3} -> {Ok, lft(Right3, Left2)}
            end;
        {K, V, nil, _, _} -> {{ok, K, V}, lft(Right, Left)}
    end.

%% @doc Converts `Tree` to an associated list.
%%
%% The resulting list is ordered by the key of the entries.
%%
%% == Example ==
%%
%% ```
%% Tree = splay_tree:from_list([{2, b}, {3, c}, {1, a}]).
%% [{1, a}, {2, b}, {3, c}] = splay_tree:to_list(Tree).
%% '''
-spec to_list(tree()) -> [{key(), value()}].
to_list(Tree) -> foldr(fun (K, V, Acc) -> [{K,V}|Acc] end, [], Tree).

%% @doc Returns the keys of the entries in `Tree'.
%%
%% The resulting list is in ascending order.
%%
%% == Example ==
%%
%% ```
%% Tree = splay_tree:from_list([{2, b}, {3, c}, {1, a}]).
%% [1, 2, 3] = splay_tree:keys(Tree).
%% '''
-spec keys(tree()) -> [key()].
keys(Tree) -> foldr(fun (K, _, Acc) -> [K|Acc] end, [], Tree).

%% @doc Returns the values of the entries in `Tree'.
%%
%% The resulting values are ordered by the associated keys.
%%
%% == Example ==
%%
%% ```
%% Tree = splay_tree:from_list([{2, a}, {3, b}, {1, c}]).
%% [c, a, b] = splay_tree:values(Tree).
%% '''
-spec values(tree()) -> [value()].
values(Tree) -> foldr(fun (_, V, Acc) -> [V|Acc] end, [], Tree).

%% @doc Makes a splay tree from the given associated list.
%%
%% == Example ==
%%
%% ```
%% Tree = splay_tree:from_list([{2, b}, {1, a}]).
%% {{ok, a}, _} = splay_tree:find(1, Tree).
%% '''
-spec from_list([{key(), value()}]) -> tree().
from_list(List) -> lists:foldl(fun ({K, V}, Tree) -> store(K, V, Tree) end, new(), List).

%% @doc Maps `Tree' to another splay tree.
%%
%% == Example ==
%%
%% ```
%% Tree0 = splay_tree:from_list([{1, 2}, {3, 4}]).
%% Tree1 = splay_tree:map(fun (K, V) -> K + V end, Tree0).
%% [{1, 3}, {3, 7}] = splay_tree:to_list(Tree1).
%% '''
-spec map(map_fn(), tree()) -> tree().
map(Fun, Tree) -> map_node(Fun, Tree).

%% @doc Folds the entries in `Tree' by ascending order.
%%
%% == Example ==
%%
%% ```
%% Tree = splay_tree:from_list([{a, 1}, {b, 2}]).
%% [2, 1] = splay_tree:foldl(fun (_, V, Acc) -> [V | Acc] end, [], Tree).
%% '''
-spec foldl(fold_fn(), term(), tree()) -> Result :: term().
foldl(Fun, Initial, Tree) -> foldl_node(Fun, Tree, Initial).

%% @doc Folds the entries in `Tree' by descending order.
%%
%% == Example ==
%%
%% ```
%% Tree = splay_tree:from_list([{a, 1}, {b, 2}]).
%% [1, 2] = splay_tree:foldr(fun (_, V, Acc) -> [V | Acc] end, [], Tree).
%% '''
-spec foldr(fold_fn(), term(), tree()) -> Result :: term().
foldr(Fun, Initial, Tree) -> foldr_node(Fun, Tree, Initial).

%% @doc Folds the entries in `Tree' by ascending order.
%%
%% If `Fun' returns `{false, Result}', the folding will be broken immediately and
%% `Result` will be returned as the resulting value.
%%
%% == Example ==
%%
%% ```
%% Tree = splay_tree:from_list([{a, 1}, {b, 2}]).
%% [1] = splay_tree:foldl_while(fun (_, V, Acc) -> {false, [V | Acc]} end, [], Tree).
%% '''
-spec foldl_while(fold_while_fn(), term(), tree()) -> Result :: term().
foldl_while(Fun, Initial, Tree) ->
    try
        foldl_while_node(Fun, Tree, Initial)
    catch
        throw:{?MODULE, break, AccFinal} -> AccFinal
    end.

%% @doc Folds the entries in `Tree' by descending order.
%%
%% If `Fun' returns `{false, Result}', the folding will be broken immediately and
%% `Result` will be returned as the resulting value.
%%
%% == Example ==
%%
%% ```
%% Tree = splay_tree:from_list([{a, 1}, {b, 2}]).
%% [2] = splay_tree:foldr_while(fun (_, V, Acc) -> {false, [V | Acc]} end, [], Tree).
%% '''
-spec foldr_while(fold_while_fn(), term(), tree()) -> term().
foldr_while(Fun, Initial, Tree) ->
    try
        foldr_while_node(Fun, Tree, Initial)
    catch
        throw:{?MODULE, break, AccFinal} -> AccFinal
    end.

%% @doc Makes a splay tree that contains entries in `Tree' for which the invocation of `Pred' returns `true'.
%%
%% == Example ==
%%
%% ```
%% Tree0 = splay_tree:from_list([{aaa, bbb}, {111, 222}]).
%% Tree1 = splay_tree:filter(fun (K, _) -> is_atom(K) end, Tree0).
%% [{aaa, bbb}] = splay_tree:to_list(Tree1).
%% '''
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

%% @doc Finds the index of the entry whose key is equal to `Key' in the tree.
%% The index of the entry is its position in ordered list of all tree entries.
%%
%% Because splay tree is an amortized data structure,
%% this function partially rebalance `Tree' and returns the updated tree.
%%
%% == Example ==
%%
%% ```
%% Tree = splay_tree:from_list([{a, p}, {b, q}, {c, r}, {d, s}, {e, t} ]).
%%
%% {{ok, 4}, _} = splay_tree:index(d, Tree).
%% {error, _} = splay_tree:index(baz, Tree).
%% '''
-spec index(key(), tree()) -> {{ok, index()}, tree()} | {error, tree()}.
index(Key, Tree) ->
    case find(Key, Tree) of
        {error, _} = Error -> Error;
        {{ok, _}, Tree2} -> 
            {_, Tree3} = update_size(Tree2),
            {{ok, node_index(Tree3)}, Tree3}
    end.

%% @doc Finds the entry with given index in the tree.
%% The index of the entry is its position in ordered list of all tree entries.
%%
%% Because splay tree is an amortized data structure,
%% this function partially rebalance `Tree' and returns the updated tree.
%%
%% == Example ==
%%
%% ```
%% Tree = splay_tree:from_list([{a, p}, {b, q}, {c, r}, {d, s}, {e, t} ]).
%%
%% {{ok, c, r}, _} = splay_tree:at(3, Tree).
%% {error, _} = splay_tree:at(baz, Tree).
%% '''
-spec at(index(), tree()) -> {{ok, key(), value()}, tree()} | {error, tree()}.
at(Index, Tree) ->
    {_, Tree1} = update_size(Tree),
    case path_to_index(Index, Tree1) of
        {nil,  Path} -> {error, splay(Path)};
        {Node, Path} -> {{ok, key(Node), val(Node)}, splay(Node, Path)}
    end.

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
lft({_, _, Lft, _, _}) -> Lft.

-spec lft(tree_node(), maybe_tree_node()) -> tree_node().
lft({Key, Val, _, nil, _}, nil) -> {Key, Val, nil, nil, 1};
lft({Key, Val, _, Rgt, _}, Lft) -> {Key, Val, Lft, Rgt, -1}.

-spec rgt(tree_node()) -> maybe_tree_node().
rgt({_, _, _, Rgt, _}) -> Rgt.

-spec rgt(tree_node(), maybe_tree_node()) -> tree_node().
rgt({Key, Val, nil, _, _}, nil) -> {Key, Val, nil, nil, 1};
rgt({Key, Val, Lft, _, _}, Rgt) -> {Key, Val, Lft, Rgt, -1}.

-spec lft_rgt(tree_node(), maybe_tree_node(), maybe_tree_node()) -> tree_node().
lft_rgt(Node, Lft, Rgt) -> {key(Node), val(Node), Lft, Rgt, -1}.

-spec rgt_lft(tree_node(), maybe_tree_node(), maybe_tree_node()) -> tree_node().
rgt_lft(Node, Rgt, Lft) -> {key(Node), val(Node), Lft, Rgt, -1}.

-spec leaf(key(), value()) -> tree_node().
leaf(Key, Value) -> {Key, Value, nil, nil, 1}.

-spec pop_front(tree_node()) -> maybe_tree_node().
pop_front(Node) ->
    case move_largest_node_to_front(lft(Node)) of
        nil   -> rgt(Node);
        Front -> rgt(Front, rgt(Node))
    end.

-spec move_largest_node_to_front(maybe_tree_node()) -> maybe_tree_node().
move_largest_node_to_front(nil)  -> nil;
move_largest_node_to_front(Node) -> move_largest_node_to_front(Node, []).

-spec move_largest_node_to_front(tree_node(), [{direction(), tree_node()}]) -> tree_node().
move_largest_node_to_front(Node, Path) ->
    case rgt(Node) of
        nil -> splay(Node, Path);
        Rgt -> move_largest_node_to_front(Rgt, [{rgt, Node}|Path])
    end.

-spec move_smallest_node_to_front(maybe_tree_node()) -> maybe_tree_node().
move_smallest_node_to_front(nil)  -> nil;
move_smallest_node_to_front(Node) -> move_smallest_node_to_front(Node, []).

-spec move_smallest_node_to_front(tree_node(), [{direction(), tree_node()}]) -> tree_node().
move_smallest_node_to_front(Node, Path) ->
    case lft(Node) of
        nil -> splay(Node, Path);
        Lft -> move_smallest_node_to_front(Lft, [{lft, Node}|Path])
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
map_node(Fun, {Key, Val, Lft, Rgt, Size}) -> {Key, Fun(Key, Val), map_node(Fun, Lft), map_node(Fun, Rgt), Size}.

-spec foldl_node(fold_fn(), maybe_tree_node(), term()) -> term().
foldl_node(_Fun, nil, Acc)                 -> Acc;
foldl_node(Fun, {Key, Val, Lft, Rgt, _}, Acc) -> foldl_node(Fun, Rgt, Fun(Key, Val, foldl_node(Fun, Lft, Acc))).

-spec foldr_node(fold_fn(), maybe_tree_node(), term()) -> term().
foldr_node(_Fun, nil, Acc)                 -> Acc;
foldr_node(Fun, {Key, Val, Lft, Rgt, _}, Acc) -> foldr_node(Fun, Lft, Fun(Key, Val, foldr_node(Fun, Rgt, Acc))).

-define(MAYBE_BREAK(Result),
        case Result of
            {false, Value} -> throw({?MODULE, break, Value});
            {true,  Value} -> Value
        end).

-spec foldl_while_node(fold_while_fn(), maybe_tree_node(), term()) -> term().
foldl_while_node(_Fun, nil, Acc)                 -> Acc;
foldl_while_node(Fun, {Key, Val, Lft, Rgt, _}, Acc) -> foldl_while_node(Fun, Rgt, ?MAYBE_BREAK(Fun(Key, Val, foldl_while_node(Fun, Lft, Acc)))).

-spec foldr_while_node(fold_while_fn(), maybe_tree_node(), term()) -> term().
foldr_while_node(_Fun, nil, Acc)                 -> Acc;
foldr_while_node(Fun, {Key, Val, Lft, Rgt, _}, Acc) -> foldr_while_node(Fun, Lft, ?MAYBE_BREAK(Fun(Key, Val, foldr_while_node(Fun, Rgt, Acc)))).

-spec node_index(tree_node()) -> index().
node_index(Node) -> 1 + size(lft(Node)).

-spec path_to_index(index(), maybe_tree_node()) -> {maybe_tree_node(), [{direction(),tree_node()}]}.
path_to_index(Index, Root) ->
    path_to_index(Index, Root, []).

-spec path_to_index(index(), maybe_tree_node(), [{direction(),tree_node()}]) ->
    {maybe_tree_node(), [{direction(),tree_node()}]}.
path_to_index(_Index, nil, Path) -> {nil, Path};
path_to_index(Index, Node, Path) ->
    case size(lft(Node)) of
        I when Index =< I -> path_to_index(Index, lft(Node), [{lft,Node}|Path]);
        I when Index > I + 1 -> path_to_index(Index - I - 1, rgt(Node), [{rgt,Node}|Path]);
        _              -> {Node, Path}
    end.

