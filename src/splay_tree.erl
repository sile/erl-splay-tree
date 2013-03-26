-module(splay_tree).

-compile(inline).

-export([new/0, store/3, find/2, lookup/2, get_value/3, erase/2, 
         size/1, update/4, update/3, filter/2, map/2,
         fold/3, from_list/1, to_list/1]).

-export_type([tree/0, key/0, value/0, 
              update_fn/0, map_fn/0, fold_fn/0, pred_fn/0]).


%%% records
-record(node, {key     :: key(),
               val     :: value(),
               lft=nil :: maybe_tree_node(),
               rgt=nil :: maybe_tree_node()}).

%%% types
-type tree()    :: maybe_tree_node().
-type key()       :: any().
-type value()     :: any().
-type update_fn() :: fun((value()) -> value()).
-type map_fn()    :: fun((key(),value()) -> value()).
-type fold_fn()   :: fun((key(),value(),term()) -> term()).
-type pred_fn()   :: fun((key(),value()) -> boolean()).

-type maybe_tree_node() :: tree_node()|nil.
-type tree_node()       :: #node{}.
-type direction()       :: lft|rgt.

%%% exported functions
-spec new() -> tree().
new() -> nil.

-spec size(tree()) -> non_neg_integer().
size(Tree) -> fold(fun (_, _, Count) -> Count+1 end, 0, Tree).
                 
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
        {Node, Path} -> splay(val(Node,Fun(Node#node.val)), Path)
    end.

-spec update(key(), update_fn(), tree()) -> tree()|error.
update(Key, Fun, Root) ->
    case path_to_node(Key, Root) of
        {nil, _Path} -> error;
        {Node, Path} -> splay(val(Node,Fun(Node#node.val)), Path)
    end.

-spec find(key(), tree()) -> {error,tree()} | {{ok,value()},tree()}.
find(Key, Root) ->
    case path_to_node(Key,Root) of
        {nil,  Path} -> {error,              splay(Path)};
        {Node, Path} -> {{ok,Node#node.val}, splay(Node,Path)}
    end.

-spec lookup(key(), tree()) -> error | {ok,value()}.
lookup(Key, Root) ->
    case lookup_node(Key,Root) of
        nil  -> error;
        Node -> {ok, Node#node.val}
    end.

-spec get_value(key(), tree(), value()) -> value().
get_value(Key, Root, DefaultValue) ->
    case lookup_node(Key,Root) of
        nil  -> DefaultValue;
        Node -> Node#node.val
    end.

-spec erase(key(), tree()) -> tree().
erase(Key, Root) ->
    case path_to_node(Key, Root) of
        {nil,  Path} -> splay(Path);
        {Node,   []} -> pop_front(Node);
        {Node, Path} -> case {pop_front(Node), hd(Path)} of
                            {C, {lft,P}} -> splay(P#node{lft=C}, tl(Path));
                            {C, {rgt,P}} -> splay(P#node{rgt=C}, tl(Path))
                        end
    end.

-spec to_list(tree()) -> [{key(),value()}].
to_list(Tree) -> lists:reverse(fold(fun (K, V, Acc) -> [{K,V}|Acc] end, [], Tree)).

-spec from_list([{key(),value()}]) -> tree().
from_list(List) -> lists:foldl(fun ({K, V}, Tree) -> store(K, V, Tree) end, new(), List).

-spec map(map_fn(), tree()) -> tree().
map(Fun, Tree) -> map_node(Fun, Tree).

-spec fold(fold_fn(), term(), tree()) -> term().
fold(Fun, Acc0, Tree) -> fold_node(Fun, Tree, Acc0).

-spec filter(pred_fn(), tree()) -> tree().
filter(Pred, Tree) -> filter_node(Pred, Tree).

%%% auxiliary functions
-spec leaf(key(), value()) -> tree_node().
leaf(Key, Value) -> #node{key=Key, val=Value}.

-spec val(tree_node(), value()) -> tree_node().
val(Node, Value) -> Node#node{val=Value}.

-spec pop_front(tree_node()) -> maybe_tree_node().
pop_front(Node) ->
    case move_largest_node_to_front(Node#node.lft) of
        nil   -> Node#node.rgt;
        Front -> Front#node{rgt=Node#node.rgt}
    end.

-spec move_largest_node_to_front(maybe_tree_node()) -> maybe_tree_node().
move_largest_node_to_front(nil) ->
    nil;
move_largest_node_to_front(Node) ->
    move_largest_node_to_front(Node, []).

-spec move_largest_node_to_front(tree_node(), [tree_node()]) -> tree_node().
move_largest_node_to_front(Node=#node{rgt=nil}, Path) ->
    Front = lists:foldl(fun (N, Front) -> N#node{rgt=Front} end,
                        Node#node.lft,
                        Path),
    Node#node{lft=Front};
move_largest_node_to_front(Node, Path) ->
    move_largest_node_to_front(Node#node.rgt, [Node|Path]).

-spec path_to_node(key(), maybe_tree_node()) -> {maybe_tree_node(), [{direction(),tree_node()}]}.
path_to_node(Key, Root) -> 
    path_to_node(Key, Root, []).

-spec path_to_node(key(), maybe_tree_node(), [{direction(),tree_node()}]) ->
                          {maybe_tree_node(), [{direction(),tree_node()}]}.
path_to_node(Key, Node, Path) ->
    case Node of
        nil                                     -> {nil,  Path};
        #node{lft=Lft} when Key < Node#node.key -> path_to_node(Key, Lft, [{lft,Node}|Path]);
        #node{rgt=Rgt} when Key > Node#node.key -> path_to_node(Key, Rgt, [{rgt,Node}|Path]);
        #node{key=Key}                          -> {Node, Path}
    end.

-spec lookup_node(key(), maybe_tree_node()) -> maybe_tree_node().
lookup_node(Key, Node) ->
    case Node of
        nil                                     -> nil;
        #node{lft=Lft} when Key < Node#node.key -> lookup_node(Key, Lft);
        #node{rgt=Rgt} when Key > Node#node.key -> lookup_node(Key, Rgt);
        #node{key=Key}                          -> Node
    end.

-spec splay([{direction(),tree_node()}]) -> maybe_tree_node().
splay([])              -> nil;
splay([{_,Node}|Path]) -> splay(Node, Path).

-spec splay(tree_node(), [{direction(),tree_node()}]) -> tree_node().
splay(X, []) -> X;
splay(X, [{Dir, P}]) ->                % zig
    case Dir of
        lft -> X#node{rgt=P#node{lft=X#node.rgt}};
        rgt -> X#node{lft=P#node{rgt=X#node.lft}}
    end;    
splay(X, [{Dir,P}, {Dir,G} | Path]) -> % zig-zig
    splay(case Dir of
              lft -> X#node{rgt=P#node{rgt=G#node{lft=P#node.rgt}, lft=X#node.rgt}};
              rgt -> X#node{lft=P#node{lft=G#node{rgt=P#node.lft}, rgt=X#node.lft}}
          end,
          Path);
splay(X, [{Dir,P}, {_,G} | Path]) ->   % zig-zag
    splay(case Dir of
              lft -> X#node{rgt=P#node{lft=X#node.rgt}, lft=G#node{rgt=X#node.lft}};
              rgt -> X#node{lft=P#node{rgt=X#node.lft}, rgt=G#node{lft=X#node.rgt}}
          end,
          Path).

-spec fold_node(fold_fn(), maybe_tree_node(), term()) -> term().
fold_node(_, nil, Acc) ->
    Acc;
fold_node(Fun, #node{key=Key, val=Value, lft=Lft, rgt=Rgt}, Acc) ->
    fold_node(Fun, Rgt, Fun(Key, Value, fold_node(Fun, Lft, Acc))).

-spec map_node(map_fn(), maybe_tree_node()) -> maybe_tree_node().
map_node(_, nil) ->
    nil;
map_node(Fun, #node{key=Key, val=Value, lft=Lft, rgt=Rgt}) ->
    #node{key=Key, val=Fun(Key,Value), lft=map_node(Fun,Lft), rgt=map_node(Fun,Rgt)}.

-spec filter_node(pred_fn(), maybe_tree_node()) -> maybe_tree_node().
filter_node(_, nil) ->
    nil;
filter_node(Pred, Node=#node{key=Key, val=Value, lft=Lft, rgt=Rgt}) ->
    case Pred(Key, Value) of
        true  -> Node#node{lft=filter_node(Pred, Lft), rgt=filter_node(Pred, Rgt)};
        false -> filter_node(Pred, pop_front(Node))
    end.
