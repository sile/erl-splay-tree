-module(splay_tree).

-export([new/0, store/3, find/2, erase/2, 
         update/4, filter/2,
         fold/3, from_list/1, is_key/2, to_list/1,
         size/1, map/2]).

-compile({inline, [leaf/2, val/2, pop_front/1, splay/1]}).

-record(node, 
        {
          key,
          val,
          lft=nil,
          rgt=nil
        }).

-record(tree,
        {
          root=nil,
          size=0
        }).

%%%
leaf(Key, Value) ->
    #node{key=Key, val=Value}.

val(Node, Value) ->
    Node#node{val=Value}.

-define(TREE(Size, Root), #tree{size=Size, root=Root}).

%%%
new() -> #tree{}.
size(#tree{size=Size}) -> Size.

store(Key, Value, #tree{root=Root, size=Size}) ->
    case path_to_node(Key, Root) of
        {nil,  Path} -> ?TREE(Size+1, splay(leaf(Key,Value), Path));
        {Node, Path} -> ?TREE(Size,   splay(val(Node,Value), Path))
    end.

update(Key, Fun, Initial, #tree{root=Root, size=Size}) ->
    case path_to_node(Key, Root) of
        {nil,  Path} -> ?TREE(Size+1, splay(leaf(Key,Initial),            Path));
        {Node, Path} -> ?TREE(Size,   splay(val(Node,Fun(Node#node.val)), Path))
    end.

find(Key, Tree=#tree{root=Root}) ->
    case path_to_node(Key,Root) of
        {nil,  Path} -> {error,              Tree#tree{root=splay(Path)}};
        {Node, Path} -> {{ok,Node#node.val}, Tree#tree{root=splay(Node,Path)}}
    end.

is_key(Key, Tree) ->
    case find(Key, Tree) of
        {error, NewTree} -> {false, NewTree};
        {_,     NewTree} -> {true,  NewTree}
    end.

erase(Key, #tree{root=Root, size=Size}) ->
    case path_to_node(Key, Root) of
        {nil,  Path} -> ?TREE(Size, splay(Path));
        {Node,   []} -> ?TREE(Size-1, pop_front(Node));
        {Node, Path} -> ?TREE(Size-1, case {pop_front(Node), hd(Path)} of
                                          {C, {lft,P}} -> splay(P#node{lft=C}, tl(Path));
                                          {C, {rgt,P}} -> splay(P#node{rgt=C}, tl(Path))
                                      end)
    end.

pop_front(Node) ->
    case move_largest_node_to_front(Node#node.lft) of
        nil   -> Node#node.rgt;
        Front -> Front#node{rgt=Node#node.rgt}
    end.

move_largest_node_to_front(nil) ->
    nil;
move_largest_node_to_front(Node) ->
    move_largest_node_to_front(Node, []).

move_largest_node_to_front(Node=#node{rgt=nil}, Path) ->
    Front = lists:foldl(fun (N, Front) -> N#node{rgt=Front} end,
                        Node#node.lft,
                        Path),
    Node#node{lft=Front};
move_largest_node_to_front(Node, Path) ->
    move_largest_node_to_front(Node#node.rgt, [Node|Path]).

path_to_node(Key, Root) -> 
    path_to_node(Key, Root, []).

path_to_node(Key, Node, Path) ->
    case Node of
        nil                                     -> {nil,  Path};
        #node{lft=Lft} when Key < Node#node.key -> path_to_node(Key, Lft, [{lft,Node}|Path]);
        #node{rgt=Rgt} when Key > Node#node.key -> path_to_node(Key, Rgt, [{rgt,Node}|Path]);
        #node{key=Key}                          -> {Node, Path}
    end.

splay([])              -> nil;
splay([{_,Node}|Path]) -> splay(Node, Path).

splay(X, []) -> X;
splay(X, [{Dir, P}]) -> % zig
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
splay(X, [{Dir,P}, {_,G} | Path]) -> % zig-zag
    splay(case Dir of
              lft -> X#node{rgt=P#node{lft=X#node.rgt}, lft=G#node{rgt=X#node.lft}};
              rgt -> X#node{lft=P#node{rgt=X#node.lft}, rgt=G#node{lft=X#node.rgt}}
          end,
          Path).

fold(Fun, Acc0, Tree) ->
    fold_node(Fun, Tree#tree.root, Acc0).

fold_node(_, nil, Acc) ->
    Acc;
fold_node(Fun, #node{key=Key, val=Value, lft=Lft, rgt=Rgt}, Acc) ->
    fold_node(Fun, Rgt, Fun(Key, Value, fold_node(Fun, Lft, Acc))).

to_list(Tree) ->
    lists:reverse(fold(fun (K, V, Acc) -> [{K,V}|Acc] end, [], Tree)).

from_list(List) ->
    lists:foldl(fun ({K, V}, Tree) -> store(K, V, Tree) end, new(), List).

map(Fun, Tree) ->
    Tree#tree{root=map_node(Fun, Tree#tree.root)}.

map_node(_, nil) ->
    nil;
map_node(Fun, #node{key=Key, val=Value, lft=Lft, rgt=Rgt}) ->
    #node{key=Key,
          val=Fun(Key,Value),
          lft=map_node(Fun,Lft),
          rgt=map_node(Fun,Rgt)}.

filter(Pred, Tree) ->
    Tree#tree{root=filter_node(Pred, Tree#tree.root)}.

filter_node(_, nil) ->
    nil;
filter_node(Pred, Node=#node{key=Key, val=Value, lft=Lft, rgt=Rgt}) ->
    case Pred(Key, Value) of
        true  -> Node#node{lft=filter_node(Pred, Lft), rgt=filter_node(Pred, Rgt)};
        false -> filter_node(Pred, pop_front(Node))
    end.
