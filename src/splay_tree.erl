-module(splay_tree).

-export([new/0, store/3, find/2, erase/2, 
         update/4, fetch_keys/1, filter/2,
         fold/3, from_list/1, is_key/2, to_list/1,
         size/1, map/2]).

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

set_val(Node, Value) ->
    Node#node{val=Value}.

tree({NewSize, NewRoot}) ->
    #tree{root=NewRoot, size=NewSize}.

%%%
size(Tree) ->
    Tree#tree.size.

new() ->
    #tree{}.

store(Key, Value, #tree{root=nil}) ->
    #tree{size=1, root=leaf(Key,Value)};
store(Key, Value, #tree{root=Root, size=Size}) ->
    tree(case path_to_node(Key, Root) of
             {nil,  Path} -> {Size+1, splay(leaf(Key,Value), Path)};
             {Node, Path} -> {Size,   set_val(splay(Node,Path),Value)}
         end).

update(Key, _, Initial, #tree{root=nil}) ->
    #tree{size=1, root=leaf(Key,Initial)};
update(Key, Fun, Initial, #tree{root=Root, size=Size}) ->
    tree(case path_to_node(Key, Root) of
             {nil,  Path} -> {Size+1, splay(leaf(Key,Initial), Path)};
             {Node, Path} -> {Size,   set_val(splay(Node,Path), Fun(Node#node.val))}
         end).

erase(_, #tree{root=nil}) ->
    #tree{};
erase(Key, #tree{root=Root, size=Size}) ->
    tree(case path_to_node(Key, Root) of
             {nil,  Path} -> {Size, splay(Path)};
             {Node, Path} ->
                 Deleted = case move_largest_node_to_front(Node#node.lft) of
                               nil   -> Node#node.rgt;
                               Front -> Front#node{rgt=Node#node.rgt}
                           end,
                 {Size-1, splay(Deleted, Path)}
         end).

find(_, #tree{root=nil}) ->
    {error, #tree{}};
find(Key, Tree=#tree{root=Root}) ->
    case path_to_node(Key,Root) of
        {nil,  Path} -> {error,              Tree#tree{root=splay(Path)}};
        {Node, Path} -> {{ok,Node#node.val}, Tree#tree{root=splay(Node,Path)}}
    end.

move_largest_node_to_front(nil) ->
    nil;
move_largest_node_to_front(Node) ->
    move_largest_node_to_front(Node, []).

move_largest_node_to_front(Node=#node{rgt=nil}, Path) ->
    Front = lists:foldl(fun (N, Front) -> N#node{rgt=Front} end,
                        nil,
                        Path),
    Node#node{lft=Front};
move_largest_node_to_front(Node=#node{rgt=Rgt}, Path) ->
    move_largest_node_to_front(Rgt, [Node|Path]).

path_to_node(Key, Tree) -> 
    path_to_node(Key, Tree, []).

path_to_node(Key, Node, Path) ->
    case Node of
        nil                                     -> {nil,  Path};
        #node{key=Key}                          -> {Node, Path};
        #node{lft=Lft} when Key < Node#node.key -> path_to_node(Key, Lft, [{lft,Node}|Path]);
        #node{rgt=Rgt}                          -> path_to_node(Key, Rgt, [{rgt,Node}|Path])
    end.

splay([{_,Node}|Path]) ->
    splay(Node, Path).

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

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%    
extract_largest_node(nil) -> %% TODO: tail recursive
    {nil, nil};
extract_largest_node(Node=#node{rgt=nil}) ->
    {Node, nil};
extract_largest_node(Node=#node{rgt=Rgt}) ->
    {Largest, Extracted} = extract_largest_node(Rgt),
    {Largest, Node#node{rgt=Extracted}}.

map_to_list(Fun, Node) ->
    lists:reverse(map_to_list(Fun, Node, [])).

map_to_list(_, nil, Acc) ->
    Acc;
map_to_list(Fun, Node, Acc) ->
    #node{key=Key, val=Value, lft=Left, rgt=Right} = Node,
    Acc2 = [Fun(Key, Value)|Acc],
    Acc3 = map_to_list(Fun, Left, Acc2),
    map_to_list(Fun, Right, Acc3).
    
fetch_keys(Tree) ->
    map_to_list(fun (Key, _) -> Key end,
                Tree#tree.root).

filter(Pred, Tree) ->
    #tree{root=filter_node(Pred, Tree#tree.root)}.

filter_node(_, nil) ->
    nil;
filter_node(Pred, Node) ->
    #node{key=Key, val=Value, lft=Left, rgt=Right} = Node,
    case Pred(Key, Value) of
        true  -> 
            Node#node{lft = filter_node(Pred, Left),
                      rgt= filter_node(Pred, Right)};
        false -> 
            case filter_node(Pred, Left) of
                nil ->
                    filter_node(Pred, Right);
                Left2 ->
                    {Node2, Left3} = extract_largest_node(Left2),
                    Node2#node{lft=Left3, rgt=filter_node(Pred, Right)}
            end
    end.

fold(Fun, Acc0, Tree) ->
    fold_node(Fun, Acc0, Tree#tree.root).

map(Fun, Tree) ->
    map_node(Fun, Tree#tree.root).

map_node(_, nil) ->
    nil;
map_node(Fun, Node) ->
    #node{key=Key, val=Value, lft=Left, rgt=Right} = Node,
    Node#node{val=Fun(Key,Value), 
              lft=map_node(Fun,Left),
              rgt=map_node(Fun,Right)}.

from_list(List) ->
    lists:foldl(fun ({K, V}, Tree) ->
                        store(K, V, Tree)
                end,
                splay_tree:new(),
                List).

to_list(Tree) ->
    lists:reverse(
      fold(fun (K, V, Acc) ->
                   [{K,V}|Acc]
           end,
           [],
           Tree)).

fold_node(_, Acc, nil) ->
    Acc;
fold_node(Fun, Acc, Node) ->
    #node{key=Key, val=Value, lft=Left, rgt=Right} = Node,
    Acc2 = Fun(Key, Value, Acc),
    Acc3 = fold_node(Fun, Acc2, Left),
    fold_node(Fun, Acc3, Right).

is_key(Key, Tree) ->
    case find(Key, Tree) of
        {error, NewTree} -> {false, NewTree};
        {_, NewTree} -> {true, NewTree}
    end.
             
