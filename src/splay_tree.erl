-module(splay_tree).

-export([new/0, store/3, find/2]).

-record(node, 
        {
          key,
          value,
          left,
          right
        }).

new() ->
    undefined.

store(Key, Value, undefined) ->
    #node{key=Key, value=Value};
store(Key, Value, Tree) ->
    Root = splay_node(Key, Tree, root, []),
    case Root#node.key of
        Key -> Root#node{value=Value};
        K when K < Key -> #node{key=Key, value=Value, left=Root};
        _              -> #node{key=Key, value=Value, right=Root}
    end.

find(_, _) ->
    ok.

splay_node(_, undefined, _, Path) ->
    splay(Path);
splay_node(Key, Node, Type, Path) ->
    #node{key=NodeKey, left=Left, right=Right} = Node,
    Path2 = [{Type,Node}|Path],
    
    if
        Key < NodeKey ->
            splay_node(Key, Left, left, Path2);
        Key > Node ->
            splay_node(Key, Right, right, Path2);
        true ->
            splay(Path2)
    end.

splay([{_, X}]) -> X;
splay([Xi, {_, P}]) ->
    %% zip
    case Xi of
        {left,  X} -> X#node{right=P#node{left=X#node.right}};
        {right, X} -> X#node{left=P#node{right=X#node.left}}
    end;
splay(_=Path) ->
    Path.
            
    
     
