%% @copyright 2013-2014, Takeru Ohta <phjgt308@gmail.com>
%%
%% TODO: doc
-module(memo_trie).

%%----------------------------------------------------------------------------------------------------------------------
%% Exported API
%%----------------------------------------------------------------------------------------------------------------------
-export([
         new/1,
         is_trie/1,
         is_empty/1,
         store/3,
         find/2,
         erase/2,
         from_list/2,

         memo_fun_none/1
        ]).

-export_type([
              trie/0, trie/2,
              key/0,
              key_component/0,
              value/0,
              make_opt/0, make_opts/0,
              memo/0,
              memo_event/0,
              memo_fun/0
             ]).

%%----------------------------------------------------------------------------------------------------------------------
%% Macros & Records & Types
%%----------------------------------------------------------------------------------------------------------------------
-define(TRIE, ?MODULE).
-define(CHILDREN(Options), (Options#opt.children_module)).

-record(opt,
        {
          memo_fun        :: memo_fun(),
          children_module :: memo_trie_children:children_module()
        }).

-record(?TRIE,
        {
          opts :: #opt{},
          root :: trie_node()
        }).

-type trie() :: trie(key(), value()).
-opaque trie(_Key, _Value) :: #?TRIE{}.

-type key() :: [key_component()].
-type key_component() :: term().
-type value() :: term().

-type memo() :: term().

-type memo_fun() :: fun ((memo_event()) -> memo()). % TODO: associative云々

-type memo_event() :: {insert_value, value(), trie_node()}
                    | {update_value, value(), trie_node()}
                    | {delete_value, value(), trie_node()}
                    | {insert_child, {key_component(), trie_node()}, trie_node()}
                    | {update_child, {key_component(), trie_node()}, trie_node()}
                    | {delete_child, {key_component(), trie_node()}, trie_node()}.

-type trie_node() :: {node, error | {ok, memo()}, leaf(), children()}.

-type leaf() :: error | {ok, value()}.
-type children() :: gb_trees:tree(key_component(), trie_node()).

-type make_opts() :: [make_opt()].
-type make_opt()  :: {memo_fun, memo_fun()} % default: memo_fun_none/1
                   | {children_module, memo_trie_children:children_module()}. %  default: memo_trie_children_gb_trees

%%----------------------------------------------------------------------------------------------------------------------
%% Exported Functions
%%----------------------------------------------------------------------------------------------------------------------
-spec new(make_opts()) -> trie().
new(Options) ->
    Opts =
        #opt{
           memo_fun        = proplists:get_value(memo_fun, Options, fun ?MODULE:memo_fun_none/1),
           children_module = proplists:get_value(children_module, Options, memo_trie_children_gb_trees)
          },
    #?TRIE{
        opts = Opts,
        root = empty_node(Opts)
       }.

-spec is_trie(trie() | term()) -> boolean().
is_trie(#?TRIE{}) -> true;
is_trie(_)        -> false.

-spec is_empty(trie()) -> boolean().
is_empty(#?TRIE{root = Root}) -> is_empty_node(Root).

-spec store(key(), value(), trie()) -> trie().
store(Key, Value, Trie) ->
    Root = store_node(Key, Value, Trie#?TRIE.root, Trie#?TRIE.opts),
    Trie#?TRIE{root = Root}.

-spec find(key(), trie()) -> {ok, value()} | error.
find(Key, Trie) ->
    find_leaf(Key, Trie#?TRIE.root, Trie#?TRIE.opts).

-spec erase(key(), trie()) -> trie().
erase(Key, Trie) ->
    Root = erase_node(Key, Trie#?TRIE.root, Trie#?TRIE.opts),
    Trie#?TRIE{root = Root}.

-spec from_list(make_opts(), [{key(), value()}]) -> trie().
from_list(Options, List) ->
    lists:foldl(
      fun ({Key, Value}, Acc) -> store(Key, Value, Acc) end,
      new(Options),
      List).

-spec memo_fun_none(memo_event()) -> none.
memo_fun_none(_) ->
    none.

%%----------------------------------------------------------------------------------------------------------------------
%% Internal Functions
%%----------------------------------------------------------------------------------------------------------------------
-spec find_leaf(key(), trie_node(), #opt{}) -> leaf().
find_leaf([], {node, _, Leaf, _}, _) -> Leaf;
find_leaf([Head | Tail], Node, Options)    ->
    case ?CHILDREN(Options):find(Head, get_children(Node)) of
        error       -> error;
        {ok, Child} -> find_leaf(Tail, Child, Options)
    end.

-spec erase_node(key(), trie_node(), #opt{}) -> trie_node().
erase_node([], Node, Options) ->
    delete_leaf(Node, Options);
erase_node([Head | Tail], Node, Options) ->
    case ?CHILDREN(Options):find(Head, get_children(Node)) of
        error        -> Node;
        {ok, Child0} ->
            Child1 = erase_node(Tail, Child0, Options),
            case is_empty_node(Child1) of
                false -> update_child(Head, Child1, Node, Options);
                true  -> delete_child(Head, Node, Options)
            end
    end.

-spec is_empty_node(trie_node()) -> boolean().
is_empty_node({node, Memo, _, _}) -> Memo =:= error.

-spec delete_child(key_component(), trie_node(), #opt{}) -> trie_node().
delete_child(Key, Node, Options) ->
    {ok, Child, Children} = ?CHILDREN(Options):take(Key, get_children(Node)),
    case get_leaf(Node) =:= error andalso ?CHILDREN(Options):is_empty(Children) of
        true  -> empty_node(Options);
        false ->
            Memo = (Options#opt.memo_fun)({delete_child, {Key, Child}, Node}),
            {node, Memo, get_leaf(Node), Children}
    end.

-spec store_node(key(), value(), trie_node(), #opt{}) -> trie_node().
store_node([], Value, Node, Options) ->
    store_value(Value, Node, Options);
store_node([Head | Tail], Value, Node, Options) ->
    case ?CHILDREN(Options):find(Head, get_children(Node)) of
        error ->
            Child = store_node(Tail, Value, empty_node(Options), Options),
            insert_child(Head, Child, Node, Options);
        {ok, Child0} ->
            Child1 = store_node(Tail, Value, Child0, Options),
            update_child(Head, Child1, Node, Options)
    end.

-spec delete_leaf(trie_node(), #opt{}) -> trie_node().
delete_leaf(Node, Options) ->
    case get_leaf(Node) of
        error       -> Node;
        {ok, Value} ->
            case ?CHILDREN(Options):is_empty(get_children(Node)) of
                true  -> empty_node(Options);
                false ->
                    Memo = (Options#opt.memo_fun)({delete_value, Value, Node}),
                    {node, Memo, error, get_children(Node)}
            end
    end.

-spec store_value(value(), trie_node(), #opt{}) -> trie_node().
store_value(Value, Node, Options) ->
    Memo = case get_leaf(Node) of
               error   -> (Options#opt.memo_fun)({update_value, Value, Node});
               {ok, _} -> (Options#opt.memo_fun)({insert_value, Value, Node})
           end,
    {node, Memo, {ok, Value}, get_children(Node)}.

-spec insert_child(key_component(), trie_node(), trie_node(), #opt{}) -> trie_node().
insert_child(Key, Child, Node, Options) ->
    Memo = (Options#opt.memo_fun)({insert_child, {Key, Child}, Node}),
    {node, Memo, get_leaf(Node), ?CHILDREN(Options):store(Key, Child, get_children(Node))}.

-spec update_child(key_component(), trie_node(), trie_node(), #opt{}) -> trie_node().
update_child(Key, Child, Node, Options) ->
    Memo = (Options#opt.memo_fun)({update_child, {Key, Child}, Node}),
    {node, Memo, get_leaf(Node), ?CHILDREN(Options):store(Key, Child, get_children(Node))}.

-spec empty_node(#opt{}) -> trie_node().
empty_node(Opts) ->
    {node, error, error, ?CHILDREN(Opts):empty()}.

-spec get_children(trie_node()) -> children().
get_children({node, _, _, Children}) -> Children.

-spec get_leaf(trie_node()) -> leaf().
get_leaf({node, _, Leaf, _}) -> Leaf.
