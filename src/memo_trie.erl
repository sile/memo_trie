%% @copyright 2014, Takeru Ohta <phjgt308@gmail.com>
%%
%% TODO: doc
-module(memo_trie).

%%----------------------------------------------------------------------------------------------------------------------
%% Exported API
%%----------------------------------------------------------------------------------------------------------------------
-export([
         new/1,
         subtrie/2,
         is_trie/1,
         is_empty/1,
         size/1,
         store/3,
         find/2,
         erase/2,
         foldl/3,
         foldr/3,
         from_list/2,
         to_list/1,

         get_root_node/1,
         find_memo/2,
         find_node/2,
         children_to_list/2,
         get_memo/1,
         get_value/1, get_value/2,
         get_children/1,

         memo_fun_none/1
        ]).

-export_type([
              trie/0, trie/2,
              key/0,
              key_component/0,
              value/0,
              make_opt/0, make_opts/0,
              fold_fun/0,
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
          memo_empty      :: memo(),
          children_module :: memo_trie_children:children_module()
        }).

-record(?TRIE,
        {
          opts :: #opt{},
          root :: trie_node()
        }).

-type trie() :: trie(key(), value()).
-type trie(_Key, _Value) :: #?TRIE{}.
%% -opaque trie(_Key, _Value) :: #?TRIE{}. % dialyzer(R16B03) says 'Polymorphic opaque types not supported yet'

-type key() :: [key_component()].
-type key_component() :: term().
-type value() :: term().

-type memo() :: term().

-type memo_fun() :: fun ((memo_event()) -> memo()). % TODO: associative云々

-type memo_event() :: {insert_value, value(), memo()}
                    | {update_value, {value(), value()}, memo()}
                    | {delete_value, value(), memo()}
                    | {insert_child, {key_component(), memo()}, memo()}
                    | {update_child, {key_component(), memo(), memo()}, memo()}
                    | {delete_child, {key_component(), memo()}, memo()}.

-type trie_node() :: {node, memo(), leaf(), children()}.

-type leaf() :: error | {ok, value()}.
-type children() :: gb_trees:tree(key_component(), trie_node()).

-type make_opts() :: [make_opt()].
-type make_opt()  :: {memo_fun, memo_fun()} % default: memo_fun_none/1
                   | {memo_empty, memo()} % default: undefined
                   | {children_module, memo_trie_children:children_module()}. %  default: memo_trie_children_gb_trees TODO: changed to identity function

-type fold_fun() :: fun ((key(), value(), Acc::term()) -> NextAcc::term()).
%%----------------------------------------------------------------------------------------------------------------------
%% Exported Functions
%%----------------------------------------------------------------------------------------------------------------------
-spec new(make_opts()) -> trie().
new(Options) ->
    Opts =
        #opt{
           memo_fun        = proplists:get_value(memo_fun, Options, fun ?MODULE:memo_fun_none/1),
           memo_empty      = proplists:get_value(memo_empty, Options, undefined),
           children_module = proplists:get_value(children_module, Options, memo_trie_children_gb_trees)
          },
    #?TRIE{
        opts = Opts,
        root = empty_node(Opts)
       }.

-spec subtrie(key(), trie()) -> {ok, trie()} | error.
subtrie(Key, Trie) ->
    case find_node(Key, Trie) of
        error      -> error;
        {ok, Node} -> {ok, Trie#?TRIE{root = Node}}
    end.

%% @doc Tests if `Value' is a trie and returns `true' if so and `false' otherwise
-spec is_trie(Value :: term()) -> boolean().
is_trie(#?TRIE{}) -> true;
is_trie(_)        -> false.

-spec is_empty(trie()) -> boolean().
is_empty(#?TRIE{root = Root, opts = Opts}) -> is_empty_node(Root, Opts).

-spec size(trie()) -> non_neg_integer().
size(Trie) ->
    foldl(fun (_, _, Size) -> Size + 1 end, 0, Trie).

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

-spec to_list(trie()) -> [{key(), value()}].
to_list(Trie) ->
    foldr(fun (Key, Value, Acc) -> [{Key, Value} | Acc] end,
          [],
          Trie).

-spec foldl(fold_fun(), Initial, trie()) -> Result when
      Initial :: term(),
      Result  :: term().
foldl(Fun, Initial, Trie) ->
    foldl_node([], Fun, Initial, Trie#?TRIE.root, Trie#?TRIE.opts).

-spec foldr(fold_fun(), Initial, trie()) -> Result when
      Initial :: term(),
      Result  :: term().
foldr(Fun, Initial, Trie) ->
    foldr_node([], Fun, Initial, Trie#?TRIE.root, Trie#?TRIE.opts).

-spec get_root_node(trie()) -> trie_node().
get_root_node(Trie) ->
    Trie#?TRIE.root.

-spec find_node(key(), trie()) -> {ok, trie_node()} | error.
find_node(Key, Trie) ->
    find_node(Key, Trie#?TRIE.root, Trie#?TRIE.opts).

-spec find_memo(key(), trie()) -> {ok, trie_node()} | error.
find_memo(Key, Trie) ->
    case find_node(Key, Trie) of
        error      -> error;
        {ok, Node} -> {ok, get_memo(Node)}
    end.

-spec children_to_list(children(), trie()) -> [{key_component(), trie_node()}].
children_to_list(Children, Trie) ->
    ?CHILDREN(Trie#?TRIE.opts):to_list(Children).

-spec get_memo(trie_node()) -> memo().
get_memo({node, Memo, _, _}) -> Memo.

-spec get_value(trie_node()) -> {ok, value()} | error.
get_value({node, _, MaybeValue, _}) -> MaybeValue. % XXX: name

-spec get_value(trie_node(), value()) -> value().
get_value(Node, Default) ->
    case get_value(Node) of
        error       -> Default;
        {ok, Value} -> Value
    end.

-spec get_children(trie_node()) -> children().
get_children({node, _, _, Children}) -> Children.

-spec memo_fun_none(memo_event()) -> none.
memo_fun_none(_) ->
    none.

%%----------------------------------------------------------------------------------------------------------------------
%% Internal Functions
%%----------------------------------------------------------------------------------------------------------------------
-spec find_node(key(), trie_node(), #opt{}) -> {ok, trie_node()} | error.
find_node([], Node, _Options) ->
    {ok, Node};
find_node([Head | Tail], Node, Options) ->
    case ?CHILDREN(Options):find(Head, get_children(Node)) of
        error       -> error;
        {ok, Child} -> find_node(Tail, Child, Options)
    end.

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
            case is_empty_node(Child1, Options) of
                false -> update_child(Head, Child0, Child1, Node, Options);
                true  -> delete_child(Head, Node, Options)
            end
    end.

-spec is_empty_node(trie_node(), #opt{}) -> boolean().
is_empty_node({node, _, Value, Children}, Options) ->
    Value =:= error andalso ?CHILDREN(Options):is_empty(Children).

-spec delete_child(key_component(), trie_node(), #opt{}) -> trie_node().
delete_child(Key, Node, Options) ->
    {ok, Child, Children} = ?CHILDREN(Options):take(Key, get_children(Node)),
    case get_leaf(Node) =:= error andalso ?CHILDREN(Options):is_empty(Children) of
        true  -> empty_node(Options);
        false ->
            Memo = (Options#opt.memo_fun)({delete_child, {Key, get_memo(Child)}, get_memo(Node)}),
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
            update_child(Head, Child0, Child1, Node, Options)
    end.

-spec delete_leaf(trie_node(), #opt{}) -> trie_node().
delete_leaf(Node, Options) ->
    case get_leaf(Node) of
        error       -> Node;
        {ok, Value} ->
            case ?CHILDREN(Options):is_empty(get_children(Node)) of
                true  -> empty_node(Options);
                false ->
                    Memo = (Options#opt.memo_fun)({delete_value, Value, get_memo(Node)}),
                    {node, Memo, error, get_children(Node)}
            end
    end.

-spec store_value(value(), trie_node(), #opt{}) -> trie_node().
store_value(Value, Node, Options) ->
    Memo = case get_leaf(Node) of
               error     -> (Options#opt.memo_fun)({insert_value, Value, get_memo(Node)});
               {ok, Old} -> (Options#opt.memo_fun)({update_value, {Old, Value}, get_memo(Node)})
           end,
    {node, Memo, {ok, Value}, get_children(Node)}.

-spec insert_child(key_component(), trie_node(), trie_node(), #opt{}) -> trie_node().
insert_child(Key, Child, Node, Options) ->
    Memo = (Options#opt.memo_fun)({insert_child, {Key, get_memo(Child)}, get_memo(Node)}),
    {node, Memo, get_leaf(Node), ?CHILDREN(Options):store(Key, Child, get_children(Node))}.

-spec update_child(key_component(), trie_node(), trie_node(), trie_node(), #opt{}) -> trie_node().
update_child(Key, Old, Child, Node, Options) ->
    Memo = (Options#opt.memo_fun)({update_child, {Key, get_memo(Old), get_memo(Child)}, get_memo(Node)}),
    {node, Memo, get_leaf(Node), ?CHILDREN(Options):store(Key, Child, get_children(Node))}.

-spec empty_node(#opt{}) -> trie_node().
empty_node(Opts) ->
    {node, Opts#opt.memo_empty, error, ?CHILDREN(Opts):empty()}.

-spec get_leaf(trie_node()) -> leaf().
get_leaf({node, _, Leaf, _}) -> Leaf.

-spec foldl_node(key(), fold_fun(), Acc, trie_node(), #opt{}) -> Acc when Acc :: term().
foldl_node(Key, Fun, Acc0, {node, _, MaybeValue, Children}, Options) ->
    Acc1 = case MaybeValue of
               error       -> Acc0;
               {ok, Value} -> Fun(Key, Value, Acc0)
           end,
    lists:foldl(fun ({Last, Child}, Acc2) ->
                        foldl_node(Key ++ [Last], Fun, Acc2, Child, Options)
                end,
                Acc1,
                ?CHILDREN(Options):to_list(Children)).

-spec foldr_node(key(), fold_fun(), Acc, trie_node(), #opt{}) -> Acc when Acc :: term().
foldr_node(Key, Fun, Acc0, {node, _, MaybeValue, Children}, Options) ->
    Acc2 =
        lists:foldr(fun ({Last, Child}, Acc1) ->
                            foldr_node(Key ++ [Last], Fun, Acc1, Child, Options)
                    end,
                    Acc0,
                    ?CHILDREN(Options):to_list(Children)),
    case MaybeValue of
        error       -> Acc2;
        {ok, Value} -> Fun(Key, Value, Acc2)
    end.
