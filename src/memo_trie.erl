%% @copyright 2014, Takeru Ohta <phjgt308@gmail.com>
%%
%% @doc 各ノードが任意のメモ情報を保持するトライ木の実装
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

         memo_fun_identity/1
        ]).

-export_type([
              trie/0, trie/2,
              trie_node/0,
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
-type key_component() :: term().  % XXX: wrong name
-type value() :: term().

-type memo() :: term().

-type memo_fun() :: fun ((memo_event()) -> memo()).

-type memo_event() :: {insert_value, NewValue::value(),                      NodeMemo::memo()}
                    | {update_value, {OldValue::value(), NewValue::value()}, NodeMemo::memo()}
                    | {delete_value, OldValue::value(),                      NodeMemo::memo()}
                    | {insert_child, {key_component(), NewChildMemo::memo()},                       NodeMemo::memo()}
                    | {update_child, {key_component(), OldChildMemo::memo(), NewChildMemo::memo()}, NodeMemo::memo()}
                    | {delete_child, {key_component(), OldChildMemo::memo()},                       NodeMemo::memo()}.

-type trie_node() :: {node, memo(), leaf(), children()}.

-type leaf() :: error | {ok, value()}.
-type children() :: memo_trie_children:state().

-type make_opts() :: [make_opt()].
-type make_opt()  :: {memo_fun, memo_fun()} % default: memo_fun_identity/1
                   | {memo_empty, memo()} % default: undefined
                   | {children_module, memo_trie_children:children_module()}. %  default: memo_trie_children_gb_trees TODO: changed to identity function

-type fold_fun() :: fun ((key(), value(), Acc::term()) -> NextAcc::term()).

%%----------------------------------------------------------------------------------------------------------------------
%% Exported Functions
%%----------------------------------------------------------------------------------------------------------------------
%% @doc 新しいトライ木を生成する
-spec new(make_opts()) -> trie().
new(Options) ->
    Opts =
        #opt{
           memo_fun        = proplists:get_value(memo_fun, Options, fun ?MODULE:memo_fun_identity/1),
           memo_empty      = proplists:get_value(memo_empty, Options, undefined),
           children_module = proplists:get_value(children_module, Options, memo_trie_children_gb_trees)
          },
    #?TRIE{
        opts = Opts,
        root = empty_node(Opts)
       }.

%% @doc `Trie'内の`Key'に対応する地点のサブトライ木を取り出す
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

%% @doc トライ木が空かどうかを判定する
-spec is_empty(trie()) -> boolean().
is_empty(#?TRIE{root = Root, opts = Opts}) -> is_empty_node(Root, Opts).

%% @doc トライ木に格納されている要素の数を取得する
%%
%% この関数は、要素数に比例した(線形の)処理オーダーを要するので注意が必要
-spec size(trie()) -> non_neg_integer().
size(Trie) ->
    foldl(fun (_, _, Size) -> Size + 1 end, 0, Trie).

%% @doc `Trie'内の`Key'に対応する地点に`Value'を格納する
-spec store(key(), value(), trie()) -> trie().
store(Key, Value, Trie) ->
    Root = store_node(Key, Value, Trie#?TRIE.root, Trie#?TRIE.opts),
    Trie#?TRIE{root = Root}.

%% @doc `Trie'内の`Key'に対応するノードの値を検索する
-spec find(key(), trie()) -> {ok, value()} | error.
find(Key, Trie) ->
    find_leaf(Key, Trie#?TRIE.root, Trie#?TRIE.opts).

%% @doc `Trie'内の`Key'に対応するノードの値を削除する
-spec erase(key(), trie()) -> trie().
erase(Key, Trie) ->
    Root = erase_node(Key, Trie#?TRIE.root, Trie#?TRIE.opts),
    Trie#?TRIE{root = Root}.

%% @doc 連想リストからトライ木を生成する
-spec from_list(make_opts(), [{key(), value()}]) -> trie().
from_list(Options, List) ->
    lists:foldl(
      fun ({Key, Value}, Acc) -> store(Key, Value, Acc) end,
      new(Options),
      List).

%% @doc トライ木から連想リストを生成する
-spec to_list(trie()) -> [{key(), value()}].
to_list(Trie) ->
    foldr(fun (Key, Value, Acc) -> [{Key, Value} | Acc] end,
          [],
          Trie).

%% @doc トライ木内の要素を左端から順番に畳み込む
-spec foldl(fold_fun(), Initial, trie()) -> Result when
      Initial :: term(),
      Result  :: term().
foldl(Fun, Initial, Trie) ->
    foldl_node([], Fun, Initial, Trie#?TRIE.root, Trie#?TRIE.opts).

%% @doc トライ木内の要素を右端から順番に畳み込む
-spec foldr(fold_fun(), Initial, trie()) -> Result when
      Initial :: term(),
      Result  :: term().
foldr(Fun, Initial, Trie) ->
    foldr_node([], Fun, Initial, Trie#?TRIE.root, Trie#?TRIE.opts).

%% @doc 木のルートノードを取得する
-spec get_root_node(trie()) -> trie_node().
get_root_node(Trie) ->
    Trie#?TRIE.root.

%% @doc `Trie'内の`Key'に対応するノードを検索する
-spec find_node(key(), trie()) -> {ok, trie_node()} | error.
find_node(Key, Trie) ->
    find_node(Key, Trie#?TRIE.root, Trie#?TRIE.opts).

%% @doc `Trie'内の`Key'に対応するノードのメモ情報を検索する
-spec find_memo(key(), trie()) -> {ok, memo()} | error.
find_memo(Key, Trie) ->
    case find_node(Key, Trie) of
        error      -> error;
        {ok, Node} -> {ok, get_memo(Node)}
    end.

%% @doc ノードからメモ情報を取り出す
-spec get_memo(trie_node()) -> memo().
get_memo({node, Memo, _, _}) -> Memo.

%% @doc ノードに格納されている値を取り出す
-spec get_value(trie_node()) -> {ok, value()} | error. % XXX: name
get_value({node, _, MaybeValue, _}) -> MaybeValue.

%% @doc ノードに格納されている値を取り出す
%%
%% そのような値が存在しない場合は、代わりに`Default'が返される
-spec get_value(trie_node(), value()) -> value().
get_value(Node, Default) ->
    case get_value(Node) of
        error       -> Default;
        {ok, Value} -> Value
    end.

%% @doc ノードの子ノード群を取得する
-spec get_children(trie_node()) -> children().
get_children({node, _, _, Children}) -> Children.

%% @doc 子ノード群を連想リスト形式に変換する
-spec children_to_list(children(), trie()) -> [{key_component(), trie_node()}].
children_to_list(Children, Trie) ->
    ?CHILDREN(Trie#?TRIE.opts):to_list(Children).

%% @doc デフォルトで使用されるメモ関数
%%
%% 既存のメモ値をそのまま返す
-spec memo_fun_identity(memo_event()) -> memo().
memo_fun_identity({_, _, Memo}) ->
    Memo.

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
