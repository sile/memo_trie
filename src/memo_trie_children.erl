%% @copyright 2014, Takeru Ohta <phjgt308@gmail.com>
%%
%% @doc ノードの子供群管理用モジュール用のインタフェース定義
-module(memo_trie_children).

%%----------------------------------------------------------------------------------------------------------------------
%% Exported API
%%----------------------------------------------------------------------------------------------------------------------
-export_type([
              children_module/0,
              state/0,
              key/0,
              child/0
             ]).

%%----------------------------------------------------------------------------------------------------------------------
%% Types
%%----------------------------------------------------------------------------------------------------------------------
-type children_module() :: module().
-type state() :: term().
-type key()   :: memo_trie:key_component().
-type child() :: memo_trie:trie_node().

%%----------------------------------------------------------------------------------------------------------------------
%% Callback API
%%----------------------------------------------------------------------------------------------------------------------
-callback empty() -> state().
-callback is_empty(state()) -> boolean().
-callback store(key(), child(), state()) -> state().
-callback find(key(), state()) -> {ok, child()} | error.
-callback take(key(), state()) -> {ok, child(), state()} | error.
-callback to_list(state()) -> [{key(), child()}].
