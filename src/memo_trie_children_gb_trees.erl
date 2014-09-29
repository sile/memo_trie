%% @copyright 2014, Takeru Ohta <phjgt308@gmail.com>
%%
%% @doc 内部表現に`gb_trees'を用いた`momo_trie_children'の実装モジュール
-module(memo_trie_children_gb_trees).

-behaviour(memo_trie_children).

%%----------------------------------------------------------------------------------------------------------------------
%% Exported API
%%----------------------------------------------------------------------------------------------------------------------
-export_type([state/0]).

%%----------------------------------------------------------------------------------------------------------------------
%% Types
%%----------------------------------------------------------------------------------------------------------------------
-opaque state() :: gb_trees:tree().

%%----------------------------------------------------------------------------------------------------------------------
%% 'memo_trie_children' Callback API
%%----------------------------------------------------------------------------------------------------------------------
-export([empty/0, is_empty/1, store/3, find/2, take/2, to_list/1]).

%%----------------------------------------------------------------------------------------------------------------------
%% 'memo_trie_children' Callback Functions
%%----------------------------------------------------------------------------------------------------------------------
%% @private
-spec empty() -> state().
empty() ->
    gb_trees:empty().

%% @private
-spec is_empty(state()) -> boolean().
is_empty(Children) ->
    gb_trees:is_empty(Children).

%% @private
-spec store(memo_trie_children:key(), memo_trie_children:child(), state()) -> state().
store(Key, Child, Children) ->
    gb_trees:enter(Key, Child, Children).

%% @private
-spec find(memo_trie_children:key(), state()) -> {ok, memo_trie_children:child()} | error.
find(Key, Children) ->
    case gb_trees:lookup(Key, Children) of
        none           -> error;
        {value, Child} -> {ok, Child}
    end.

%% @private
-spec take(memo_trie_children:key(), state()) -> {ok, memo_trie_children:child(), state()} | error.
take(Key, Children) ->
    case gb_trees:lookup(Key, Children) of
        none           -> error;
        {value, Child} -> {ok, Child, gb_trees:delete(Key, Children)}
    end.

%% @private
-spec to_list(state()) -> [{memo_trie_children:key(), memo_trie_children:child()}].
to_list(Children) ->
    gb_trees:to_list(Children).
