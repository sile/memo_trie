%% @copyright 2014, Takeru Ohta <phjgt308@gmail.com>
%%
-module(memo_trie_tests).

-include_lib("eunit/include/eunit.hrl").

%%----------------------------------------------------------------------------------------------------------------------
%% Macros
%%----------------------------------------------------------------------------------------------------------------------
-define(TRIE_NEW(), memo_trie:new([])).

%%----------------------------------------------------------------------------------------------------------------------
%% Unit Tests
%%----------------------------------------------------------------------------------------------------------------------
new_test_() ->
    [
     {"Creates an empty trie",
      fun () ->
              Trie = ?TRIE_NEW(),
              ?assert(memo_trie:is_trie(Trie)),
              ?assert(memo_trie:is_empty(Trie))
      end}
    ].

is_trie_test_() ->
    [
     {"Returns 'true' if the argument is a trie",
      ?_assert(memo_trie:is_trie(?TRIE_NEW()))},
     {"Returns 'false' if the argument is not a trie",
      ?_assertNot(memo_trie:is_trie(hoge))}
    ].

store_test_() ->
    [
     {"Stores root entry",
      fun () ->
              Trie0 = ?TRIE_NEW(),

              Trie1 = memo_trie:store("", 1, Trie0),
              ?assertEqual({ok, 1}, memo_trie:find("", Trie1)),
              ?assertNot(memo_trie:is_empty(Trie1))
      end},
     {"Stores common-prefix entries",
      fun () ->
              Trie0 = ?TRIE_NEW(),

              Trie1 = memo_trie:store("abc", 1, Trie0),
              Trie2 = memo_trie:store("ab",  2, Trie1),
              Trie3 = memo_trie:store("a",   3, Trie2),
              Trie4 = memo_trie:store("acb", 4, Trie3),

              %% Founds
              ?assertEqual({ok, 1}, memo_trie:find("abc", Trie4)),
              ?assertEqual({ok, 2}, memo_trie:find("ab",  Trie4)),
              ?assertEqual({ok, 3}, memo_trie:find("a",   Trie4)),
              ?assertEqual({ok, 4}, memo_trie:find("acb", Trie4)),

              %% Not Founds
              ?assertEqual(error, memo_trie:find("", Trie4)),
              ?assertEqual(error, memo_trie:find("ac", Trie4)),
              ?assertEqual(error, memo_trie:find("abcd", Trie4))
      end},
     {"Updates existing entry",
      fun () ->
              Trie0 = ?TRIE_NEW(),
              Trie1 = memo_trie:store("abc", 1, Trie0),
              Trie2 = memo_trie:store("ab",  2, Trie1),
              Trie3 = memo_trie:store("a",   3, Trie2),
              Trie4 = memo_trie:store("acb", 4, Trie3),

              Trie5 = memo_trie:store("ab", 5, Trie4),

              ?assertEqual({ok, 5}, memo_trie:find("ab", Trie5))
      end}
    ].

erase_test_() ->
    [
     {"Erases non-existing key",
      fun () ->
              Trie0 = ?TRIE_NEW(),
              Trie1 = memo_trie:erase("hoge", Trie0),
              ?assertEqual(Trie0, Trie1)
      end},
     {"Erases existing key",
      fun () ->
              Trie0 = ?TRIE_NEW(),
              Trie1 = memo_trie:store("abc", 1, Trie0),
              Trie2 = memo_trie:store("ab",  2, Trie1),
              Trie3 = memo_trie:store("a",   3, Trie2),

              Trie4 = memo_trie:erase("ab", Trie3),
              ?assertEqual(error,   memo_trie:find("ab", Trie4)),
              ?assertEqual({ok, 1}, memo_trie:find("abc", Trie4)),
              ?assertEqual({ok, 3}, memo_trie:find("a", Trie4))
      end}
    ].

from_list_test_() ->
    [
     {"Creates a trie from list",
      fun () ->
              Input0 = lists:sort([{integer_to_list(N), N} || N <- lists:seq(1, 1000)]),
              Input1 = shuffle(Input0),

              %% from_list
              Trie0 = memo_trie:from_list([], Input1),
              ?assertEqual(length(Input1), memo_trie:size(Trie0)),

              %% find
              lists:foreach(fun ({Key, Value}) ->
                                    ?assertEqual({ok, Value}, memo_trie:find(Key, Trie0))
                            end,
                            Input1),

              %% to_list
              ?assertEqual(Input0, memo_trie:to_list(Trie0)),

              %% erase
              Trie1 =
                  lists:foldl(fun ({Key, Value}, AccTrie0) ->
                                      ?assertEqual({ok, Value}, memo_trie:find(Key, AccTrie0)),
                                      AccTrie1 = memo_trie:erase(Key, AccTrie0),
                                      ?assertEqual(error, memo_trie:find(Key, AccTrie1)),
                                      AccTrie1
                              end,
                              Trie0,
                              Input1),
              ?assert(memo_trie:is_empty(Trie1))
      end}
    ].

%%----------------------------------------------------------------------------------------------------------------------
%% Internal Functions
%%--------------------------------------------------------------------------------------------------------------------
-spec shuffle([Element]) -> [Element] when Element :: term().
shuffle(List) ->
    [E || {_, E} <- lists:ukeysort(1, [{random:uniform(), E} || E <- List])].
