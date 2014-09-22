%% @copyright 2013-2014, Takeru Ohta <phjgt308@gmail.com>
%%
-module(memo_trie_tests).

-include_lib("eunit/include/eunit.hrl").

%%----------------------------------------------------------------------------------------------------------------------
%% Unit Tests
%%----------------------------------------------------------------------------------------------------------------------
new_test_() ->
    [
     {"Creates empty trie",
      fun () ->
              Trie = memo_trie:new([{memo_fun, constant_memo_fun(ok)}]),
              ?assert(memo_trie:is_trie(Trie)),
              ?assert(memo_trie:is_empty(Trie))
      end}
    ].

store_test_() ->
    [
     {"Stores root value",
      fun () ->
              Trie0 = memo_trie:new([{memo_fun, constant_memo_fun(ok)}]),
              Trie1 = memo_trie:store([], value, Trie0),
              ?assertEqual({ok, value}, memo_trie:find([], Trie1))
      end},
     {"Stores non-existing key",
      fun () ->
              Trie0 = memo_trie:new([{memo_fun, constant_memo_fun(ok)}]),
              Trie1 = memo_trie:store([key], value, Trie0),
              ?assertEqual({ok, value}, memo_trie:find([key], Trie1))
      end},
     {"Updates existing key",
      fun () ->
              Trie0 = memo_trie:new([{memo_fun, constant_memo_fun(ok)}]),

              Trie1 = memo_trie:store([key], first, Trie0),
              {ok, first} = memo_trie:find([key], Trie1),

              Trie2 = memo_trie:store([key], second, Trie1),
              ?assertEqual({ok, second}, memo_trie:find([key], Trie2))
      end}
    ].

erase_test_() ->
    [
     {"TODO: doc",
      fun () ->
              Trie0 = memo_trie:new([{memo_fun, constant_memo_fun(ok)}]),

              Trie1 = memo_trie:store([key], first, Trie0),
              {ok, first} = memo_trie:find([key], Trie1),

              Trie2 = memo_trie:erase([key], Trie1),
              ?assertEqual(error, memo_trie:find([key], Trie2)),
              ?assert(memo_trie:is_empty(Trie2))
      end}
    ].

from_list_test_() ->
    [
     {"Creates a trie from list",
      fun () ->
              Input =
                  [
                   {[aa],         0},
                   {[aa, bb, cc], 1},
                   {[aa, cc, cc], 2},
                   {[11, 22, 33], 3},
                   {[11, 22, 22], 4},
                   {[11, 22],     5},
                   {[11, 33, 44], 6},
                   {[],           7}
                  ],
              Trie = memo_trie:from_list([{memo_fun, constant_memo_fun(ok)}], Input),
              lists:foreach(
                fun ({Key, Value}) ->
                        ?assertEqual({ok, Value}, memo_trie:find(Key, Trie))
                end,
                Input)
      end}
    ].

%%----------------------------------------------------------------------------------------------------------------------
%% Internal Functions
%%----------------------------------------------------------------------------------------------------------------------
constant_memo_fun(ConstValue) ->
    fun (_) -> ConstValue end.
