memo_trie
=========

各ノードがメモ情報を保持しているトライ木の実装

メモ情報
--------

トライ木の生成時に任意のメモ情報計算関数を登録することが可能。

メモ情報計算関数は以下のタイミングで呼び出される:
- ノードの値の登録
- ノードの値の更新
- ノードの値の削除
- 子ノードの追加
- 子ノードの更新
- 子ノードの削除

特に重要なのは後半の３つの子ノードに関連する呼び出しであり、
それぞれの呼び出しで適切なメモ情報を計算し、ノードに保持させておくことで、
その情報を後から取得したい場合に、再度子ノード全体を操作し再計算することなく、
キャッシュされた（メモされた）情報が即座に利用可能となる。
（つまり典型的にはメモ情報には、子孫ノード全体のサマリ情報が保持されることになる）


また、メモ関数は結合法則を満たしていることが望ましい。

使用例
------
```erlang
%% 子孫ノードの値全てをフラットなリストとして保持しておくようなメモ関数
> MemoFun =
    fun ({insert_value, V,        Memo}) -> [V | Memo];
        ({update_value, {V0, V1}, Memo}) -> [V1 | lists:delete(V0, Memo)];
        ({delete_value, V,        Memo}) -> lists:delete(V, Memo);
        ({insert_child, {_, C},      Memo}) -> Memo ++ C;
        ({update_child, {_, C0, C1}, Memo}) -> Memo ++ C1 -- C0;
        ({delete_child, {_, C},      Memo}) -> Memo -- C
    end.

%% 値の登録
> T0 = memo_trie:new([{memo_fun, MemoFun}, {memo_empty, []}]).
> T1 = memo_trie:store("abc", val1, T0).
> T2 = memo_trie:store("abb", val2, T1).
> T3 = memo_trie:store("a",   val3, T2).
> T4 = memo_trie:store("123", val4, T3).

%% メモ情報の取得

% ルートノードのメモ情報には全てのノードの値が含まれている
> memo_trie:find_memo("", T4).
{ok,[val1,val2,val3,val4]}

%% "ab"に対応するノードのメモ情報には、２つの子ノード("abc" and "abb")の値が含まれている
> memo_trie:find_memo("ab", T4).
{ok,[val1,val2]}


%% "a"に対応するノードのメモ情報には、自分自身および２つの子孫ノード("abc" and "abb")の値が含まれている
> memo_trie:find_memo("a", T4).
{ok,[val1,val2,val3]}
```

API
---
See [EDoc Document](doc/README.md)
