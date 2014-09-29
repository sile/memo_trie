

# Module memo_trie #
* [Description](#description)
* [Data Types](#types)
* [Function Index](#index)
* [Function Details](#functions)


各ノードが任意のメモ情報を保持するトライ木の実装.
Copyright (c) 2014, Takeru Ohta <phjgt308@gmail.com>


<a name="types"></a>

## Data Types ##




### <a name="type-children">children()</a> ###



<pre><code>
children() = <a href="memo_trie_children.md#type-state">memo_trie_children:state()</a>
</code></pre>





### <a name="type-fold_fun">fold_fun()</a> ###



<pre><code>
fold_fun() = fun((<a href="#type-key">key()</a>, <a href="#type-value">value()</a>, Acc::term()) -&gt; NextAcc::term())
</code></pre>





### <a name="type-key">key()</a> ###



<pre><code>
key() = [<a href="#type-key_component">key_component()</a>]
</code></pre>





### <a name="type-key_component">key_component()</a> ###



<pre><code>
key_component() = term()
</code></pre>



 XXX: wrong name



### <a name="type-leaf">leaf()</a> ###



<pre><code>
leaf() = error | {ok, <a href="#type-value">value()</a>}
</code></pre>





### <a name="type-make_opt">make_opt()</a> ###



<pre><code>
make_opt() = {memo_fun, <a href="#type-memo_fun">memo_fun()</a>} | {memo_empty, <a href="#type-memo">memo()</a>} | {children_module, <a href="memo_trie_children.md#type-children_module">memo_trie_children:children_module()</a>}
</code></pre>



  default: memo_trie_children_gb_trees TODO: changed to identity function



### <a name="type-make_opts">make_opts()</a> ###



<pre><code>
make_opts() = [<a href="#type-make_opt">make_opt()</a>]
</code></pre>





### <a name="type-memo">memo()</a> ###



<pre><code>
memo() = term()
</code></pre>





### <a name="type-memo_event">memo_event()</a> ###



<pre><code>
memo_event() = {insert_value, NewValue::<a href="#type-value">value()</a>, NodeMemo::<a href="#type-memo">memo()</a>} | {update_value, {OldValue::<a href="#type-value">value()</a>, NewValue::<a href="#type-value">value()</a>}, NodeMemo::<a href="#type-memo">memo()</a>} | {delete_value, OldValue::<a href="#type-value">value()</a>, NodeMemo::<a href="#type-memo">memo()</a>} | {insert_child, {<a href="#type-key_component">key_component()</a>, NewChildMemo::<a href="#type-memo">memo()</a>}, NodeMemo::<a href="#type-memo">memo()</a>} | {update_child, {<a href="#type-key_component">key_component()</a>, OldChildMemo::<a href="#type-memo">memo()</a>, NewChildMemo::<a href="#type-memo">memo()</a>}, NodeMemo::<a href="#type-memo">memo()</a>} | {delete_child, {<a href="#type-key_component">key_component()</a>, OldChildMemo::<a href="#type-memo">memo()</a>}, NodeMemo::<a href="#type-memo">memo()</a>}
</code></pre>





### <a name="type-memo_fun">memo_fun()</a> ###



<pre><code>
memo_fun() = fun((<a href="#type-memo_event">memo_event()</a>) -&gt; <a href="#type-memo">memo()</a>)
</code></pre>





### <a name="type-trie">trie()</a> ###



<pre><code>
trie(_Key, _Value) = #memo_trie{opts = undefined | #opt{memo_fun = undefined | <a href="#type-memo_fun">memo_fun()</a>, memo_empty = undefined | <a href="#type-memo">memo()</a>, children_module = undefined | <a href="memo_trie_children.md#type-children_module">memo_trie_children:children_module()</a>}, root = undefined | <a href="#type-trie_node">trie_node()</a>}
</code></pre>



  -opaque trie(_Key, _Value) :: #?TRIE{}. % dialyzer(R16B03) says 'Polymorphic opaque types not supported yet'



### <a name="type-trie">trie()</a> ###



<pre><code>
trie() = <a href="#type-trie">trie</a>(<a href="#type-key">key()</a>, <a href="#type-value">value()</a>)
</code></pre>





### <a name="type-trie_node">trie_node()</a> ###



<pre><code>
trie_node() = {node, <a href="#type-memo">memo()</a>, <a href="#type-leaf">leaf()</a>, <a href="#type-children">children()</a>}
</code></pre>





### <a name="type-value">value()</a> ###



<pre><code>
value() = term()
</code></pre>


<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#children_to_list-2">children_to_list/2</a></td><td>子ノード群を連想リスト形式に変換する.</td></tr><tr><td valign="top"><a href="#erase-2">erase/2</a></td><td><code>Trie</code>内の<code>Key</code>に対応するノードの値を削除する.</td></tr><tr><td valign="top"><a href="#find-2">find/2</a></td><td><code>Trie</code>内の<code>Key</code>に対応するノードの値を検索する.</td></tr><tr><td valign="top"><a href="#find_memo-2">find_memo/2</a></td><td><code>Trie</code>内の<code>Key</code>に対応するノードのメモ情報を検索する.</td></tr><tr><td valign="top"><a href="#find_node-2">find_node/2</a></td><td><code>Trie</code>内の<code>Key</code>に対応するノードを検索する.</td></tr><tr><td valign="top"><a href="#foldl-3">foldl/3</a></td><td>トライ木内の要素を左端から順番に畳み込む.</td></tr><tr><td valign="top"><a href="#foldr-3">foldr/3</a></td><td>トライ木内の要素を右端から順番に畳み込む.</td></tr><tr><td valign="top"><a href="#from_list-2">from_list/2</a></td><td>連想リストからトライ木を生成する.</td></tr><tr><td valign="top"><a href="#get_children-1">get_children/1</a></td><td>ノードの子ノード群を取得する.</td></tr><tr><td valign="top"><a href="#get_memo-1">get_memo/1</a></td><td>ノードからメモ情報を取り出す.</td></tr><tr><td valign="top"><a href="#get_root_node-1">get_root_node/1</a></td><td>木のルートノードを取得する.</td></tr><tr><td valign="top"><a href="#get_value-1">get_value/1</a></td><td>ノードに格納されている値を取り出す.</td></tr><tr><td valign="top"><a href="#get_value-2">get_value/2</a></td><td>ノードに格納されている値を取り出す.</td></tr><tr><td valign="top"><a href="#is_empty-1">is_empty/1</a></td><td>トライ木が空かどうかを判定する.</td></tr><tr><td valign="top"><a href="#is_trie-1">is_trie/1</a></td><td>Tests if <code>Value</code> is a trie and returns <code>true</code> if so and <code>false</code> otherwise.</td></tr><tr><td valign="top"><a href="#memo_fun_identity-1">memo_fun_identity/1</a></td><td>デフォルトで使用されるメモ関数.</td></tr><tr><td valign="top"><a href="#new-1">new/1</a></td><td>新しいトライ木を生成する.</td></tr><tr><td valign="top"><a href="#size-1">size/1</a></td><td>トライ木に格納されている要素の数を取得する.</td></tr><tr><td valign="top"><a href="#store-3">store/3</a></td><td><code>Trie</code>内の<code>Key</code>に対応する地点に<code>Value</code>を格納する.</td></tr><tr><td valign="top"><a href="#subtrie-2">subtrie/2</a></td><td><code>Trie</code>内の<code>Key</code>に対応する地点のサブトライ木を取り出す.</td></tr><tr><td valign="top"><a href="#to_list-1">to_list/1</a></td><td>トライ木から連想リストを生成する.</td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="children_to_list-2"></a>

### children_to_list/2 ###


<pre><code>
children_to_list(Children::<a href="#type-children">children()</a>, Trie::<a href="#type-trie">trie()</a>) -&gt; [{<a href="#type-key_component">key_component()</a>, <a href="#type-trie_node">trie_node()</a>}]
</code></pre>
<br />

子ノード群を連想リスト形式に変換する
<a name="erase-2"></a>

### erase/2 ###


<pre><code>
erase(Key::<a href="#type-key">key()</a>, Trie::<a href="#type-trie">trie()</a>) -&gt; <a href="#type-trie">trie()</a>
</code></pre>
<br />

`Trie`内の`Key`に対応するノードの値を削除する
<a name="find-2"></a>

### find/2 ###


<pre><code>
find(Key::<a href="#type-key">key()</a>, Trie::<a href="#type-trie">trie()</a>) -&gt; {ok, <a href="#type-value">value()</a>} | error
</code></pre>
<br />

`Trie`内の`Key`に対応するノードの値を検索する
<a name="find_memo-2"></a>

### find_memo/2 ###


<pre><code>
find_memo(Key::<a href="#type-key">key()</a>, Trie::<a href="#type-trie">trie()</a>) -&gt; {ok, <a href="#type-memo">memo()</a>} | error
</code></pre>
<br />

`Trie`内の`Key`に対応するノードのメモ情報を検索する
<a name="find_node-2"></a>

### find_node/2 ###


<pre><code>
find_node(Key::<a href="#type-key">key()</a>, Trie::<a href="#type-trie">trie()</a>) -&gt; {ok, <a href="#type-trie_node">trie_node()</a>} | error
</code></pre>
<br />

`Trie`内の`Key`に対応するノードを検索する
<a name="foldl-3"></a>

### foldl/3 ###


<pre><code>
foldl(Fun::<a href="#type-fold_fun">fold_fun()</a>, Initial, Trie::<a href="#type-trie">trie()</a>) -&gt; Result
</code></pre>

<ul class="definitions"><li><code>Initial = term()</code></li><li><code>Result = term()</code></li></ul>

トライ木内の要素を左端から順番に畳み込む
<a name="foldr-3"></a>

### foldr/3 ###


<pre><code>
foldr(Fun::<a href="#type-fold_fun">fold_fun()</a>, Initial, Trie::<a href="#type-trie">trie()</a>) -&gt; Result
</code></pre>

<ul class="definitions"><li><code>Initial = term()</code></li><li><code>Result = term()</code></li></ul>

トライ木内の要素を右端から順番に畳み込む
<a name="from_list-2"></a>

### from_list/2 ###


<pre><code>
from_list(Options::<a href="#type-make_opts">make_opts()</a>, List::[{<a href="#type-key">key()</a>, <a href="#type-value">value()</a>}]) -&gt; <a href="#type-trie">trie()</a>
</code></pre>
<br />

連想リストからトライ木を生成する
<a name="get_children-1"></a>

### get_children/1 ###


<pre><code>
get_children(X1::<a href="#type-trie_node">trie_node()</a>) -&gt; <a href="#type-children">children()</a>
</code></pre>
<br />

ノードの子ノード群を取得する
<a name="get_memo-1"></a>

### get_memo/1 ###


<pre><code>
get_memo(X1::<a href="#type-trie_node">trie_node()</a>) -&gt; <a href="#type-memo">memo()</a>
</code></pre>
<br />

ノードからメモ情報を取り出す
<a name="get_root_node-1"></a>

### get_root_node/1 ###


<pre><code>
get_root_node(Trie::<a href="#type-trie">trie()</a>) -&gt; <a href="#type-trie_node">trie_node()</a>
</code></pre>
<br />

木のルートノードを取得する
<a name="get_value-1"></a>

### get_value/1 ###


<pre><code>
get_value(X1::<a href="#type-trie_node">trie_node()</a>) -&gt; {ok, <a href="#type-value">value()</a>} | error
</code></pre>
<br />

ノードに格納されている値を取り出す
<a name="get_value-2"></a>

### get_value/2 ###


<pre><code>
get_value(Node::<a href="#type-trie_node">trie_node()</a>, Default::<a href="#type-value">value()</a>) -&gt; <a href="#type-value">value()</a>
</code></pre>
<br />


ノードに格納されている値を取り出す


そのような値が存在しない場合は、代わりに`Default`が返される
<a name="is_empty-1"></a>

### is_empty/1 ###


<pre><code>
is_empty(Memo_trie::<a href="#type-trie">trie()</a>) -&gt; boolean()
</code></pre>
<br />

トライ木が空かどうかを判定する
<a name="is_trie-1"></a>

### is_trie/1 ###


<pre><code>
is_trie(Value::term()) -&gt; boolean()
</code></pre>
<br />

Tests if `Value` is a trie and returns `true` if so and `false` otherwise
<a name="memo_fun_identity-1"></a>

### memo_fun_identity/1 ###


<pre><code>
memo_fun_identity(X1::<a href="#type-memo_event">memo_event()</a>) -&gt; <a href="#type-memo">memo()</a>
</code></pre>
<br />


デフォルトで使用されるメモ関数


既存のメモ値をそのまま返す
<a name="new-1"></a>

### new/1 ###


<pre><code>
new(Options::<a href="#type-make_opts">make_opts()</a>) -&gt; <a href="#type-trie">trie()</a>
</code></pre>
<br />

新しいトライ木を生成する
<a name="size-1"></a>

### size/1 ###


<pre><code>
size(Trie::<a href="#type-trie">trie()</a>) -&gt; non_neg_integer()
</code></pre>
<br />


トライ木に格納されている要素の数を取得する


この関数は、要素数に比例した(線形の)処理オーダーを要するので注意が必要
<a name="store-3"></a>

### store/3 ###


<pre><code>
store(Key::<a href="#type-key">key()</a>, Value::<a href="#type-value">value()</a>, Trie::<a href="#type-trie">trie()</a>) -&gt; <a href="#type-trie">trie()</a>
</code></pre>
<br />

`Trie`内の`Key`に対応する地点に`Value`を格納する
<a name="subtrie-2"></a>

### subtrie/2 ###


<pre><code>
subtrie(Key::<a href="#type-key">key()</a>, Trie::<a href="#type-trie">trie()</a>) -&gt; {ok, <a href="#type-trie">trie()</a>} | error
</code></pre>
<br />

`Trie`内の`Key`に対応する地点のサブトライ木を取り出す
<a name="to_list-1"></a>

### to_list/1 ###


<pre><code>
to_list(Trie::<a href="#type-trie">trie()</a>) -&gt; [{<a href="#type-key">key()</a>, <a href="#type-value">value()</a>}]
</code></pre>
<br />

トライ木から連想リストを生成する
