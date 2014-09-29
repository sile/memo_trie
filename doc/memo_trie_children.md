

# Module memo_trie_children #
* [Description](#description)
* [Data Types](#types)


ノードの子供群管理用モジュール用のインタフェース定義.
Copyright (c) 2014, Takeru Ohta <phjgt308@gmail.com>


__This module defines the `memo_trie_children` behaviour.__<br /> Required callback functions: `empty/0`, `is_empty/1`, `store/3`, `find/2`, `take/2`, `to_list/1`.

<a name="types"></a>

## Data Types ##




### <a name="type-child">child()</a> ###



<pre><code>
child() = <a href="memo_trie.md#type-trie_node">memo_trie:trie_node()</a>
</code></pre>





### <a name="type-children_module">children_module()</a> ###



<pre><code>
children_module() = module()
</code></pre>





### <a name="type-key">key()</a> ###



<pre><code>
key() = <a href="memo_trie.md#type-key_component">memo_trie:key_component()</a>
</code></pre>





### <a name="type-state">state()</a> ###



<pre><code>
state() = term()
</code></pre>


