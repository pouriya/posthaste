![posthaste travis test status](https://travis-ci.org/Pouriya-Jahanbakhsh/posthaste.png?branch=master)

# posthaste
An Erlang/Elixir [hooking](https://en.wikipedia.org/wiki/Hooking) library.  

# When to use Posthaste ?
When you want to lookup callbacks of hooks (especially with large numbers of processes at the same time), Posthaste is about **3-5 times faster** than Erlang ETS. But in add/delete operations it's too slow (compared with ETS). Then if you have more lookup operation than add/delete operations, use Posthaste.

# How to use?
In Posthaste, each **hook** must be an Erlang `atom`.  
```erlang
-type hook() :: atom().
```
Each **hook** has one or more **key**.  
```erlang
-type key() :: atom() | binary() | number() | list() | tuple().
```
Finally each **key** has its own **handlers**. **handlers** is a list which may contain one or more **handler**. Each **handler** contains **priority** which is non negative `integer` and **module** and **function** which are `atom`s.  
```erlang
-type handlers() :: [] | [handlers()].
-type  handlers() :: {priority(), module(), func()}.
-type   priority() :: non_neg_integer().
-type   func() :: atom().
```
Posthaste maps each **key** to its **handlers** for each **hook**:
```erlang
1> Hooks = my_hooks.
my_hooks

%% Starting hook server process:
2> posthaste:start_link(Hooks).
{ok,<0.97.0>}

3> Hook1 = foo.
foo

4> Key1 = bar.
bar

%% Adding a hook with priority 10, module 'module' and function 'function':
5> posthaste:add(Hooks, Hook1, Key1, module, function, 10).
ok

%% Getting handlers of Key1 for Hook1:
6> posthaste:handlers(Hooks, Hook1, Key1).
[{10, module, function}]

7> posthaste:add(Hooks, Hook1, Key1, module, function, 10).
ok
8> posthaste:add(Hooks, Hook1, Key1, module2, function2, 20).
ok
9> posthaste:add(Hooks, Hook1, Key1, module2, function2, 15).
ok

10> posthaste:handlers(Hooks, Hook1, Key1).                  
[{10, module, function},
 {10, module, function},
 {15, module2, function2},
 {20, module2, function2}]

11> Key2 = baz.                                               
baz
12> posthaste:add(Hooks, Hook1, Key2, mod, func, 100).        
ok

13> posthaste:handlers(Hooks, Hook1, Key2).          
[{100,{mod,func}}]

14> posthaste:handlers(Hooks, Hook1, Key1).
[{10, module, function},
 {10, module, function},
 {15, module2, function2},
 {20, module2, function2}]

%% New hook:
15> Hook2 = qux.                                      
qux
16> posthaste:add(Hooks, Hook2, Key1, m, f, 1).        
ok
17> posthaste:handlers(Hooks, Hook2, Key1).   
[{1, m, f}]

%% Deleting a callback:
18> posthaste:delete(Hooks, Hook1, Key1, module2, function2, 15).
ok
19> posthaste:callbacks(Hooks, Hook1, Key1).                     
[{10, module,function},
 {10, module,function},
 {20, module2,function2}]
```

# What is going on? (under the hood)
Let's run above code snippet again:  
```erlang
1> Hooks = my_hooks.        
my_hooks
2> posthaste:start_link(Hooks).
{ok,<0.97.0>}
3> Hook1 = foo.
foo
4> Key1 = bar.
bar
5> posthaste:add(Hooks, Hook1, Key1, module, function, 10).
ok

6> Hooks:Hook1(Key1).
[{10, module, function}]

7> my_hooks:foo(bar).
[{10, module, function}]
```
Actually each Posthaste server process creates new Erlang module and adds every **hook** as an Erlang function to it. **key** is an argument of **hook** function and finally **callbacks** are in function body.

### Contributing
I love pull requests from everyone. But it's good to explain idea, feautre or bug in issues before.

### Author
**`pouriya.jahanbakhsh@gmail.com`**

### License
**`BSD 3-Clause`**

### Hex version
[**`18.5.19`**](https://hex.pm/packages/posthaste)
