# Keyword list, синтаксический сахар и макросы

Этот материал факультативный и его можно спокойно пропустить.

Но если вам интересно, как работает синтаксис Эликсир, и почему в однострочном `do` есть запятая и двоеточие:
```
def my_fun(a), do: a
```
а в многострочном `do` их нет:
```
def my_fun(a) do
  a
end
```
то не пропускайте :)


## Синтаксический сахар для Keyword list

Мы про это уже говорили, когда разбирали Keyword List, но давайте повторим. Это нам пригодится дальше.

Возьмем функцию, которая принимает список опций как Keyword List:

```elixir
def my_fun(arg1, options) do
  IO.puts("my_fun called with arg1:#{arg1} and options:#{inspect options}")
end
```

Вызовем её:

```elixir
iex(2)> options = [a: 42, b: 100]
[a: 42, b: 100]
iex(3)> DoEnd.my_fun(42, options)
my_fun called with arg1:42 and options:[a: 42, b: 100]
:ok
```

В этой ситуации можно опустить квадратные скобки для options:

```elixir
iex(4)> DoEnd.fun_with_options(42, a: 42, b: 500, c: 100)
fun called with arg1:42 and options:[a: 42, b: 500, c: 100]
:ok
```

Более того, во многих случаях при вызове функции можно опускать круглые скобки:

```elixir
iex(5)> DoEnd.fun_with_options 42, a: 42, b: 500, c: 100
fun called with arg1:42 and options:[a: 42, b: 500, c: 100]
```

Раньше это была распространённая практика. Потом оказалось, что трудно реализовать парсер языка так, чтобы это работало везде. Поэтому вызов функции без круглых скобок работает не везде, и его объявили deprecated.

Но это полезно знать, потому что это повсеместно используется для макросов.


## Какая магия скрывается за макросом if ?

Определим функцию `if_1` и в ней используем макрос if:

```elixir
def if_1(condition) do
  if condition do
    a = 42
    {:true_branch, a + a}
  else
    b = 50
    {:false_branch, b * 2}
  end
end
```

Вызовем, посмотрим как это работает:
```elixir
> DoEnd.if_1(true)
{:true_branch, 84}
> DoEnd.if_1(false)
{:false_branch, 100}
```

С функцией `DoEnd.fun_with_options` мы постепенно добавляли синтаксический сахар. Теперь пойдем в обратную сторону, и начнем постепенно убирать его.

Заменим многострочный `do..end` на однострочный:

```elixir
def if_2(condition) do
  if condition, do: (
    a = 42
    {:true_branch, a + a}
  ),
  else: (
    b = 50
    {:false_branch, b * 2}
  )
end

iex(31)> DoEnd.if_2(false)
{:false_branch, 100}
```

Добавим квадратные скобки:

```elixir
def if_3(condition) do
  if(condition, [
    do: (
      a = 42; {:true_branch, a + a}
    ),
    else: (
      b = 50; {:false_branch, b * b}
    )
  ])
end

iex(32)> DoEnd.if_3(true)
{:true_branch, 84}
```

И запишем всё так, чтобы уместить в одну строку:

```elixir
def if_4(condition) do
  code_block_1 = (a = 42; {:true_branch, a + a})
  code_block_2 = (b = 50; {:false_branch, b * b})
  if(condition, [{:do, code_block_1}, {:else, code_block_2}])
end

iex(33)> DoEnd.if_4(true)
{:true_branch, 84}
```

И вот мы дошли до полной формы макроса, без сахара:

```elixir
if(condition, [{:do, code_block_1}, {:else, code_block_2}])
```

Мы видим, что вызов этого макроса выглядит так же, как и вызов функции `DoEnd.fun_with_options/2`.

Макрос `if` принимает 2 аргумента: предикат и keyword list с двумя элементами. Первый элемент имеет ключ `:do`, второй элемент имеет ключ `:else`. А значениями этих ключей являются блоки кода.


## Макросы def и defmodule

Аналогичными макросами являются `defmodule`, `def`, `defp`:

Макрос `def` для создания функции мы тоже можем привести к такому виду:

Начинаем с привычного синтаксиса:
```elixir
def some_fun_1(arg1, arg2) do
  a = 42
  arg1 + arg2 + a
end
```

Используем однострочный `do`:
```
def some_fun_2(arg1, arg2), do: (a = 42; arg1 + arg2 + a)
```

Добавляем квадратные скобки:
```
def( some_fun_3(arg1, arg2), [{:do, (a = 42; arg1 + arg2 + a)}] )
```

И получаем его настоящую форму, без синтаксического сахара:
```
def(function_signature, [{:do, code_block}])
```

И все эти формы работают одинаково:

```
iex(5)> DoEnd.some_fun_1(1, 2)
45
iex(6)> DoEnd.some_fun_2(1, 2)
45
iex(7)> DoEnd.some_fun_3(1, 2)
45
```

Попробуем то же самое с макросом `defmodule`:

```
defmodule(MyModule, [{:do, (def f1(), do: 42; def f2(), do: 100)}])
```

Это тоже работает:

```
iex(8)> DoEnd.MyModule.f1()
42
iex(9)> DoEnd.MyModule.f2()
100
```


## Блок do-end

Как мы видим, блок do-end является кортежем из 2-х элементов и может записываться в двух видах:

```elixir
do
   line1
   line2
   line3
end
```

или:

```elixir
do: (line1; line2; line3)
```

Однострочный вариант называется **do: form**. Он удобен в случаях, когда в блоке только одна строка кода.

Вспомним пример из 3-го урока, где мы реализовали троичную логику Стивена Клини:

```elixir
def sk_and(false, _), do: false
def sk_and(nil, false), do: false
def sk_and(nil, _), do: nil
def sk_and(true, second_arg), do: second_arg
```

И теперь мы понимаем, у этой формы именно такой синтаксис. Потому что это keyword list, который передаётся аргументом в макрос `def`.
