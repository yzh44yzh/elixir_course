# Keyword list, синтаксический сахар и макросы

## Keyword list

Пары ключ-значение можно хранить в виде кортежей в списке:

```elixir
iex(1)> kwl = [{"a", 42}, {"b", 500}]
[{"a", 42}, {"b", 500}]
```

Это легаси из давних времен, когда в BEAM не было map. С появлением map необходимость в такой структуре пропала. Но по сложившейся традиции она используется до сих пор, обычно для передачи какого-нибудь списка опций.

Как и в случае с map, keyword list поддерживает синтаксический сахар для ключей-атомов:
```elixir
iex(2)> kwl = [{:a, 42}, {:b, 500}]
[a: 42, b: 500]
```

Возьмем функцию, которая принимает список опций как keyword list:

```elixir
def fun_with_options(arg1, options) do
  IO.puts("fun called with arg1:#{arg1} and options:#{inspect options}")
end
```

Такого рода функции не редко встречаются в стандартных библиотеках. Например: `GenServer.start_link(module, init_arg, options \\ [])`.

Вызовем её:

```elixir
iex(2)> DoEnd.fun_with_options(42, [a: 42, b: 500])
fun called with arg1:42 and options:[a: 42, b: 500]
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

С функцией `DoEnd.fun_with_options` мы постепенно добавляли синтаксический сахар. Теперь пойдем в обратную сторону, и начнем постепенно убирать его:

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

Уберем ещё немного сахара:

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

Продолжим:

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

Мы видим, что это тоже самое, что вызов `DoEnd.fun_with_options`.

Макрос if принимает 2 аргумента: предикат и keyword list с двумя элементами. Первый элемент имеет ключ `:do`, второй элемент имеет ключ `:else`. А значениями этих ключей являются блоки кода.


## Блок do-end

Аналогичными макросами являются `defmodule`, `def`, `defp`:

```elixir
def some_fun_1(arg1, arg2) do
  a = 42
  arg1 + arg2 + a
end

def some_fun_2(arg1, arg2), do: (a = 42; arg1 + arg2 + a)

def( some_fun_3(arg1, arg2), [{:do, (a = 42; arg1 + arg2 + a)}] )
```

Как мы видим, блок do-end является кортежем из 2-х элементов, и может записываться в двух видах:

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

Второй вариант называется **do: form**. Он удобен в случаях, когда в блоке только одна строка кода:

```elixir
def sk_and(false, _), do: false
def sk_and(nil, false), do: false
def sk_and(nil, _), do: nil
def sk_and(true, second_arg), do: second_arg
```

И теперь мы понимаем, почему у этой формы именно такой синтаксис.
