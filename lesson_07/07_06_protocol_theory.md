# Протокол, теория.

## Стандартные протоколы

В Elixir есть 5 стандартных протоколов:
- Enumerable
- Collectable
- Inspect
- List.Chars
- String.Chars

Рассмотрим их по-очереди.


### Enumerable

https://hexdocs.pm/elixir/Enumerable.html

**Enumerable** -- пожалуй, самый часто используемый протокол. Модуль **Enum** находится в центре любой работы с коллекциями, и любая коллекция реализует Enumerable.

Модуль **Stream** тоже работает с коллекциями через этот протокол.

Протокол содержит 4 функции: `count`, `member?`, `reduce`, `slice`. Все многообразие функций модуля Enum реализовано через эти 4 функции.

Теоретически достаточно только `reduce`. Все остальное -- `count`, `map`, `filter`, `any`, `take` и даже `sort` можно реализовать через `reduce`. Но на практике это не очень эффективно. Например, `member?` для Map выполняется за константное время, тогда как реализация на основе `reduce` была бы `O(n)`.

`Enumerable.reduce` это не то же самое, что `Enum.reduce`. Там более сложная реализация, где можно управлять итерацией -- останавливать, возобновлять и прерывать. Это позволяет более эффективно реализовывать остальные функции, не проходя всю коллекцию до конца, если это не нужно, или итерироваться по двум коллециям одновременно.


### Collectable

https://hexdocs.pm/elixir/Collectable.html

Этот протокол в некотором роде противоположность Enumerable. Если идея Enumerable -- итерироваться по коллекции, то есть по очереди извлекать из нее элементы, то идея **Collectable** -- собирать коллекцию, добавляя в нее элементы.

Зачем это нужно? Это тоже нужно для модуля Enum. Дело в том, что функции модуля Enum на входе принимают любые коллекции, но на выходе у них всегда список. А что, если нужен не список? Для этого есть функция `Enum.into`, которая преобразует список в другую коллекцию, реализующую Collectable.

```elixir-iex
iex(1)> my_map = %{a: 1, b: 2}
%{a: 1, b: 2}
iex(3)> Enum.map(my_map, fn({k, v}) -> {k, v + 1} end)
[a: 2, b: 3]
iex(4)> Enum.map(my_map, fn({k, v}) -> {k, v + 1} end) |> Enum.into(%{})
%{a: 2, b: 3}
```

Протокол Collectable описывает, как получить нужную коллекцию из списка. Протокол содержит только одну функцию -- `into`. И каждая коллекция, которая хочет работать с Enum.into, реализует её.

Аналогично это работает с конструкторами списков:

```elixir-iex
iex(8)> for {k, v} <- my_map, do: {k, v + 1}
[a: 2, b: 3]
iex(9)> for {k, v} <- my_map, into: %{}, do: {k, v + 1}
%{a: 2, b: 3}
```

### Inspect

Мы не раз видели в iex консоли, как отображаются разные значения. В том числе такие сложные, как наши event.

```elixir-iex
iex(1)> MyCalendar.sample_event_typed_struct()
%MyCalendar.Model.TypedStruct.Event{
  title: "Team Meeting",
  ...
```

Здесь многострочное форматирование с вложенными отступами и подсветка синтаксиса.

Это делает функция `Kernel.inspect/2`. А делает она это благодаря тому, что все типы данных в Эликсир реализуют протокол **Inspect**.

Каждый тип сам описывает, как он должен быть представлен в консоли. Описание является не просто строкой, а документом в специальном формате `Inspect.Algebra`, который позволяет по-разному представлять структуру в зависимости от настроек `(Inspect.Opts)`.

```elixir-iex
inspect(event)
inspect(event, pretty: true)
inspect(event, pretty: true, limit: 3)
inspect(event, pretty: true, width: 10)
```

inspect не редко используется в логировании:

```elixir-iex
require Logger
Logger.info("my event is #{inspect(event)}")
Logger.info("my event is #{inspect(event, pretty: true, width: 10)}")
```

Как мы уже видели, протокол Inspect по умолчанию работает с нашими event любой реализации. Но иногда бывает необходимо реализовать протокол явно. Например, чтобы скрыть некоторые поля наших структур, чтобы они не выводились в лог. Это важно для приватной информации -- пароли, ключи, сертификаты.

Допустим, у нас есть структура **AuthData**, содержащая логин и пароль:

```elixir-iex
defmodule Model.AuthData do
  defstruct [:login, :password]
end

> data1 = %Model.AuthData{login: "Tihon", password: "123"}
%Model.AuthData{login: "Tihon", password: "123"}
```

С помощью атрибута `@derive` можно указать, какие поля скрывать или показывать в реализации Inspect для этой структуры:

```elixir-iex
defmodule Model.AuthData do
  @derive {Inspect, only: [:login]}

  defstruct [:login, :password]
end

> data2 = %Model.AuthData{login: "Tihon", password: "123"}
#Model.AuthDataDerive<login: "Tihon", ...>
```

Или можно явно реализовать Inspect:

```elixir-iex
defmodule Model.AuthData do
  defstruct [:login, :password]
end

defimpl Inspect, for: Model.AuthData do

  def inspect(auth_data, opts) do
    "Login: #{auth_data.login}"
  end
end

> data3 = %Model.AuthData{login: "Tihon", password: "123"}
Login: Tihon
```

### String.Chars и List.Chars

https://hexdocs.pm/elixir/String.Chars.html

https://hexdocs.pm/elixir/List.Chars.html

Эти два протокола позволяют конвертировать данные в строку.

**String.Chars** -- в строку в двойных кавычках, то есть в бинарные данные в UTF8.

**List.Chars** -- в строку в одиночных кавычках, то есть в список Unicode Codepoints.

В отличие от Inspect, не все типы реализуют эти протоколы по умолчанию:

```elixir-iex
iex(10)> IO.puts("here is a number: #{42}")
here is a number: 42
:ok
iex(11)> IO.puts("here is a map: #{ %{a: 42} }")
** (Protocol.UndefinedError) protocol String.Chars not implemented for
...
iex(11)> IO.puts("here is a map: #{ inspect(%{a: 42}) }")
here is a map: %{a: 42}
:ok
```

Но их можно явно реализовать для любого типа:

```elixir-iex
iex(1)> ce = %CharsExample{a: 42, b: 500}
%CharsExample{a: 42, b: 500}
iex(2)> "ce is #{ce}"
"ce is #CharsExample<a=42, b=500>"
```

В том числе и для Map:

```elixir-iex
iex(3)> my_map = Map.from_struct(ce)
%{a: 42, b: 500}
iex(4)> "my_map is #{my_map}"
"my_map is #Map of size:2"
```


## Protocol vs Behaviour

Behavirour (поведение) нужно определять в том модуле, который реализует это поведение. Поэтому его можно реализовать только для своих модулей, но не для модулей стандартной библиотеки или чужих библиотек.

Protocol можно определить в любом месте. Его можно определить и для модулей стандартной библиотеки (и мы это делали для Map и для Tuple) и для модулей чужих библиотек. Очевидно, что протоколы гибче.

Behavirour нужно использовать только для совместимости с Эрланг-модулями и Эрланг-библиотеками. Это может понадобится не так уж и редко, потому что Эликсир под капотом часто использует Эрланг модули.
