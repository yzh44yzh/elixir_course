# Протокол

Мы уже знаем, что модуль Enum работает с самыми разными коллециями: List, Map, String, Range и другими.

```elixir-iex
iex(1)> Enum.map([1, 2, 3], fn(i) -> i * 2 end)
[2, 4, 6]

iex(2)> Enum.map('Hello', fn(char) -> char + 1 end)
'Ifmmp'

iex(3)> Enum.map(1..10, fn(i) -> i * i end)
[1, 4, 9, 16, 25, 36, 49, 64, 81, 100]

iex(5)> Enum.map(%{a: 1, b: 2}, fn({k, v}) -> {v, k} end)
[{1, :a}, {2, :b}]
```

Это можно было бы реализовать примерно так:

```
defmodule Enum do

  def map(value, fn) when is_list(value), do: ...

  def map(value, fn) when is_map(value), do: ...

do
```

Такой вариант плохо масштабируется. Нам пришлось бы вносить изменения в модуль Enum каждый раз, когда мы хотим добавить новую коллекцию. В Эликсире это сделано лучше -- с помощью протоколов.

Протоколы очень похожи на интерфейсы в Java. Они описывают некий набор функций, с известными аргументами и возвращаемыми значениями, но без реализации.

Например, протокол Enumerable описывает какие функции должна реализовать новая коллекция, чтобы модуль Enum мог с ней работать. Протокол реализуется в модуле коллекции, а не в модуле Enum. Таким образом коллекции можно добавлять без изменений в Enum.

## Calendar

Рассмотрим пример. Допустим, у нас есть модуль Calendar, который умеет хранить и отображать некие CalendarItem с привязкой ко времени. CalendarItem описаны протоколом, так что Calendar не знает, какие конкретные реализации уже есть и еще будут. Например, у нас уже есть несколько Event с разными реализациями, которые хотелось бы добавлять в Calendar. А потом, вероятно, появятся еще какие-то CalendarItem.

Модуль Calendar будет очень простым. Он умеет принимать item, хранить их в списке и как-то отображать.

```elixir
defmodule Model.Calendar do
...
```

Протокол CalendarItems описывает две функции: datetime и description:

```elixir
defprotocol Model.CalendarItem do
...
```

Теперь реализуем этот протокол для наших Event разного типа:

```elixir
defimpl Model.CalendarItem, for: [Model.Event.Event, Model.TypedEvent.Event] do
...
defimpl Model.CalendarItem, for: Map do
...
```

И теперь Calendar может работать с ними:

```elixir-iex
alias Model.Calendar, as: C
c = C.new

e1 = Event.sample_typed_struct_event()
e2 = Event.sample_struct_event()
e3 = Event.sample_map_event()
c = C.add_item(c, e1)
c = C.add_item(c, e2)
c = C.add_item(c, e3)
C.show(c)
```

У нас есть еще одна реализация Event, на базе кортежа. Мы не реализовывали протокол CalendarItem для нее. И если мы попробуем добавить в календарь такое событие:

```elixir-iex
e4 = Event.sample_tuple_event()
c = C.add_item(c, e4)
C.show(c)

** (Protocol.UndefinedError) protocol Model.CalendarItem not implemented for {:event, ...
```
то получим исключение.

Компилятор Эликсир автоматически создает тип для протокола, в нашем случае `CalendarItem.t`. И мы можем использовать этот тип в описании функции:

```elixir
@spec add_item(t, CalendarItem.t) :: t
```

К сожалению, dialyzer не ловит ошибку:

```elixir-iex
c = C.add_item(c, e4)
```

Так что мы узнаем об этом только в рантайме.

Даже в таких очевидных случаях:

```elixir
Enum.map(:not_an_enum, fn(i) -> i end)
```

dyalyzer не находит ошибок.

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

Enumerable -- пожалуй, самый часто используемый протокол. Модуль Enum находится в центре любой работы с коллекциями, и любая коллекция реализует Enumerable.

Модуль Stream тоже работает с коллекциями через этот протокол.

Протокол содержит 4 функции: count, member?, reduce, slice. Все многообразие функций модуля Enum реализовано через эти 4 функции.

Теоретически достаточно только reduce. Все остальное -- count, map, filter, slice, any, take и даже sort можно реализовать через reduce. Но на практике это не очень эффективно. Например, member? для Map выполняется за константное время, тогда как реализация на основе reduce была бы O(n).

Enumerable.reduce это не то же самое, что Enum.reduce. Там более сложная реализация, где можно управлять итерацией -- останавливать, возобновлять и прерывать. Это позволяет более эффективно реализовывать остальные функции, не проходя всю коллекцию до конца, если это не нужно, или итерироваться по двум коллециям одновременно.

### Collectable

https://hexdocs.pm/elixir/Collectable.html

Этот протокол в некотором роде противоположность Enumerable. Если идея Enumerable -- итерироваться по коллекции, то есть по очереди извлекать из нее элементы, то идея Collectable -- собирать коллекцию, добавляя в нее элементы.

Зачем это нужно? Это тоже нужно для модуля Enum. Дело в том, что функции модуля Enum на входе принимают любые коллекции, но на выходе у них всегда список. А что, если нужен не список? Для этого есть функция Enum.into, которая преобразует список в другую коллекцию, реализующую Collectable.

```elixir-iex
iex(1)> my_map = %{a: 1, b: 2}
%{a: 1, b: 2}
iex(3)> Enum.map(my_map, fn({k, v}) -> {k, v + 1} end)
[a: 2, b: 3]
iex(4)> Enum.map(my_map, fn({k, v}) -> {k, v + 1} end) |> Enum.into(%{})
%{a: 2, b: 3}
```

Протокол Collectable описывает, как получить нужную коллекцию из списка. Протокол содержит только одну функцию -- into. И каждая коллекция, которая хочет работать с Enum.into, реализует её.

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
iex(4)> event = StructExample.create()
%Model.Event.Event{
  agenda: [
```

Здесь многострочное форматирование с вложенными отступами и подсветка синтаксиса.

Это делает функция `Kernel.inspect/2`. А делает она это благодаря тому, что все типы данных в Эликсир реализуют протокол Inspect.

Каждый тип сам описывает, как он должен быть представлен в консоли. Описание является не просто строкой, а документом в специальном формате Inspect.Algebra, который позволяет по-разному представлять структуру в зависимости от настроек (Inspect.Opts).

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

Кроме `Kernel.inspect/2` есть еще функция `IO.inspect/3`, которая позволяет направить вывод в файл или в консоль. В реальных проектах это не очень нужно, так как есть Logger. Но IO.inspect полезен в pipeline для отладки, чтобы посмотреть промежуточные результаты.

Например, у нас есть такой pipeline:

```elixir-iex
iex(24)> data = [1, 2, 3]
[1, 2, 3]
iex(25)> data |>
...(25)> Enum.map(fn(i) -> i * i end) |>
...(25)> Enum.sum()
14
```

Мы можем использовать IO.inspect так:

```elixir-iex
data |>
IO.inspect(label: "step 1") |>
Enum.map(fn(i) -> i * i end) |>
IO.inspect(label: "step 2") |>
Enum.sum()

step 1: [1, 2, 3]
step 2: [1, 4, 9]
14
```

Как мы уже видели, протокол Inspect по умолчанию работает с нашими event любой реализации. Но иногда бывает необходимо реализовать протокол явно. Например, чтобы скрыть некоторые поля наших структур, чтобы они не выводились в лог. Это важно для приватной информации -- пароли, ключи, сертификаты.

Допустим, у вас есть стуктура AuthData, содержащая логин и пароль:

```elixir-iex
> data1 = %Model.AuthData{login: "Tihon", password: "123"}
%Model.AuthData{login: "Tihon", password: "123"}
```

С помощью атрибута @derive можно указать, какие поля скрывать или показывать в реализации Inpsect для этой структуры:

```elixir-iex
> data2 = %Model.AuthDataS{login: "Tihon", password: "123"}
#Model.AuthDataS<login: "Tihon", ...>
```

Или можно явно реализовать Inspect:

```elixir-iex
> data3 = %Model.AuthDataC{login: "Tihon", password: "123"}
#AuthData<login:"Tihon">
```

### String.Chars и List.Chars

https://hexdocs.pm/elixir/String.Chars.html

https://hexdocs.pm/elixir/List.Chars.html

Эти два протокола позволяют конвертировать данные в строку.

String.Chars -- в строку в двойных кавычках, то есть в бинарные данные в UTF8.

List.Chars -- в строку в одиночных кавычках, то есть в список Unicode Codepoints.

В отличие от Inspect не все типы реализуют эти протоколы по умолчанию:

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
