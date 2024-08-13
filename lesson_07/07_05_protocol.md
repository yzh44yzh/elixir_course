# Протокол

Мы уже знаем, что модуль Enum работает с самыми разными коллециями: List, Map, String, Range и другими.

```elixir-iex
iex(1)> Enum.map([1, 2, 3], fn i -> i * 2 end)
[2, 4, 6]

iex(2)> Enum.map('Hello', fn char -> char + 1 end)
'Ifmmp'

iex(3)> Enum.map(1..10, fn i -> i * i end)
[1, 4, 9, 16, 25, 36, 49, 64, 81, 100]

iex(5)> Enum.map(%{a: 1, b: 2}, fn {k, v} -> {v, k} end)
[{1, :a}, {2, :b}]
```

Это можно было бы реализовать примерно так:

```
defmodule Enum do

  def map(collection, f) when is_list(collection), do: ...

  def map(collection, f) when is_map(collection), do: ...

  def reduce(collection, acc, f) when is_list(collection), do: ...

  def reduce(collection, acc, f) when is_map(collection), do: ...

  def filter(collection, f) when is_list(collection), do: ...

  def filter(collection, f) when is_map(collection), do: ...

do
```

В Эрланг есть `:lists.map/2`, которая работает только со списками, и `:maps.map/2`, которая работает со словарями. То же самое для `filter` и для `fold`. То есть, там сделаны отдельные реализации каждой функции для каждой коллекции.

Этот вариант плохо масштабируется. Если в языке появляется новая коллекция, то для неё нужно делать новые реализации всех таких функций.

Протокол решает эту проблему. Как и Behaviour, он похож на интерфейсы в Java. Протокол описывает некий набор функций с известными аргументами и возвращаемыми значениями.

Например, протокол Enumerable описывает какие функции должна реализовать новая коллекция, чтобы модуль Enum мог с ней работать. Протокол реализуется в модуле коллекции, а не в модуле Enum. И после этого все функции модуля Enum могут работать с новой коллекцией.


## Calendar

Давайте сделаем модуль Calendar, который умеет хранить и отображать те события, которые мы моделировали раньше.

Модуль будет очень простым -- он умеет принимать события, хранить их в списке и как-то отображать.


```
defmodule MyCalendar.Model do
  defmodule Calendar do
    alias MyCalendar.Model.CalendarItem

    @type t :: %__MODULE__{
            items: [CalendarItem.t()]
          }

    @enforce_keys [:items]
    defstruct [:items]

    @spec add_item(Calendar.t(), CalendarItem.t()) :: Calendar.t()
    def add_item(calendar, item) do
      items = [item | calendar.items]
      %__MODULE__{calendar | items: items}
    end

    @spec show(Calendar.t()) :: String.t()
    def show(calendar) do
      ...
    end
  end
end
```

События реализованы на разных структурах данных, но модуль Calendar будет работать с ними через протокол CalendarItem, который мы опишем так:

```
  defprotocol CalendarItem do
    @spec get_title(CalendarItem.t()) :: String.t()
    def get_title(item)

    @spec get_time(CalendarItem.t()) :: DateTime.t()
    def get_time(item)
  end
```

Компилятор Эликсир автоматически создает тип для протокола, в нашем случае это `CalendarItem.t()`. И мы можем использовать этот тип в спецификациях функций `add_item` и `show`.


Теперь добавим реализацию функции `show`:

```
defmodule MyCalendar.Model do
  defmodule Calendar do
    ...

    @spec show(Calendar.t()) :: String.t()
    def show(calendar) do
      Enum.map(
        calendar.items,
        fn item ->
          title = CalendarItem.get_title(item)
          time = CalendarItem.get_time(item) |> DateTime.to_iso8601()
          "#{title} at #{time}"
        end
      )
      |> Enum.join("\n")
    end
  end
```

Реализуем этот протокол CalendarItem для словарей:

```elixir
  defimpl CalendarItem, for: Map do
    @spec get_title(CalendarItem.t()) :: String.t()
    def get_title(item), do: Map.get(item, :title, "No Title")

    @spec get_time(CalendarItem.t()) :: DateTime.t()
    def get_time(item), do: Map.get(item, :time)
  end
```

И теперь Calendar может работать с ними:

```elixir-iex
defmodule MyCalendar do
  ...
  def sample_calendar() do
    alias MyCalendar.Model.Calendar

    %Calendar{items: []}
    |> Calendar.add_item(sample_event_map())
  end
end
```

TODO: event_tuple()
```
  defimpl CalendarItem, for: Tuple do
    @spec get_title(CalendarItem.t()) :: String.t()
    def get_title({:event, title, _, _, _, _}), do: title

    @spec get_time(CalendarItem.t()) :: DateTime.t()
    def get_time({:event, _, _, time, _, _}), do: time
  end

```

Попробуем в консоли, как это работает:

```
iex(1)> cal = MyCalendar.sample_calendar()
iex(2)> MyCalendar.Model.Calendar.show(cal) |> IO.puts
```

Если мы добавим событие типа `MyCalendar.Model.Struct.Event`:
```
    %Calendar{items: []}
    |> Calendar.add_item(sample_event_map())
    |> Calendar.add_item(sample_event_struct())
```

То при вызове `show` получим ошибку:

```
iex(2)> MyCalendar.Model.Calendar.show(cal) |> IO.puts
** (Protocol.UndefinedError) protocol MyCalendar.Model.CalendarItem not implemented for %MyCalendar.Model.Struct.Event{...
```

Добавим реализацию протокола для таких событий:

```
  defimpl CalendarItem, for: MyCalendar.Model.Struct.Event do
    @spec get_title(CalendarItem.t()) :: String.t()
    def get_title(event), do: event.title

    @spec get_time(CalendarItem.t()) :: DateTime.t()
    def get_time(event), do: event.time
  end

  defimpl CalendarItem, for: MyCalendar.Model.TypedStruct.Event do
    @spec get_title(CalendarItem.t()) :: String.t()
    def get_title(event), do: event.title

    @spec get_time(CalendarItem.t()) :: DateTime.t()
    def get_time(event), do: event.time
  end
```

Добавим эти события в календарь:

```
    %Calendar{items: []}
    |> Calendar.add_item(sample_event_map())
    |> Calendar.add_item(sample_event_struct())
    |> Calendar.add_item(sample_event_typed_struct())
```

И теперь функция `show` работает.

Однако для структур принято определять протоколы внутри модуля структуры. Уберём реализации протокола в модуле `MyCalendar.Model` и добавим в модули структур:

```
defmodule MyCalendar.Model.Struct do
  ...

  defmodule Event do
    alias MyCalendar.Model.CalendarItem

    ...

    defimpl CalendarItem do

      @spec get_title(CalendarItem.t()) :: String.t()
      def get_title(event), do: event.title

      @spec get_time(CalendarItem.t()) :: DateTime.t()
      def get_time(event), do: event.time
    end
  end
end
```

и

```
defmodule MyCalendar.Model.TypedStruct do
  ...

  defmodule Event do
    alias MyCalendar.Model.CalendarItem

    ...

    defimpl CalendarItem do

      @spec get_title(CalendarItem.t()) :: String.t()
      def get_title(event), do: event.title

      @spec get_time(CalendarItem.t()) :: DateTime.t()
      def get_time(event), do: event.time
    end
  end
end
```

Функция `show` по-прежнему работает.


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
defmodule Model.AuthData do
  defstruct [:login, :password]
end

> data1 = %Model.AuthData{login: "Tihon", password: "123"}
%Model.AuthData{login: "Tihon", password: "123"}
```

С помощью атрибута @derive можно указать, какие поля скрывать или показывать в реализации Inpsect для этой структуры:

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
