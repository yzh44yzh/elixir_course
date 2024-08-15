# Протокол, практика.

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

У нас ещё остался 4-й тип событий `MyCalendar.Model.Tuple.Event`. Давайте и для них тоже реализуем протокол:

```
  defimpl CalendarItem, for: Tuple do
    @spec get_title(CalendarItem.t()) :: String.t()
    def get_title({:event, title, _, _, _, _}), do: title

    @spec get_time(CalendarItem.t()) :: DateTime.t()
    def get_time({:event, _, _, time, _, _}), do: time
  end
```

И добавим такое событие в календарь:

```
    %Calendar{items: []}
    |> Calendar.add_item(sample_event_map())
    |> Calendar.add_item(sample_event_struct())
    |> Calendar.add_item(sample_event_typed_struct())
    |> Calendar.add_item(sample_event_tuple())
```

Функция `show` по-прежнему работает.
