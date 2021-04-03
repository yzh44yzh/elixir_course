# Протокол

Мы уже знаем, что модуль Enum работает с самыми разными коллециями: list, map, string, range и другими.

```
iex(1)> Enum.map([1,2,3], fn(i) -> i * 2 end)
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
def map(value, fn) when is_list(value), do: ...
def map(value, fn) when is_map(value), do: ...
```

Такой вариант плохо масштабируется. Нам пришлось бы вносить изменения в модуль Enum каждый раз, когда мы хотим добавить новую коллекцию. В Эликсире это сделано лучше -- с помощью протоколов.

Протоколы очень похожи на интерфейсы в Java. Они описывают некий набор функций, с известными аргументами и возвращаемыми значениями, но без реализации.

Например, протокол Enumerable описывает какие функции должна реализовать новая коллекция, чтобы модуль Enum мог с ней работать. Добавление новой коллекции для Enum реализуется в модуле коллекции, а не в модуле Enum. Таким образом коллекции можно добавлять без изменений в Enum.

Рассмотрим пример. Допустим, у нас есть модуль Calendar, который умеет хранить и отображать некие CalendarItem с привязкой ко времени. CalendarItem описаны протоколом, так что Calendar не знает, какие конкретные реализации уже есть и еще будут. Например, у нас уже есть несколько Event с разными реализациями, которые хотелось бы добавлять в Calendar. А потом, вероятно, появятся еще какие-то CalendarItem.


Модуль Calendar будет очень простым. Он умеет принимать item, хранить их в списке, и как-то отображать.
```
defmodule Model.Calendar do
...
```

Протокол CalendarItems описывает две функции: datetime и description:
```
defprotocol Model.CalendarItem do
...
```

Теперь реализуем этот протокол для наших Event разного типа:
```
defimpl Model.CalendarItem, for: [Model.Event.Event, Model.TypedEvent.Event] do
...
defimpl Model.CalendarItem, for: Map do
...
```

И теперь Calendar может работать с ними:

```
alias Model.Calendar, as: C
c = C.new

e1 = TypedStructExample.create
e2 = StructExample.create
e3 = SimpleExample.create_map
c = C.add_item(c, e1)
c = C.add_item(c, e2)
c = C.add_item(c, e3)
C.show(c)
```

У нас есть еще одна реализация Event, на базе кортежа. Мы не реализовывали протокол CalendarItem для нее. И если мы попробуем добавить в календарь такое событие:
```
e4 = SimpleExample.create
c = C.add_item(c, e4)
C.show(c)

** (Protocol.UndefinedError) protocol Model.CalendarItem not implemented for {:event, ...
```
то получим исключение.


```
mix dialyzer
```
dialyzir 1.1.0 -- все ок. В более старой версии было не ок

TODO проверить, поймает ли диалайзер попытку добавить e4 в календарь?


## Стандартные протоколы

В Elixir есть 5 стандартных протоколов:
- Enumerable
- Collectable
- Inspect
- List.Chars
- String.Chars

TODO
- просмотреть доку по каждому протоколу
- и эту доку https://hexdocs.pm/elixir/Protocol.html
- описать, для чего нужен каждый.

Enum module which provides many functions that work with any data structure that implements the Enumerable protocol:

String.Chars protocol, which specifies how to convert a data structure to its human representation as a string. It’s exposed via the to_string function


The Inspect protocol is the protocol used to transform any data structure into a readable textual representation.

Мы не раз видели в iex, как отображаются разные значения. Это делает функция Kernel.inspect

```
TODO примеры
```

inspect не редко используется в логировании
```
require Logger
Logger.info("data is #{inspect data})
```
