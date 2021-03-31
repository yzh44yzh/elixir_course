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

Мы не раз пользовались функцией inspect, которая показывает в форматированном виде любые данные. 
А блин, не, непользовались еще :)

```
TODO примеры
```

Это можно было бы реализовать примерно так:
```
def inspect(value) when is_atom(value), do: ...
def inspect(value) when is_binary(value), do: ...
```
Но в Эликсире это сделано лучше -- с помощью протоколов.

Protocols are a mechanism to achieve polymorphism in Elixir when you want behavior to vary depending on the data type.

This is where protocols can help us: protocols allow us to extend the original behavior for as many data types as we need. That’s because dispatching on a protocol is available to any data type that has implemented the protocol and a protocol can be implemented by anyone, at any time.

Рассмотрим их на примере:
TODO описать идею Calendar и CalendarItem.

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

e4 = SimpleExample.create
c = C.add_item(c, e4)
C.show(c)

** (Protocol.UndefinedError) protocol Model.CalendarItem not implemented for {:event, ...
```

```
mix dialyzer
```
dialyzir 1.1.0 -- все ок. В более старой версии было не ок

## Стандартные протоколы

Elixir comes with the following protocols:
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