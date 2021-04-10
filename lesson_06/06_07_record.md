# Record

https://hexdocs.pm/elixir/Record.html

Records are simply tuples where the first element is an atom.

This module provides conveniences for working with records at compilation time, where compile-time field names are used to manipulate the tuples, providing fast operations on top of the tuples' compact structure.

In Elixir, records are used mostly in two situations:
- to work with short, internal data
- to interface with Erlang records

```
iex(1)> RecordExample.create
{:event, "Team Meeting", ~U[2021-03-10 19:40:00.000000Z],
 {:location, {:address, "Belarus", "Minsk", "Partizanskij pr", 178},
  {:room, 6, 610}},
 [
   {:participant, :human, "Helen", :project_manager},
   {:participant, :cat, "Tihon", :cate}
 ], [{:topic, :medium, "buying food for cat"}]}
```

Чтение данных:
```
> import Model.RecordEvent.Event
Model.RecordEvent.Event
> event(ev, :title)
"Team Meeting"

> import Model.RecordEvent.Location
Model.RecordEvent.Location
> loc = event(ev, :location)
{:location, {:address, "Belarus", "Minsk", "Partizanskij pr", 178},
 {:room, 6, 610}}
> location(loc, :room)
{:room, 6, 610}
```

Модификация данных:
```
> ev = event(ev, title: "Food for cat")
{:event, "Food for cat", ~U[2021-03-10 19:40:00.000000Z],

> import Model.RecordEvent.Room
Model.RecordEvent.Room
> rm = location(loc, :room)
{:room, 6, 610}
> rm = room(rm, number: 611)
{:room, 6, 611}
> loc = location(loc, room: rm)
{:location, {:address, "Belarus", "Minsk", "Partizanskij pr", 178},
 {:room, 6, 611}}
> ev = event(ev, location: loc)
{:event, "Food for cat", ~U[2021-03-10 19:40:00.000000Z],
 {:location, {:address, "Belarus", "Minsk", "Partizanskij pr", 178},
  {:room, 6, 611}},
 [
   {:participant, :human, "Helen", :project_manager},
   {:participant, :cat, "Tihon", :cate}
 ], [{:topic, :medium, "buying food for cat"}]}
```


Record мало известен среди Эликсир разработчиков. И, вероятно, вы не будете их использовать. Тогда почему они включены в курс?

Потому что я, автор курса, Эрланг-разработчик :) Если вы будете встречаться и общаться с Эрланг разработчиками, то знание о Record подымет ваш авторитет в их глазах :)

Шучу. На самом деле Record полезен, и сейчас мы узнаем почему.


KV структуры, статические (фиксированный набор полей) и динамические.

Настоящий статических структур нет ни в Эрланге ни в Эликсире. Но есть по-разному удачные (или по-разному неудачные) попытки создать эти структуры на базе существующих.

В Эликсир создали Struct на базе Map, из-за чего структура на уровне абстракции языка считается статической, но по реализации остается динамической.

В Эрланг пошли другим путем, и создали Record на базе Tuple. Получился другой компромис с другими особенностями.
Преимущество: эффективно по CPU и по памяти.
Недостаток: нет информации об именах ключей в рантайме. Что сильно затрудняет создание чего-то похожего на Ecto или Phoenix. Попытки были (ChicagoBoss), но не взлетели.


Struct vs Record внешне одинаковые, разница во внутреннем представлении: map vs tuple, соотвественно, в расходе памяти.
в Эликсире выбрали Struct, в Эрланге Record.

historical, used before maps

Record -- более естественное представление ADT, чем Struct.

Активно используются в Эрланг, поэтому могут понадобится при использовании эрланговских библиотек.
