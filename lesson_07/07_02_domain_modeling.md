# Моделирование предметной области

Давайте смоделируем такой достаточно сложный объект, как "встреча" (митинг, совещание).

Митинг состоит из:
- место
- время
- участники
- агенда

TODO нарисовать схему

В принципе, для моделирования сущностей любой сложности достаточно кортежей и списков. И в нашем случае это может выглядеть так:

```
$ iex -S mix
iex(1)> Event.sample_tuple_event()
{:event, "Team Meeting", ~U[2021-03-10 19:40:00.000000Z],
 {{:address, ...
```

Или можно использовать map и списки:
```
iex(3)> Event.sample_map_event()
%{
  agenda: [ ...
```

В принципе, tuple, list и map достаточно, чтобы построить любые структуры данных, любой сложности и вложенности. Этот вариант интересен как некий базовый подход, общий для всех функциональных языков.

Но, конечно, Эликсир предлагает более удобные средства:
```
iex(1)> Event.sample_struct_event()
%Model.Event.Event{ ...
```

Особенности:
- struct должна быть определена внутри модуля;
- модуль может содержать только одну struct, и у них общее имя;
- struct это абстракция поверх map;
- дублирование полей: defstruct, enforce_keys, type.


## Чтение данных из Struct

```
event.agenda
event.location.address.city

alias Model.Event, as: E
%E.Event{participants: participants} = event
%E.Event{participants: [first | _]} = event
%E.Event{location: %E.Location{room: room}} = event
room.number
```

## Модификация данных внутри Struct

Модифицировать данные на первом уровне вложенности легко:
```
event = %E.Event{event | title: "Team Gathering"}
event.title
```
Это делается так же, как и для map.

Но модифицировать данные глубже первого уровня не так просто, ведь эти данные иммутабельные. Нужно обновить каждую структуру на каждом уровне вложенности:
```
new_room = %E.Room{ room | number: 612 }
new_location = %E.Location{event.location | room: new_room}
event = %E.Event{event | location: new_location}
```
Не очень удобный подход.

Это можно сделать в одну строку:
```
event = %E.Event{event | location:
    %E.Location{event.location | room:
        %E.Room{ room | number: 611 }}}
```
Но все равно это не удобно.


## put_in, update_in для map

В Эликсир есть более удобные средства для доступа к вложеным данным и их обновления. Модуль Kernel макросы и одноименные им функции: `get_in`, `put_in`, `update_in`.

Посмотрим как они работают на примере map:
```
event = Event.sample_map_event()
```

**get_in**
```
event.location.room.number
get_in(event, [:location, :room, :number])
```
get_in имеет только форму функции, потому что чтение доступно и так, без макроса.

**put_in**
```
event = put_in(event.location.room.number, 611)
event = put_in(event, [:location, :room, :number], 612)
```
put_in имеет форму и функции, и макроса. Макрос разворачивается в одноименную функцию. А функция выполняет такое же каскадное обновление, как мы делали вручную.

**update_in**
```
event = update_in(event.location.room.number, fn(number) -> number + 10 end)
event = update_in(event, [:location, :room, :number], fn(number) -> number + 10 end)
```
Макросы подходят, когда весь путь известен статически, на этапе компиляции. Функции подходят, когда путь формируется динамически в рантайме:
```
keys = [:location, :room, :number]
event = update_in(event, keys, fn(number) -> number + 10 end)
```

Со структурами из кортежей это тоже работает:
```
> get_in(event, [Access.elem(4), Access.all(), Access.elem(1)])
["Helen", "Bob", "Kate", "Tihon"]
```
Но мы не будем вникать в тонкости использования модуля Access. Это редко используется.


## put_in, update_in для struct

Если мы попытаемся вызывать put_in, update_in для struct, то увидим, что макросы работают, а функции нет.
```
event = Event.sample_struct_event()

event = put_in(event.location.room.number, 613)

event = put_in(event, [:location, :room, :number], 612)
** (UndefinedFunctionError) function Model.Event.Event.get_and_update/3 is undefined (Model.Event.Event does not implement the Access behaviour)

event = update_in(event.location.room.number, fn(number) -> number + 10 end)

event = update_in(event, [:location, :room, :number], fn(number) -> number + 10 end)
** (UndefinedFunctionError) function Model.Event.Event.get_and_update/3 is undefined (Model.Event.Event does not implement the Access behaviour)
```

Это потому, что при компиляции сразу генерируется нужный код. А в рантайме нужно, чтобы объект, на котором мы вызываем эти функции, реализовал Access behaviour. Мы пока не изучали, что такое behaviour, так что не будем углубляться. В большинстве случаев хватает макросов.
