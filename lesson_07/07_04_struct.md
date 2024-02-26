# Использование Struct

Конечно, Эликсир предлагает более удобные средства, чем map и tuple.

```elixir-iex
iex(1)> Event.sample_struct_event()
%Model.Struct.Event{ ...
```

Особенности:
- struct должна быть определена внутри модуля;
- модуль может содержать только одну struct, и у них общее имя;
- struct это абстракция поверх map;
- дублирование полей: defstruct, enforce_keys, type.

## Чтение данных из Struct

```elixir-iex
event.agenda
event.location.address.city

alias Model.Struct, as: S
%S.Event{participants: participants} = event
%S.Event{participants: [first | _]} = event
%S.Event{location: %S.Location{room: room}} = event
room.number
```

## Модификация данных внутри Struct

Модифицировать данные на первом уровне вложенности легко:

```elixir-iex
event = %S.Event{event | title: "Team Gathering"}
event.title
```

Это делается так же, как и для map.

Но модифицировать данные глубже первого уровня не так просто, ведь эти данные иммутабельные. Нужно обновить каждую структуру на каждом уровне вложенности:

```elixir-iex
new_room = %S.Room{ room | number: 612 }
new_location = %S.Location{event.location | room: new_room}
event = %S.Event{event | location: new_location}
```

Не очень удобный подход.

Это можно сделать в одну строку:

```elixir-iex
event = 
  %S.Event{event | location:
    %S.Location{event.location | room:
      %S.Room{ room | number: 611 }}}
```

Но все равно это не удобно.

## put_in, update_in для struct

Если мы попытаемся вызывать put_in, update_in для struct, то увидим, что макросы работают, а функции нет.

```elixir-iex
event = Event.sample_struct_event()

event = put_in(event.location.room.number, 613)

event = put_in(event, [:location, :room, :number], 612)
** (UndefinedFunctionError) function Model.Event.Event.get_and_update/3 is undefined (Model.Event.Event does not implement the Access behaviour)

event = update_in(event.location.room.number, fn(number) -> number + 10 end)

event = update_in(event, [:location, :room, :number], fn(number) -> number + 10 end)
** (UndefinedFunctionError) function Model.Event.Event.get_and_update/3 is undefined (Model.Event.Event does not implement the Access behaviour)
```

Для функций нужно, чтобы наша struct реализовала поведение Access behaviour. Для Map такая реализация есть по умолчанию. А для каждой struct это нужно реализовать явно.

Что касается макросов, для для всех struct их реализация генерируется на этапе компиляции. Поэтому для struct удобнее пользоваться макросами, чем функциями.

## Struct vs Map

При моделировании сущностей не редко обходятся просто Map, не прибегая к созданию Module и Struct для каждого случая.

Часто это бывает при активном использовании JSON как формата обмена данными между клиентом и сервером и между серверами. Map напрямую сериализируется в JSON, и наоборот, JSON десериализируется в Map. А вот чтобы превратить Struct в JSON и обратно, нужно затратить больше усилий.

(Сериализация Map to Struct реализована в библиотеке Ecto с помощью Schema и Changeset.)

Важно знать, что Struct -- это уровень абстракции поверх Map. Эта абстракция существует на этапе компиляции, но в рантайме любая Struct это не более, чем Map с дополнительным ключом, указывающим ее тип:

```elixir-iex
> event = StructExample.create
> event.__struct__
Model.Event.Event
> i event
> event_m = Map.from_struct(event)
> i event_m
```

Поэтому почти все, что можно сделать с Map, точно так же можно сделать и со Struct.
