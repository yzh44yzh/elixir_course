# Использование Struct

Конечно, Эликсир предлагает более удобные средства, чем map и tuple.

Основная рабочая лошадка Эликсир для создания пользовательских сущностей -- это структура (Struct).

Определяется макросом `defstruct`, передаётся список полей. Каждая структура завёрнута в отдельный модуль:

```
defmodule MyCalendar.Model.Struct do
  defmodule Place do
    defstruct [:office, :room]
  end
end

iex(3)> my_place = %MyCalendar.Model.Struct.Place{office: "Office #1", room: "1"}
%MyCalendar.Model.Struct.Place{office: "Office #1", room: "1"}
```

Модуль может содержать только одну структуру, и у них общее имя.

Нередко имена таких модулей получаются слишком длинными, так что удобнее использовать alias:

```
iex(6)> alias MyCalendar.Model.Struct.Place
MyCalendar.Model.Struct.Place
iex(7)> other_place = %Place{office: "Office #2", room: "42"}
%MyCalendar.Model.Struct.Place{office: "Office #2", room: "42"}
```

TODO: значения по умолчанию

TODO: enforce_keys (показать, что компилятор не даёт создать struct без этих полей)

TODO: Struct это абстракция поверх map.

Важно знать, что Struct -- это уровень абстракции поверх Map. Эта абстракция существует на этапе компиляции, но в рантайме любая Struct это не более, чем Map с дополнительным ключом, указывающим ее тип:

```elixir-iex
> event = StructExample.create
> event.__struct__
Model.Event.Event
> i event
> event_m = Map.from_struct(event)
> i event_m
```


## Модификация данных внутри Struct

Модифицировать данные на первом уровне вложенности легко. Ситаксис для структур аналогичен синтаксису для словарей:

```elixir-iex
event = %S.Event{event | title: "Team Gathering"}
event.title
```

Это делается так же, как и для map.

Если мы попытаемся вызывать `put_in` или `update_in` для struct, то увидим, что макросы работают, а функции нет.

```elixir-iex
> event = MyCalendar.sample_event_struct
> put_in(event.place.room, "Room 456")

iex(12)> put_in(event, [:place, :room], "Room 789")
** (UndefinedFunctionError) function MyCalendar.Model.Struct.Event.get_and_update/3 is undefined (MyCalendar.Model.Struct.Event does not implement the Access behaviour
```

Идея тут в том, что структура имеет статические поля, использование которых проверяется на этапе компиляции. А такую проверку нельзя сделать, если путь задан динамически.

Понятно, что раз нет динамических путей, то и модуль Access использовать нельзя.

Мне эта концепция не нравится, но приходится мириться с тем, что есть.

Мы можем сделать так, чтобы динамические пути и модуль Access работали со структурами. Но для этого нужно реализовать behaviour Access.

TODO: что такое behaviour

Реализация:
```
  defmodule Place do
    @behaviour Access
    @enforce_keys [:office, :room]
    defstruct [:office, :room]

    @impl true
    def fetch(%Place{} = place, :office), do: {:ok, place.office}
    def fetch(%Place{} = place, :room), do: {:ok, place.room}
    def fetch(_, _), do: :error

    @impl true
    def get_and_update(%Place{} = place, :office, f) do
      {curr_val, new_val} = f.(place.office)
      new_place = %Place{place | office: new_val}
      {curr_val, new_place}
    end

    def get_and_update(place, _key, _f), do: {nil, place}

    @impl true
    def pop(place, _key), do: {nil, place}
  end
```

Смотрим, как это работает.

Функцию `fetch` мы реализовали для обоих ключей:
```
iex(14)> get_in(place, [:room])
"42"
iex(15)> get_in(place, [:office])
"Office #2"
```

А функцию `get_and_update` мы реализовали только для ключа `office`:
```
iex(12)> put_in(place, [:office], "Office 5")
%MyCalendar.Model.Struct.Place{office: "Office 5", room: "42"}
iex(13)> put_in(place, [:room], "Room 5")
%MyCalendar.Model.Struct.Place{office: "Office #2", room: "42"}
```
Так что попытка изменить значение для ключа `room` не работает.

Понятно, что реализовать behaviour нужно для каждой нашей структуры: Place, Participant, Topic, Event. Это работа не сложная, но большая и занудная. И в целом бессмысленная.

Для изменения ключей на первом уровне проще использовать обычный синтаксис:
```
iex(24)> %S.Place{place | room: "Room 5"}
%MyCalendar.Model.Struct.Place{office: "Office #2", room: "Room 5"}
```

А для глубоко вложенных данных лучше реализовать специфические для данной сущности функции. Например для `Event` реализовать функции `add_participant` и `replace_participant`.

```
  defmodule Event do
    @enforce_keys [:title, :place, :time, :participants, :agenda]
    defstruct [:title, :place, :time, :participants, :agenda]

    def add_participant(
          %Event{participants: participants} = event,
          %Participant{} = participant
        ) do
      %Event{event | participants: [participant | participants]}
    end
  end
```

```
iex(19)> alias MyCalendar.Model.Struct, as: S
iex(20)> john = %S.Participant{name: "John", role: :qa}
iex(21)> S.Event.add_participant(event, john)
```

TODO: реализовать replace_participant.


## Struct vs Map

При моделировании сущностей не редко обходятся просто словарями, не прибегая к созданию модулей и структур для каждого случая.

Часто это бывает при активном использовании JSON как формата обмена данными. Словари легко сериализируются в JSON, и наоборот.

А вот чтобы сериализовать/десериализовать структуру в JSON, нужно затратить больше усилий. Поскольку структура имеет статические ключи, то при десериализации её из JSON нужно ещё обеспечить валидацию.

Возможен комбинированный подход, когда важные, ключевые сущности в проекте описаны структурами, а второстепенные словарями.

Однако, многие эликсировские проекты используют библиотеку Ecto. А в ней есть ещё один уровень абстракции над структурами -- схемы (Schema). Они решают проблему сериализации, десериализации и валидации данных.

Но мы не будем изучать библиотеку Ecto сейчас. Цель этого курса -- изучить язык Эликсир, те возможности, которые предлагает он сам, из коробки, без сторонних библиотек.

Я планирую другой курс: "Веб-разработка на Эликсир", где обязательно будут использоваться фреймворк Phoneix и библиотека Ecto.
