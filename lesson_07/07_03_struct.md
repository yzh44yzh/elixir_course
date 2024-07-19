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
```

Синтаксис простой, внутри модуля мы указываем макрос `defstruct` и список полей, которые должна иметь структура.

Создаём экземпляр:

```
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

Мы можем указать значения по-умолчанию для полей:
```
  defmodule Topic do
    defstruct [
      :subject,
      {:priority, :medium},
      :description
    ]
  end
```

И тогда экземпляр можно создать не указывая все поля:

```
iex(4)> alias MyCalendar.Model.Struct.Topic
MyCalendar.Model.Struct.Topic
iex(5)> topic = %Topic{subject: "Interview", description: "Job Interview"}
%MyCalendar.Model.Struct.Topic{
  subject: "Interview",
  priority: :medium,
  description: "Job Interview"
}
iex(6)> topic = %Topic{subject: "Interview", description: "Job Interview", priority: :high}
%MyCalendar.Model.Struct.Topic{
  subject: "Interview",
  priority: :high,
  description: "Job Interview"
}
```

На самом деле значение по умолчанию есть у всех полей, так что можно никакие поля не указывать:
```
%MyCalendar.Model.Struct.Topic{
  subject: nil,
  priority: :medium,
  description: nil
}
```

Понятно, что в таком случае значением по-умолчанию будет `nil`.

И это не всегда хорошо. Часто бывает необходимо сделать некоторые поля обязательными, так что бы нельзя было создать экземпляр без их указания. Это делается с помощью аттрибута модуля `@enforce_keys`:

```
  defmodule Topic do
    @enforce_keys [:subject]
    defstruct [
      :subject,
      {:priority, :medium},
      :description
    ]
  end
```

И теперь если мы попытаемся создать экземпляр не указав `subject`, то получим исключение:
```
iex(7)> topic = %Topic{}
** (ArgumentError) the following keys must also be given when building struct MyCalendar.Model.Struct.Topic: [:subject]

iex(9)> topic = %Topic{subject: "Interview"}
%MyCalendar.Model.Struct.Topic{
  subject: "Interview",
  priority: :medium,
  description: nil
}
```

Структура -- это абстракция поверх словарей. Она существует на этапе компиляции, но в рантайме любая структура превращается в словарь с дополнительным ключом `__struct__`, указывающим ее тип:

```elixir-iex
iex(11)> topic.__struct__
MyCalendar.Model.Struct.Topic
iex(13)> topic_m = Map.from_struct(topic)
%{priority: :medium, description: nil, subject: "Interview"}
iex(14)> i topic
iex(15)> i topic_m
```

Реализуем все структуры для нашей модели:

```
defmodule MyCalendar.Model.Struct do

  defmodule Place do
    @enforce_keys [:office, :room]
    defstruct [:office, :room]
  end

  defmodule Participant do
    @enforce_keys [:name]
    defstruct [:name, :role]
  end

  defmodule Topic do
    @enforce_keys [:subject]
    defstruct [
      :subject,
      {:priority, :medium}, # :high | :medium | :low
      :description
    ]
  end

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

end
```

Создадим событие:

```
  def sample_event_struct() do
    alias MyCalendar.Model.Struct, as: S

    place = %S.Place{office: "Office #1", room: "Room 123"}
    time = ~U[2024-07-05 15:00:00Z]
    participants = [
      %S.Participant{name: "Kate", role: :project_manager},
      %S.Participant{name: "Bob", role: :developer},
      %S.Participant{name: "Bill", role: :qa}
    ]
    agenda = [
      %S.Topic{subject: "Release MyCalendar 1.0", description: "disscuss release"},
      %S.Topic{subject: "Buy cookies", description: "disscuss cookies", priority: :low}
    ]
    %S.Event{
      title: "Team Meeting",
      place: place,
      time: time,
      participants: participants,
      agenda: agenda
    }
  end
```

И создадим экземпляр:

```
iex(17)> event = MyCalendar.sample_event_struct()
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

Но прежде нужно объяснить, что это такое. И тут есть некоторая сложность, потому что в Эликсире есть **Protocol** (протокол) и **Behaviour** (поведение) -- две разные сущности, которые немного по-разному делают одно и тоже. Так получилось, потому что Behaviour достался в наследство от Эрланг. А Protocol -- это уже собственная абстракция Эликсир.

(Позже мы увидим аналогичную ситуацию с исключениями, где часть функциональности унаследована от Эрланг, а часть специфична для Эликсир, и эти части не очень хорошо друг с другом сочетаются).

Обе абстракции -- и Protocol и Behaviour, являются аналогом Interface в Jave или Trait в Rust. Они описывают какие функции с какими аргументами должен реализовать модуль, чтобы его можно было использовать в некотором контексте.

Протокол мы рассмотрим позже, а сейчас нам нужно поведение. Оно состоит из двух частей:
- Behaviour module
- Callback module

Behaviour module указывает, какие функции обратного вызова (callback) он собирается вызываеть. В нашем случае это модуль `Access`. И он будет вызвать такие функции:
- fetch(term, key)
- get_and_update(data, key, function)
- pop(data, key)

Callback module реализует эти функции у себя. В нашем случае это модуль `Place`:
```
  defmodule Place do
    @behaviour Access
    @enforce_keys [:office, :room]
    defstruct [:office, :room]

    @impl true
    def fetch(place, key) do
      TODO
    end

    @impl true
    def get_and_update(place, key, f) do
      TODO
    end

    @impl true
    def pop(place, key) do
      TODO
    end
  end
```

Здесь мы указываем аттрибут `@behaviour Access`. Компилятор заглядывает в модуль `Access`, находит в нём описания callback, которые должны быть реализованы. Каждый callback мы помечаем аттрибутом `@impl true`, так что компилятор может проверить, что все они реализованы.

Полная реализация:

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

Теперь мы можем проверить, как это работает.

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

Для `replace_participant` нам нужно идентифицировать участника. По-хорошему, это должен быть уникальный id. Но мы не предусмотрели такой id, так что будем использовать name. (Что годится для учебных целей, но не годится для реального проекта).

```
    def replace_participant(
          %Event{participants: participants} = event,
          %Participant{} = updated_participant
        ) do
      participants = Enum.filter(participants, fn p ->
        p.name != updated_participant.name
      end)
      %Event{event | participants: [updated_participant | participants]}
    end
```

Запускаем:
```
iex(22)> bill = %MyCalendar.Model.Struct.Participant{name: "Bill", role: :devops}
iex(24)> MyCalendar.Model.Struct.Event.replace_participant(event, bill)
```

## Struct vs Map

При моделировании сущностей не редко обходятся просто словарями, не прибегая к созданию модулей и структур для каждого случая.

Часто это бывает при активном использовании JSON как формата обмена данными. Словари легко сериализируются в JSON, и наоборот.

А вот чтобы сериализовать/десериализовать структуру в JSON, нужно затратить больше усилий. Поскольку структура имеет статические ключи, то при десериализации её из JSON нужно ещё обеспечить валидацию.

Возможен комбинированный подход, когда важные, ключевые сущности в проекте описаны структурами, а второстепенные словарями.

Однако, многие эликсировские проекты используют библиотеку Ecto. А в ней есть ещё один уровень абстракции над структурами -- схемы (Schema). Они решают проблему сериализации, десериализации и валидации данных.

Но мы не будем изучать библиотеку Ecto сейчас. Цель этого курса -- изучить язык Эликсир, те возможности, которые предлагает он сам, из коробки, без сторонних библиотек.

Я планирую другой курс: "Веб-разработка на Эликсир", где обязательно будут использоваться фреймворк Phoneix и библиотека Ecto.
