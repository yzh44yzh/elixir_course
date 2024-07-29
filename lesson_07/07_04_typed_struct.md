# Struct с указанием типов

## Элементы статической типизации в Эликсир

Хотя Эликсир является языком с динамической типизацией, он все же опционально поддерживает и статическую типизацию.

Статическая типизация полезна, она:
- позволяет исключить определенный класс ошибок;
- дает компилятору больше информации для оптимизации кода;
- улучшает самодокументируемость и читабельность кода.

В Эликсир с версии 1.17 внедряется система типов [Gradual Set-Theoretic Types](https://hexdocs.pm/elixir/main/gradual-set-theoretic-types.html), которую Жозе Валим разрабатывает вместе с опытными в этом деле людьми. Но это дело будущего, пока она работает очень ограничено.

Я планирую посвятить этой теме отдельные видео. Но в данном курсе я про эту систему типов рассказывать не буду.

Вместо этого мы рассмотрим **Dialyzer** -- старый инструмент, который появился ещё в Эрланг, а позже был адаптировани и для Эликсир.

[Dialyzer](https://erlang.org/doc/man/dialyzer.html), a DIscrepancy AnaLYZer for ERlang programs -- это статический анализатор, который ищёт проблемы в коде. И помимо прочего он выполняет проверку типов. Работает не идеально, но пользу приносит.

Чтобы Dialyzer приносил больше пользы нужно указывать в своём коде [описания типов](https://hexdocs.pm/elixir/typespecs.html).


## Описываем типы для структур

Вернемся к нашему проекту и создадим модель на базе структур с описанием типов.

```
  defmodule Place do
    @type t() :: %Place{
            office: String.t(),
            room: String.t()
          }

    @enforce_keys [:office, :room]

    defstruct [:office, :room]
  end
```

Принято создать тип с именем `t`, который описывает структуру в модуле. Тип может иметь любое имя, но модули в стандартной библиотеке используют имя `t`:
- String.t()
- Regexp.t()
- DateTime.t()
- Duration.t()

Здесь мы три раза перечислили все поля структуры. Очевидно это не лучший синтаксис, но это то, что предлагает Эликсир из коробки.

К счастью, есть сторонние библиотеки, которые позволяют избежать такого дублирования путём применения макросов. И есть упомянутая раньше библиотека **Ecto**, которая предлагает абстракцию **Schema**, и она делает то же самое -- генерирует структуры с помощью макросов без дублирования кода. Но сейчас мы изучаем "Эликсир из коробки".

Мы можем хотя бы не повторять имя модуля дважды:

```
  defmodule Participant do
    @type t() :: %__MODULE__{
            name: String.t(),
            role: atom()
          }

    @enforce_keys [:name]

    defstruct [:name, :role]
  end
```

Макрос `%__MODULE__` раскрывается в имя модуля, внутри которого он находится. Громоздкая и не очень красивая конструкция. Зато если мы переименовываем модуль, что бывает не так уж редко, то макрос позволяет менять имя в одном месте, а не в нескольких местах.

Некоторые типы являются базовыми, и их имена унаследованы от Эрланга:
- atom()
- number()
- boolean()

```
  defmodule Topic do
    @type t() :: %__MODULE__{
            subject: String.t(),
            priority: :high | :medium | :low,
            description: String.t()
          }

    @enforce_keys [:subject]

    defstruct [
      :subject,
      {:priority, :medium},
      :description
    ]
  end
```

Тип можно описать как множество возможных значений: `priority: :high | :medium | :low`. Это **перечисляемый тип**. И на самом деле именно он и должен называться **Enum**, как в большинстве других языков программирования. Но в Эликсир название **Enum** почему-то дано модулю для работы с коллециями.

```
  defmodule Event do
    @type t() :: %__MODULE__{
            title: String.t(),
            place: Place.t(),
            time: DateTime.t(),
            participants: [Participant.t()],
            agenda: [Topic.t()]
          }

    @enforce_keys [:title, :place, :time, :participants, :agenda]

    defstruct [:title, :place, :time, :participants, :agenda]
  end
```

Типы-контейнеры содержат внутри себя другие типы.

Список можно описать так: `[inner_type()]` или так: `list(inner_type())`. Оба варианта работают, но на практике чаще встревается первый.

Словарь можно описать так: `%{key_type() => value_type()}` или так: `map(key_type(), value_type())`. Здесь тоже первый вариант всречается чаще. Но не редко словарь указывают просто как `map()` без указания внутренних типов.

Создадим экземпляр встречи:

```
defmodule MyCalendar do
  ...
  def sample_event_typed_struct() do
    alias MyCalendar.Model.TypedStruct, as: TS

    place = %TS.Place{office: "Office #1", room: "Room 123"}
    time = ~U[2024-07-05 15:00:00Z]

    participants = [
      %TS.Participant{name: "Kate", role: :project_manager},
      %TS.Participant{name: "Bob", role: :developer},
      %TS.Participant{name: "Bill", role: :qa}
    ]

    agenda = [
      %TS.Topic{subject: "Release MyCalendar 1.0", description: "disscuss release"},
      %TS.Topic{subject: "Buy cookies", description: "disscuss cookies", priority: :low}
    ]

    %TS.Event{
      title: "Team Meeting",
      place: place,
      time: time,
      participants: participants,
      agenda: agenda
    }
  end
end
```

И запустим проект:

```
iex(1)> MyCalendar.sample_event_typed_struct()
%MyCalendar.Model.TypedStruct.Event{
  title: "Team Meeting",
  ...
```


## Подключаем dialyzer

Поскольку dialyzer разработан для Эрланг, использовать его напрямую в Эликсир проекте не получится. Нужна библиотека, которая адаптирует его для Эликсир.

Я не хотел использовать сторонние библиотеки в данном курсе, но в этом случае сделаю исключение.

Библиотеку нужно указать как зависимость в файле mix.exs:

```elixir
  defp deps do
    [
      {:dialyxir, "~> 1.4"}
    ]
  end
```

Загружаем зависимость и собираем проект:

```shell
mix deps.get
mix compile
```

И теперь можно запускать dialyzer:

```shell
mix dialyzer
```

Первый запуск будет долгим. Сперва dialyzer формирует специальный файл PLT (Persistent Lookup Table), куда включена информация обо всех модулях проекта, сторонних библиотеках, стандартных библиотеках Эликсира и Эрланга. Такой файл уникален для проекта и используемых в нем версий Эликсир и Эрланг.

Затем PLT используется для статического анализа кода проекта. Повторные запуски работают быстро, потому что PLT-файл уже существует и его не нужно создавать заново. Иногда его нужно будет обновлять: при смене версий Эликсир и Эрланг или обновлении библиотек.

Результат анализа:

```
Finding suitable PLTs
Checking PLT...
...
Starting Dialyzer
...
Total errors: 0, Skipped: 0, Unnecessary Skips: 0
done in 0m3.47s
done (passed successfully)
```

TODO stopped here

Давайте намеренно сделаем ошибку и посмотрим, как компилятор и dialyzer будет реагировать на нее:

```elixir
  defmodule Event do
    @type t() :: %__MODULE__{
            title: String.t(),
            place: Location.t(),
            time: DateTime.t(),
            participants: [Participant.t()],
            agenda: [Topic.t()]
          }
```

Мы указали несуществующий тип `Location.t()`.

Компилятор не находит эту ошибку, dialyzer находит:

```
Total errors: 1, Skipped: 0, Unnecessary Skips: 0
done in 0m3.43s
lib/model/event_typed_struct.ex:43:unknown_type
Unknown type: Location.t/0.
```

Сделаем другую ошибку:

```
  defmodule Event do
    @type t() :: %__MODULE__{
            title: String.t(),
            place: place(),
            time: DateTime.t(),
            participants: [Participant.t()],
            agenda: [Topic.t()]
          }
```

Снова укажем несуществующий тип, но на этот раз без ссылки на модуль, где он, якобы, описан:

```
 $ mix compile
Compiling 1 file (.ex)

== Compilation error in file lib/model/event_typed_struct.ex ==
** (Kernel.TypespecError) lib/model/event_typed_struct.ex:41: type place/0 undefined (no such type in MyCalendar.Model.TypedStruct.Event)
```

Теперь ошибку видит и компилятор тоже.

Dialyzer нужно применять сразу со старта проекта. Добавить его в большой проект, который изначально разрабатывался без проверок dialyzer может быть довольно трудно.

А чтобы не забывать его запускать, его стоит вклчюить в процесс CI, наряду с запуском тестов.
