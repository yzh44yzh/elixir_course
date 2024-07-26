# Struct с указанием типов

## Элементы статической типизации в Эликсир

Хотя Эликсир является языком с динамической типизацией, он все же опционально поддерживает и статическую типизацию.

Статическая типизация:
- позволяет исключить определенный класс ошибок;
- дает компилятору больше информации для оптимизации кода;
- улучшает самодокументируемость и читабельность кода.

TODO
The Design Principles of the Elixir Type System
Giuseppe Castagna, Guillaume Duboc, and José Valim
Gradual Set-Theoretic Types
v1.17

В случае с Эликсир есть нюансы, так как компилятор игнорирует атрибуты статической типизации, а все проверки выполняются отдельным тулом **dialyzer** уже после того, как код скопилирован.

https://erlang.org/doc/man/dialyzer.html
Dialyzer, a DIscrepancy AnaLYZer for ERlang programs.

Этот тул изначально был создан для Эрланг. Но он умеет проверять и исходный код Эрланг, и скомпилированный байт-код. В случае с Эликсир dialyzer не может работать с исходным кодом, а проверяет только байт-код.

https://hexdocs.pm/elixir/typespecs.html


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

TODO stopped here

Давайте намеренно сделаем ошибку и посмотрим, как компилятор и dialyzer будет реагировать на нее:

```elixir
  defmodule Location do
    @type t :: %__MODULE__{
      address: Address.t,
      room: Broom.t
    }
    @enforce_keys [:address, :room]
    defstruct [:address, :room]
  end
```

Собираем проект:

```shell
$ mix compile
Compiling 5 files (.ex)
Generated event app
```

Компилятор никак не реагирует на указание несуществующего типа.


## Подключаем dialyzer

Поскольку dialyzer разработан для Эрланг, напрямую использовать его в Эликсир проекте сложно. К счастью, есть сторонняя библиотека, которая делает эту задачу тривиальной.

Библиотеку нужно указать как зависимость в файле mix.exs:

```elixir
  defp deps do
    [
      {:dialyxir, "~1.2", only: [:dev], runtime: false}
    ]
  end
```

Пока не будем вдаваться в детали, про управление зависимостями будет отдельная тема.

Подключаем зависимость и собираем проект:

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

```shell
Starting Dialyzer
...
Total errors: 1, Skipped: 0, Unnecessary Skips: 0
done in 0m1.11s
:0:unknown_type
Unknown type: Broom.t/0.
```

Ошибка найдена. Исправим ее и запустим анализ снова:

```shell
Starting Dialyzer
...
Total errors: 0, Skipped: 0, Unnecessary Skips: 0
done in 0m1.12s
done (passed successfully)
```

dialyzer проверяет не только типы данных, но и правильность вызовов функций (вызванные функции и модули действительно существуют), правильность передачи им аргументов, неиспользуемые ветки кода и другие проблемы. Если типы данных не указаны, то во многих случаях dialyzer может вывести их сам.

К сожалению, многие разработчики этот тул игнорируют. Но чаще бывает, что используют, но нерегулярно. Из-за того, что dialyzer запускается отдельно от компилятора, его просто забывают запускать. В результате в коде проекта накапливаются невыявленные проблемы, и потом бывает сложно их исправить.

Эту проблему решает встраивание запуска dialyzer в процесс CI, наряду с запуском тестов.
