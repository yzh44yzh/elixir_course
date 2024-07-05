# Создание проекта

До сих пор мы работали с одиночными эликсир-скриптами. Чтобы двигаться дальше, нам понадобится взаимодействие кода из нескольких файлов и подключение внешних зависимостей, то есть проект.

Для работы с проектом используется утилита **mix**, которая поставляется вместе с Эликсир.

```shell
mix help
```

mix умеет создавать проекты, управлять зависимостями, собирать и запускать их, запускать тесты.

Создадим проект **my_calendar**:

```shell
mix new my_calendar
* creating README.md
* creating .formatter.exs
* creating .gitignore
* creating mix.exs
* creating lib
* creating lib/my_calendar.ex
* creating test
* creating test/test_helper.exs
* creating test/my_calendar_test.exs

Your Mix project was created successfully.
...
```

И рассмотрим его структуру:

```shell
tree my_calendar
my_calendar
├── lib
│   └── my_calendar.ex
├── mix.exs
├── README.md
└── test
    ├── my_calendar_test.exs
    └── test_helper.exs

2 directories, 5 files
```

В корне проекта есть два каталога (*lib* и *test*) и два файла (*mix.exs* и *README.md*). Еще есть скрытые файлы *.formatter.exs* и *.gitignore*.

Каталог *lib* содержит исходный код проекта. Каталог *test*, очевидно, тесты.

Файл *mix.exs* описывает конфигурацию, необходимую для mix. Этот файл представляет собой эликсир-скрипт, который интерпретируется каждый раз, когда mix выполняет какую-либо команду.

Заглянем в него. Здесь 3 блока конфигурации: project, application и deps.

```
defmodule MyCalendar.MixProject do
  use Mix.Project

  def project do
    [
    ...
    ]
  end

  # Run "mix help compile.app" to learn about applications.
  def application do
    [
    ...
    ]
  end

  # Run "mix help deps" to learn about dependencies.
  defp deps do
    [
    ...
    ]
  end
end
```

Каждый блок традиционно представлен в виде Keyword List (кроме блока *deps*, там структура может быть сложнее).

Блок *project* содержит метаданные проекта (название, версия) и указывает требуемую версию Эликсир. Блок deps содержит описание сторонних библиотек, необходимых проекту. (Остальные настройки мы рассмотрим позже, когда они нам понадобятся). Команда `mix deps.get` подтянет нужные нам зависимости.

Соберем наш проект:

```shell
cd my_calendar
mix deps.get
mix compile
```

После сборки появляется каталог *_build*, куда компилятор складывает байткод модулей самого проекта и всех библиотек.

Наш проект пустой, запускать пока нечего, так что перейдем к следущему шагу.


## Моделирование предметной области

Давайте смоделируем такой достаточно сложный объект, как "встреча" (митинг, совещание).

Чтобы описать встречу (митинг), нужно указать:
- место (где?)
- время (когда?)
- участники (кто?)
- агенда (о чём?)

Например:
- Место: Главный офис, 7-й этаж, переговорка №71.
- Время: 4 июля 2024, 16:00.
- Участники: Вася, Катя, Петя, Лена.
- Агенда: собеседование кандидата на позицию "дрессировщик котов".

Мы реализуем все необходимы сущности несколькими способами:
- с помощью кортежей и списков
- с помощью словарей
- с помощью структур
- с помощью типизированных структур

И по ходу дела попробуем разные способы работы с одинаковыми сущностями, реализованными разными способами.


## Моделирование с помощью кортежей и списков

Я программировал на Эрланг в те времена, когда ещё не было Эликсир. Более того, я застал те времена, когда в Эрланг ещё не было словарей. Они появились в 2014 году, а до этого больше 20 лет Эрланг обходился без них.

Тогда нам хватало кортежей и списков чтобы описать любые данные. Сейчас это слишком архаичный подход. Но я испытываю некоторую ностальгию по тем временам, так что не откажу себе в удовольствии начать с него.

Моделируем сущности:
```
defmodule MyCalendar.Model.Tuple do

  defmodule Place do

    def new(office, room) do
      {:place, office, room}
    end

  end

  defmodule Participant do

    def new(name, role) do
      {:participant, name, role}
    end

  end

  defmodule Topic do

    def new(subject, description) do
      {:topic, subject, description}
    end

  end

  defmodule Event do

    def new(title, place, time, participants, agenda) do
      {:event, title, place, time, participants, agenda}
    end

  end

end
```

Создаём встречу:
```
defmodule MyCalendar do

  def sample_event_tuple() do
    alias MyCalendar.Model.Tuple, as: T

    place = T.Place.new("Office #1", "Room 123")
    time = ~U[2024-07-05 15:00:00Z]
    participants = [
      T.Participant.new("Kate", :project_manager),
      T.Participant.new("Bob", :developer),
      T.Participant.new("Bill", :qa),
    ]
    agenda = [
      T.Topic.new("Release MyCalendar 1.0", "disscuss release"),
      T.Topic.new("Buy cookies", "disscuss cookies")
    ]
    T.Event.new("Team Meeting", place, time, participants, agenda)
  end
end
```

Собираем и запускаем проект:
```
mix compile
iex -S mix

iex(1)> MyCalendar.sample_event_tuple()
{:event, "Team Meeting", {:place, "Office #1", "Room 123"},
 ~U[2024-07-05 15:00:00Z],
 [
   {:participant, "Kate", :project_manager},
   {:participant, "Bob", :developer},
   {:participant, "Bill", :qa}
 ],
 [
   {:topic, "Release MyCalendar 1.0", "disscuss release"},
   {:topic, "Buy cookies", "disscuss cookies"}
 ]}
```

Начало положено :)
