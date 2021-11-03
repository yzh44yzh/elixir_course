# Application

Приложение -- сущность специфичная для BEAM, и не имеет прямых аналогов в других языках программирования.

С одной стороны application сродни **package**, потому что объединяет несколько модулей. С другой стороны application похож на **pod** в **kubernetes**, потому что объединяет несколько процессов в единую группу с общим жизненным циклом и общей конфигурацией.

Приложение это:
- пакет из нескольких Эликсир (или Эрланг) модулей;
- группа процессов с общим жизненным циклом, собранных в дерево супервизии;
- организация модулей и ресурсов в файловой системе стандартным способом;
- система конфигурации, работающая на этапе компиляции и в рантайме.

Библиотеки тоже принято оформлять как приложения. При этом библиотеки могут не иметь всех перечисленных выше компонентов. Не редко они сводятся только к первому пункту -- пакет из нескольких модулей. 
TODO: примеры.
Но так же не редко встречаются библиотеки являющиеся полноценные приложениями. 
TODO: примеры: cowboy и др.

Обычно проект состоит из нескольких приложений:

Во-первых, это приложения, непосредственно реализующие проект. То есть, это код, который мы пишем сами.

Во-вторых, это используемые библиотеки, прямые и транзитивные зависимости.

В-третьих, это приложения, входящие в состав OTP. Например, приложение для работы с сетью **inets**, приложения, отвечающие за шифрование **crypto** и **ssl**, приложение для модульного тестирования **eunit** и другие.
TODO: актуальный список приложений, kernel и stdlib.

Если просто запусить iex консоль, то в ней уже запущены 6 приложений:
```
iex(1)> Application.started_applications()
[
  {:logger, 'logger', '1.11.3'},
  {:iex, 'iex', '1.11.3'},
  {:elixir, 'elixir', '1.11.3'},
  {:compiler, 'ERTS  CXC 138 10', '7.6.3'},
  {:stdlib, 'ERTS  CXC 138 10', '3.13.2'},
  {:kernel, 'ERTS  CXC 138 10', '7.1'}
]
```

Эрланг консоль проще, в ней запущены всего 2 приложения:
```
1> application:which_applications().
[{stdlib,"ERTS  CXC 138 10","3.13.2"},
 {kernel,"ERTS  CXC 138 10","7.1"}]
```


## Жизненный цикл приложения

Жизненный цикл приложений состоит из трёх этапов:
- загрузка;
- запуск;
- остановка.

На этапе **загрузки** виртуальная машина загружает с диска **файл ресурсов (resource file)**, содержащий всю необходимую информацию о приложении. Затем устанавливает зависимости между приложениями, и загружает их.

На этапе **запуска** виртуальная машина загружает байткод модулей приложения и вызывает модуль и функцию, которые указаны в файле ресурсов. Обычно это модуль, реализующий **Application behaviour**. Запускается корневой супервизор, и разворачивается дерево супервизоров. После этого приложение готово к работе.

На этапе **остановки** завершается дерево супервизоров в очередности, противоположной запуску. То есть, сперва завершаются рабочие потоки, потом дочерние супервизоры, и последним завершается корневой супервизор.

Все эти процессы автоматизированы. На машине разработчика их реализует **mix**, а на удаленной машине их реализуют специальные скрипты запуска системы, составляющие **release**. 


## Файл ресурсов

Каждое приложение имеет файл ресурсов (resource file), содержащий всю необходимую информацию для загрузки и запуска. 

Давайте создадим пустое приложение и посмотрим, где находится этот файл, и как он выглядит.

```
$ mix new my_cool_app
$ cd my_cool_app
$ mix compile
$ cat _build/dev/lib/my_cool_app/ebin/my_cool_app.app
{application,my_cool_app,
             [{applications,[kernel,stdlib,elixir,logger]},
              {description,"my_cool_app"},
              {modules,['Elixir.MyCoolApp']},
              {registered,[]},
              {vsn,"0.1.0"}]}.
```

Файл ресурсов не нужно создавать вручную, он генерируется при сборке. Данные в нем представлены в виде эрланговских (не эликсировских) структур.

Здесь мы видим:
- имя приложения;
- зависимость от других приложений;
- человекочитаемое описание приложения;
- список модулей, входящих в состав приложения;
- список глобальных имен, под которыми регистрируются процессы;
- версия приложения.

Эта информация позволяет скриптам запуска проверить зависимости между приложениями, проверить наличие конфликтов в регистрируемых именах процессов, установить правильный порядок запуска и загрузить нужные модули.

Часть этой информации известна при сборке, например, имена модулей. Другая часть должна быть указана в `mix.exs` -- конфигурационном файле для mix.

В нашем пустом проекте mix.exs выглядит так:
```
defmodule MyCoolApp.MixProject do
  use Mix.Project

  def project do
    [
      app: :my_cool_app,
      version: "0.1.0",
      elixir: "~> 1.11",
      start_permanent: Mix.env() == :prod,
      deps: deps()
    ]
  end

  def application do
    [
      extra_applications: [:logger]
    ]
  end

  defp deps do
    [
    ]
  end
end
```
Отсюда берутся имя и версия приложения, а так же список других приложений, от которых зависит наше. Здесь указан только `:logger`. Остальные зависимости, `:elixir`, `:strlib`, `:kernel` подключаются неявно. 

Также mix неявно считает, что точкой запуска приложения является модуль `MyCoolApp`. Это можно переопределить, указав ключ `:mod`:
```
def application do
  [
    extra_applications: [:logger],
    mod: {MyMod, [some_args]}
  ]
end
```

Так же можно явно указать имена процессов:
```
def application do
  [
    extra_applications: [:logger],
    mod: {MyMod, [some_args]},
    registered: [:agent_1, :agent2, PathFinder]
  ]
end
```


## Application Behaviour

По умолчанию mix генерирует точку входа в приложение как модуль с именем, совпадающим с именем приложения:
```
defmodule MyCoolApp do
  @moduledoc """
  Documentation for `MyCoolApp`.
  """

  @doc """
  Hello world.

  ## Examples

      iex> MyCoolApp.hello()
      :world

  """
  def hello do
    :world
  end
end
```

Однако этот модуль должен реализовать `application behaviour`, который включает один обязательный обработчик: `start/2` и несколько необязательных (`pre_stop/1`, `stop/1` и др).

```
defmodule MyCoolApp do
  use Application

  @impl true
  def start(_start_type, start_args) do
    {:ok, self()}
  end
end
```

mix запускает приложение сразу на старте:

```
$ mix compile
$ iex -S mix
iex(1)> Application.started_applications()
[
  {:my_cool_app, 'my_cool_app', '0.1.0'},
  ...
]
```

При желании приложение можно запустить явно:

```
$ iex -S mix run --no-start
iex(1)> Application.started_applications()
[
  {:mix, 'mix', '1.11.3'},
  {:logger, 'logger', '1.11.3'},
  {:iex, 'iex', '1.11.3'},
  {:elixir, 'elixir', '1.11.3'},
  {:compiler, 'ERTS  CXC 138 10', '7.6.3'},
  {:stdlib, 'ERTS  CXC 138 10', '3.13.2'},
  {:kernel, 'ERTS  CXC 138 10', '7.1'}
]
iex(2)> Application.start(:my_cool_app)
:ok
iex(3)> Application.started_applications()
[
  {:my_cool_app, 'my_cool_app', '0.1.0'},
  ...
]
```


### start/2

TODO start_callback -- объяснить аргументы
TODO запустить корневой супервизор в MyCoolApp

To implement the Application behaviour, we have to use Application and define a start/2 function. The goal of start/2 is to start a supervisor, which will then start any child services or execute any other code our application may need.

When an application starts, developers may configure a callback module that executes custom code. Developers use this callback to start the application supervision tree.

The start/2 callback has to spawn and link a supervisor and return {:ok, pid} or {:ok, pid, state}, where pid is the PID of the supervisor, and state is an optional application state. args is the second element of the tuple given to the :mod option.

The type argument passed to start/2 is usually :normal unless in a distributed setup where application takeovers and failovers are configured. Distributed applications are beyond the scope of this documentation.


### stop/1

TODO

When an application is shutting down, its stop/1 callback is called after the supervision tree has been stopped by the runtime. This callback allows the application to do any final cleanup. The argument is the state returned by start/2, if it did, or [] otherwise. The return value of stop/1 is ignored.

By using Application, modules get a default implementation of stop/1 that ignores its argument and returns :ok, but it can be overridden.

Application callback modules may also implement the optional callback prep_stop/1. If present, prep_stop/1 is invoked before the supervision tree is terminated. Its argument is the state returned by start/2, if it did, or [] otherwise, and its return value is passed to stop/1.

Stopping an application with a callback module has three steps:
- If present, invoke the optional callback prep_stop/1.
- Terminate the top-level supervisor.
- Invoke the required callback stop/1.
step 2 is a blocking one. 



