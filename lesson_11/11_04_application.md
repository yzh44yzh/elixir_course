# Application

An application is a component implementing some specific functionality, with a standardized directory structure, configuration, and life cycle. Applications are loaded, started, and stopped. Each application also has its own environment, which provides a unified API for configuring each application.

In a nutshell, an application consists of all of the modules defined in the .app file, including the .app file itself. An application has generally only two directories: ebin, for Elixir artefacts, such as .beam and .app files, and priv, with any other artefact or asset you may need in your application.

На уровне синтаксиса языка код структурируется в функции и модули.
На уровне потоков код структурируется в дерево супервизоров.
Эти две структуры существуют независимо друг от друга.
Но есть **Application**, которое связывает их вместе.

Во многих языка мы привыкли, что после функций и модулей, следующим
уровнем идут пакеты.  В эрланг нет пакетов, но приложение
(Application) отчасти выполняет эту роль -- группирует несколько
модулей в одну сущность.

С другой стороны, приложение контролирует часть дерева супервизоров и
группирует потоки подобно тому, как пакет группирует модули. Эта
группа (поддерево) может быть запущена и остановлена как единое целое.

Ещё конфигурация.

Приложение следует рассматривать как некий компонент,
предназначенный для повторного использования в разных проектах.
Причем, для повторного использования предназначены обе структуры: и
структура кода (функции-модули), и структура потоков (поддерево
супервизоров).

Проект на эрланг обычно состоит из нескольких приложений:

Во-первых, это приложения, которые пишут разработчики --
непосредственно код проекта.

Во-вторых, это используемые библиотеки. Обычно каждая библиотека
оформляется как Application. Например, библиотека для логирования
[lager](https://github.com/basho/lager), библиотека для сериализации
JSON [jiffy](https://github.com/davisp/jiffy), драйвер для работы с
PostgreSQL [epgsql](https://github.com/epgsql/epgsql) и другие.

В-третьих, это приложения, входящие в состав OTP. Например, приложение
для работы с сетью **inets**, приложения, отвечающие за шифрование
**crypto** и **ssl**, приложение для модульного тестирования **eunit**
и другие.

Приложение состоит, как минимум, из главного модуля, реализующего
**behaviour(application)**, нескольких других модулей и файла
ресурсов. (Полную структуру мы рассмотрим на следующем уроке).


## Lifecycle

Applications are loaded, which means that the runtime finds and processes their resource files.
When an application is loaded, the environment specified in its resource file is merged with any overrides from config files.
Loading an application does not load its modules.
In practice, you rarely load applications by hand because that is part of the start process

Once your application is compiled, running your system is a matter of starting your current application and its dependencies.
Differently from other languages, Elixir does not have a main procedure that is responsible for starting your system. Instead, you start one or more applications, each with their own initialization and termination logic.
start/2 callback is invoked. The PID of the top-level supervisor returned by this function is stored by the runtime for later use, and the returned application state is saved too

Stopping an application with a callback module has three steps:
- If present, invoke the optional callback prep_stop/1.
- Terminate the top-level supervisor.
- Invoke the required callback stop/1.
step 2 is a blocking one. 

При остановке приложения завершается его поддерево супервизоров в
очередности, противоположной запуску.  То есть, сперва завершаются
рабочие потоки, потом дочерние супервизоры, и последним завершается
корневой супервизор.

Shutting down a live system cleanly can be done by calling System.stop/1. It will shut down every application in the opposite order they had been started.

Generally, build tools like Mix take care of starting an application and all of its dependencies for you

В эрланговской ноде всегда стартуют минимум 2 приложения: kernel и stdlib.

```
$ erl
Erlang/OTP 17 [erts-6.3] [source] [64-bit] [smp:4:4] [async-threads:10] [hipe] [kernel-poll:false]
Eshell V6.3  (abort with ^G)
1> application:which_applications().
[{stdlib,"ERTS  CXC 138 10","2.3"},
 {kernel,"ERTS  CXC 138 10","3.1"}]
```


## Resource file

The first step is to tell our application definition (i.e. our .app file) which module is going to implement the application callback. Let’s do so by opening mix.exs and changing def application to the following:

The :mod option specifies the “application callback module”, followed by the arguments to be passed on application start. The application callback module can be any module that implements the Application behaviour.

Although Mix generates and maintains the .app file for us, we can customize its contents by adding new entries to the application/0 function inside the mix.exs project file. 

But in the OTP world an application is a bundle of code that comes with a descriptor. 
That descriptor tells the runtime:
- what dependencies the code has, 
- what global names it registers, 
- and so on. 
In fact, an OTP application is more like a dynamic link library 
or a shared object than a conventional application.

application resource file
app_name.app file is used to define your application to the runtime environment.
- name, version, description
- list of modules
- list of dependencies
- application-callback module

Mix creates this file automatically from the information in mix.exs 
combined with information it gleans from compiling your application.

The mod: option tells OTP the module that is the main entry point for our app.
The second element of the tuple is the parameter to pass to start function.

The registered: option lists the names that our application will register. 
We can use this to ensure each name is unique across all loaded applications in a node or cluster.

Mix tells us it has created a sequence.app file, but where is it? 
You’ll find it tucked away in _build/dev/lib/sequence/ebin.

_build/dev/lib/sequence/ebin/sequence.app
```
{application,sequence,
  [
    {applications,[kernel,stdlib,elixir,logger]},
    {description,"sequence"},
    {modules,[
      'Elixir.Sequence','Elixir.Sequence.Application',
      'Elixir.Sequence.Server','Elixir.Sequence.Stash'
    ]},
    {vsn,"0.1.0"},
    {mod,{'Elixir.Sequence.Application',456}},
    {registered,['Elixir.Sequence.Server']},
    {extra_applications,[logger]}
  ]
}.
```
This file contains an Erlang tuple that defines the app. 

Some of the information comes from the project and application section of mix.exs. 
Mix also automatically added a list of the names of all the compiled modules in our app
and a list of the apps our app depends on ( kernel , stdlib , and elixir ).

Начнем с файла, описывающего метаинформацию о приложении.
Он должен называться по имени приложения и иметь расширение **app**.
Например, **my_cool_component.app**.

Внутри он содержит кортеж из трех элементов:

```
{application, ApplicationName, Properties}.
```

**ApplicationName** -- имя приложения в виде атома. Например, **my_cool_component**.

**Properties** -- свойства приложения в виде proplist, где, обычно, присутствуют такие элементы:
- **description** -- краткое описание приложения одной строкой;
- **vsn** -- версия приложения, обычно в формате "major.minor.patch";
- **modules** -- список всех модулей, входящих в состав приложения;
- **registered** -- список всех имен под которыми регистрируются потоки;
- **env** -- настройки приложения в виде вложенного proplist;
- **applications** -- список других приложений, от которых зависит данное приложение;
- **mod** -- основной модуль приложения, реализующий behaviour(application).

Все опции считаются необязательными, но лучше указывать их явно.
Большинство из них важны для сборки релиза.  Инструменты, собирающие
релиз, проверяют наличие указанных модулей, определяют очередность
загрузки приложений, выявляют конфликты имен потоков. _(Сборка релизов
не входит в данный курс.)_


## Callback Module

To implement the Application behaviour, we have to use Application and define a start/2 function. The goal of start/2 is to start a supervisor, which will then start any child services or execute any other code our application may need. Let’s use this opportunity to start the KV.Supervisor we have implemented earlier in this chapter.

Whenever we invoke iex -S mix, it automatically starts our application by calling Application.start(:kv), which then invokes the application callback. The application callback’s job is to start a supervision tree.

A general guideline is to use the supervisor without a callback module only at the top of your supervision tree, generally in the Application.start/2 callback. 

To implement the Application behaviour, we have to use Application and define a start/2 function. The goal of start/2 is to start a supervisor, which will then start any child services or execute any other code our application may need.



## Configuration

TODO большая тема, лучше сделать отдельный раздел 11_05_app_configuration.md

By default, the environment of an application is an empty list. In a Mix project's mix.exs file, you can set the :env key in application/0:
```
def application do
  [env: [db_host: "localhost"]]
end
```
Now, in your application, you can read this environment by using functions such as fetch_env!/2 and friends:
Application.fetch_env!(:my_app, :db_host)

In Mix projects, the environment of the application and its dependencies can be overridden via the config/config.exs file.
```
import Config
config :my_app, :db_host, "db.local"
```
if you change the value of the application environment after the code is compiled, the value used at runtime is not going to change!

You can provide values through config script files.
Config scripts are evaluated before project is compiled and started.
Generated sys.config are baked into OTP release.
So build machine makes the same configuration for all prod machines.
Config scripts can't provide parameters from external sources, such as OS environment, ini-files, etcd, or vault.


можно передавать аргументы из mix.ex в app:
```
# mix.ex
def application do
  [
    mod: {
      Sequence.Application, 456
    },
    extra_applications: [:logger],
  ]
end

defmodule Sequence.Application do
  use Application
  def start(_type, initial_number) do
    ..
  end
end
```

### Diff configuration for test env

config.exs
```
use Mix.Config
config :my_app, param1: "value_default"

import config "#{Mix.env()}.exs"
```

dev.exs
```
use Mix.Config
config :my_app, param1: "value_dev"
```

test.exs
```
use Mix.Config
config :my_app, param1: "value_test"
```

config.exs provides common settings for all env. Other configs may override it.


### Кастомизация конфигурации для разных машин

TODO 


