# Application

TODO
+ https://elixir-lang.org/getting-started/mix-otp/supervisor-and-application.html
- https://hexdocs.pm/elixir/1.12/Application.html

In a nutshell, an application consists of all of the modules defined in the .app file, including the .app file itself. An application has generally only two directories: ebin, for Elixir artefacts, such as .beam and .app files, and priv, with any other artefact or asset you may need in your application.

Each application in our system can be started and stopped. The rules for starting and stopping an application are also defined in the .app file.

The first step is to tell our application definition (i.e. our .app file) which module is going to implement the application callback. Let’s do so by opening mix.exs and changing def application to the following:

The :mod option specifies the “application callback module”, followed by the arguments to be passed on application start. The application callback module can be any module that implements the Application behaviour.

To implement the Application behaviour, we have to use Application and define a start/2 function. The goal of start/2 is to start a supervisor, which will then start any child services or execute any other code our application may need. Let’s use this opportunity to start the KV.Supervisor we have implemented earlier in this chapter.

Whenever we invoke iex -S mix, it automatically starts our application by calling Application.start(:kv), which then invokes the application callback. The application callback’s job is to start a supervision tree.

Applications are the entities that are started and stopped as a whole by the runtime. 


But in the OTP world an application is a bundle of code that comes with a descriptor. 
That descriptor tells the runtime:
- what dependencies the code has, 
- what global names it registers, 
- and so on. 
In fact, an OTP application is more like a dynamic link library 
or a shared object than a conventional application.

An OTP application is a component that consists of multiple modules
and that can depend on other application.
This makes it possible to start the entire system and dependent components
with a single function call.

Application is an OTP behaviour powered by :application/Application module.

start/2
ensure_all_started/2
stop/1

Both Application.stop/1 and System.stop/0 work in polite way. 
Every process in the supervision tree can perform some final cleanup.

Library app doesn't have a sup tree.


## application specification

application resource file
app_name.app file is used to define your application to the runtime environment.
- name, version, description
- list of modules
- list of dependencies
- application-callback module

Mix creates this file automatically from the information in mix.exs 
combined with information it gleans from compiling your application.

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

The mod: option tells OTP the module that is the main entry point for our app.
The second element of the tuple is the parameter to pass to start function.

The registered: option lists the names that our application will register. 
We can use this to ensure each name is unique across all loaded applications in a node or cluster.
```
def application do
  [
    mod: {
      Sequence.Application, 456
    },
    registered: [
      Sequence.Server,
    ],
    extra_applications: [:logger],
  ]
end
```     

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


## Application environment

```
def application do
  [
    mod: { Sequence.Application, [] },
    registered: [ Sequence.Server, ],
    env: [ initial_number: 456 ],
    extra_applications: [:logger],
  ]
end

def start(_type, _args) do
  Application.get_env(:sequence, :initial_number)
  |> Sequence.Supervisor.start_link()
end
``` 

You can provide values through config script files.
Config scripts are evaluated before project is compiled and started.
Generated sys.config are baked into OTP release.
    
So build machine makes the same configuration for all proc machines.

Config scripts can't provide parameters from external sources, such as OS environment, ini-files, etcd, or vault.

Library app shouldn't get configuration from app env, but accept them as function arguments.

Способы конфигурирования системы могут быть разными. Бывает так, что в компании есть некий способ доставки, разворачивания и конфигурирования системы на прод серверах, и этот способ универсальный для разных систем, написанных на разных языках программирования. И занимаются этим делом отдельные люди, вовсе не знакомые с эликсиром, его синтаксисом, и синтаксисом его конфигов. Тогда стандартные эликсировские конфиги вряд ли подойдут. 

Эликсировские конфиги могут подойти, если в соответсвии с духом настоящего DevOps, разработчики сами поставляют, разворачивают и конфигурируют свои системы на прод серверах. Однако это мало реально, если число инсталяций системы идет на сотни. Если разработчики будут разворачивать сотни систем, то они перестанут быть разработчиками, так как у них просто не останется времени на программирование.


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


# Application

На уровне синтаксиса языка код структурируется в функции и модули.
На уровне потоков код структурируется в дерево супервизоров.
Эти две структуры существуют независимо друг от друга.
Но есть **Application**, которое связывает их вместе.

Во многих языка мы привыкли, что после функций и модулей, следующим
уровнем идут пакеты.  В эрланг нет пакетов, но приложение
(Application) отчасти выполняет эту роль -- группирует несколько
модулей в одну сущность.  _(К сожалению, Application не создает
пространства имен.  Имена всех модулей находятся в одной области
видимости, и конфликты имен иногда случаются.)_

С другой стороны, приложение контролирует часть дерева супервизоров и
группирует потоки подобно тому, как пакет группирует модули. Эта
группа (поддерево) может быть запущена и остановлена как единое целое.

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


## Файл ресурсов (Application Resource File)

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

Пример ресурс файла, взят из cowboy 1.0.1:

```
{application, cowboy, [
	{description, "Small, fast, modular HTTP server."},
	{vsn, "1.0.1"},
	{id, "git"},
	{modules, []},
	{registered, [cowboy_clock, cowboy_sup]},
	{applications, [
		kernel,
		stdlib,
		ranch,
		cowlib,
		crypto
	]},
	{mod, {cowboy_app, []}},
	{env, []}
]}.
```

Из этого файла видно следующее:

Ключ **id** не документирован, это авторы cowboy сами что-то
придумали :)

Список модулей оставлен пустым. Его трудно поддерживать вручную,
обычно он генерируется автоматически при сборке проекта.

Cowboy регистритует 2 потока с именами **cowboy_clock** и
**cowboy_sup**.

Cowboy зависит от пяти других приложений. kernel, stdlib и crypto --
это часть OTP, ranch и cowlib -- это две библиотеки от тех же авторов.

Главный модуль -- **cowboy_app**.

Настроек тут нет, cowboy конфигурируется другим способом.


## Запуск и остановка приложения

В эрланговской ноде всегда стартуют минимум 2 приложения: kernel и stdlib.

```
$ erl
Erlang/OTP 17 [erts-6.3] [source] [64-bit] [smp:4:4] [async-threads:10] [hipe] [kernel-poll:false]
Eshell V6.3  (abort with ^G)
1> application:which_applications().
[{stdlib,"ERTS  CXC 138 10","2.3"},
 {kernel,"ERTS  CXC 138 10","3.1"}]
```

Один из потоков, которые запускает kernel, называется **application_controller**.
Он отвечает за загрузку и запуск других приложений.

Чтобы запустить приложение, нужно вызвать
**application:start(my_app_name)**.  При этом application_controller
загружает метаданные приложения, проверяет, что все зависимые
приложения уже запущены, и вызывает обработчик (callback) **my_app_name:start/2**.

Обработчик **start/2** получает аргументы StartType и StartArgs. Они
важны в распределенных приложениях, которые в данном курсе не
описываются.  _(Но вам никто не запретит посмотреть
[документацию](http://www.erlang.org/doc/apps/kernel/application.html#Module:start-2)
:)_ Здесь нужно запустить корневой супервизор приложения, и вернуть его Pid.

Чтобы остановить приложение, нужно вызывать **application:stop(my_app_name)**.
При этом будут вызваны обработчики **my_app_name:pre_stop/1** до остановки приложения,
и **my_app_name:stop/1** после его остановки.

**pre_stop/1** необязательный обработчик, так что его не нужно
определять, если в нем не планируете ничего делать. А вот **stop/1** --
обязательный обработчик, так что его всегда определяют, хотя чаще
всего оставляют пустым.

При остановке приложения завершается его поддерево супервизоров в
очередности, противоположной запуску.  То есть, сперва завершаются
рабочие потоки, потом дочерние супервизоры, и последним завершается
корневой супервизор.

В процессе разработки на локальной машине приложения не редко запускают вручную,
вызовом application:start/1. При этом нужно заботиться о том, чтобы
запускать их в правильном порядке, иначе start вернет:

```
{error, {not_started, SomeOtherApp}}.
```

Запуск упрощается вызовом:

```
application:ensure_all_started(my_cool_app).
```

Этот вызов сперва проверяет, что все зависимые приложения запущены.
Если не запущены, запускает их, и затем запускает my\_cool\_app.

```
1> application:start(ssl).
{error,{not_started,crypto}}
2> application:ensure_all_started(ssl).
{ok,[crypto,asn1,public_key,ssl]}
```

Здесь мы попытались запустить приложение ssl, но не получилось, потому
что оно зависит от crypto и public\_key.  А public\_key еще зависит от
asn1.  Вызов ensure\_all\_started запустил ssl и все эти зависимые
приложения.

При использовании релизов запуск отличается. Здесь от разработчика
требуется правильно указать зависимости приложений друг от друга в
файле ресурсов. Затем автоматически генерируется скрипт запуска ноды,
и там предусмотрен запуск всех приложений в правильном порядке.


## Настройки

Приложение можно конфигурировать внешними настройками. Один источник
таких настроек мы уже знаем -- это файл ресурсов.

Узел **env** в таком файле хранит настройки в виде proplist.

```
{application, my_cool_app,
 [
  {description, "The best app ever"},
  {vsn, "1.0.0"},
  {registered, []},
  {applications, [kernel, stdlib]},
  {mod, {my_cool_app, []}},
  {env, [{key1, "value 1"},
         {key2, 42},
         {key3, [1,2,3,4]},
         {key4, <<"value 4">>}
        ]}
 ]}.
```

Ключи должны быть атомами, а значения могут быть любого типа.

Другой источник, который используется чаще, это внешний
конфигурационный файл.  Он может иметь любое имя, но расширение должно
быть **.config**.

В файле должен быть список кортежей вида {AppName, AppSettings}, где
AppName -- атом, имя приложения, а AppSettings -- proplist, такой же,
как в файле ресурсов.

```
%% file my_project.config
[
 %% some app settings
  {my_cool_app, [
         {key1, "value 1"},
         {key2, 42},
         {key3, [1,2,3,4]},
         {key4, <<"value 4">>}
        ]}
 %% sasl app settings
 {sasl, [
         {errlog_type, error}
        ]},
 %% lager app settings
 {lager, [...]}
]
```


При запуске ноды нужно указать опцию **-config my_project**.

```
erl -config my_project ... other options
```

Для чтения настроек используются функции **applications:get_env**:

```
3> application:get_env(param1).
undefined
4> application:get_env(my_cool_app, param1).
{ok,"val1"}
5> application:get_env(my_cool_app, param2).
{ok,"val2"}
6> application:get_env(my_cool_app, param3).
undefined
7> application:get_env(my_cool_app, param3, "default value").
"default value"
```

**get_env/1** работает внутри модуля, принадлежащего конкретному приложению,
и возвращает настройку для этого приложения.

**get_env/2** требует указать приложение и ключ, и возвращает {ok, Value} или undefined.

**get_env/3** позволяет указать дефолтное значение на случай, если настройки нет в конфиге.

Есть нюанс, что в отличие от get\_env/2, которая возвращает {ok, Value},
get\_env/3 возвращает просто Value. Об этом нюансе нужно
помнить, если у вас в коде изначально был вызов get\_env/2, и вы
дописали к нему 3-й аргумент. Тут, скорее всего, нужно будет
поправить и код, принимающий значение из функции.

С настройками на рабочих серверах чаще имеют дело администраторы, чем
разработчики.  И для них такой синтаксис файла настроек неудобен.
Честно говоря, этот синтаксис неудобен и для самих разработчиков --
легко можно ошибиться в запятых и скобках. Компилятор этот файл не
проверяет, так что ошибка в синтаксисе проявится только при старте ноды.

Поэтому некоторые (и я в том числе), предпочитают использовать более
привычные **ini**-файлы, или что-то подобное. Хотя тут придется
приложить дополнительные усилия, чтобы загрузить и распарсить настройки.

Ну каждая команда в своем проекте делает выбор сама,
так что я воздержусь от рекомендаций.
