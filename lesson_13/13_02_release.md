# Релиз

Релиз -- это проект, собранный в пакет и готовый для доставки. И это также процесс сборки проекта в пакет.

TODO проект из одного приложения или из нескольких (umbrella)

В состав релиза входит:
- скомпилированный код (байткод) наших приложений;
- байткод всех необходимых зависимостей;
- конфигурация;
- скрипты для управления системой (для запуска, остановки и тд).

В состав релиза не входит:
- mix;
- исходный код;
- код тестов;
- зависимости, нужные только для тестов;
- любые файлы проекта, для которых явно не указано, что они должны быть в релизе.

Опционально в состав релиза можно включить виртуальную машину. Если мы не включаем виртуальную машину, то она должна быть установлена на тех хостах, куда мы будем доставлять проект. Причем, что важно, в нужной версии.

Бывает проще включить виртуальную машину в состав релиза, и тогда не требуется её наличия на хостах.

В обоих случаях доставленая и развёрнутая на хосте система выглядит так:

![Release](./img/release.png)

Это минимальная система, состоящая из одного узла.

Не редко система представляет собой кластер из нескольких узлов. Причём этот кластер не самодостаточный, а ему необходимо наличие базы данных, очереди сообщений и взаимодействие с другими системами по сети:

![Host](./img/host.png)

В этом варианте все компоненты развернуты на одном хосте. Такое бывает на стейджинг окружениях и на машине разработчика. Но реально использующаяся система (production) разворачивается на нескольких хостах:

![Cluster](./img/cluster.png)

Если пойти еще дальше, то можно построить федерацию -- систему из нескольких кластеров, находящихся в разных датацентрах:

![Federation](./img/federation.png)


## Инструменты сборки

В составе OTP есть приложения **systool** и **reltool**. Они низкоуровневые, и пользоваться ими трудно. Зато они позволяют собрать кастомизированые релизы под узкие задачи.

TODO
- сборка на машине разработчика и сборка в CI
- разница между mix run и bin/proj start

Distillery — The Elixir Release Manager

Поэтично:
Every alchemist requires good tools, and one of the greatest tools in the alchemist's disposal is the distillery. The purpose of the distillery is to take something and break it down to its component parts, reassembling it into something better, more powerful. That is exactly what this project does - it takes your Mix project and produces an Erlang/OTP release, a distilled form of your raw application's components; a single package which can be deployed anywhere, independently of an Erlang/Elixir installation. 

Distillery produces an artifact, a tarball, which contains your application and everything needed to run it.

This artifact also contains scripts which allow you to run the application in three different modes (console, foreground, and daemonized), as well as a variety of utility commands, such as remote_console which provides an easy way to connect an IEx session to your running application.

Distillery is a layer of abstraction on top of this complexity.
Normally it manages to hide it, but sometimes the lower levels leak out
and you get to see how the sausage is made.

```
defp deps do
  [
    {:distillery, "~> 1.5", runtime: false},
  ]
end

$ mix release.init # generates rel/config.exs
$ mix release --env=prod
...
==> Release successfully built!
You can run it in one of the following ways:
Interactive: _build/dev/rel/sequence/bin/sequence console
Foreground: _build/dev/rel/sequence/bin/sequence foreground
Daemon: _build/dev/rel/sequence/bin/sequence start
```

rel/sequence/releases/0.0.1/sequence.tar.gz
This is the file we deploy to our servers.

Release doesn't contain source code, documentation files, test etc.

You can build the system on your development machine or the build server and ship only binary artifacts.
The host machine doesn't need to have any tools installed.

You can embed the minimum erlang runtime into release. Then you don't need Elixir and Erlang installed on host machine. Whatever is required to run the system will be part of your release package.

**Release handling** is the way of systematic online system upgrades (and downgrades).

- compiled OTP applications;
- erlang runtime binaries;
- boot script describing which OTP applications need to be started;
- vm.args -- arguments that will be passed to the virtual machine;
- sys.config -- configuration file;
- helper script to start, stop, and interact with the system.

_build/prod/rel/proj_name/releases/0.1.0/proj_name.tar.gz
compressed version of the entire release
copy this file to host machine, unpack, and use bin/proj_name start

This file plays important role in live upgrade.
bin/proj_name upgrade "0.2.0"

Boot file описывает, какие приложения и модули загружать, и запускать, и в какой последовательности.
Создается текстовый, потом компилируется в бинарник.

Sample:
releases/18/start_sasl.script
releases/18/start_sasl.boot

Вообще тут 4 boot файла:
- start_clean.boot -- запустить только kernel и stdlib
- start_sasl.boot -- запустить kernel, stdlib и sasl
- no_dot_erlang.boot -- не выполнять инструкции в файле .erlang при запуске ноды
- start.boot -- копия одного из 3х выше, выполняется по умолчанию.

As you can see this script operates at a very low level, instructing the runtime what modules to load, checkpointing major events, using apply instructions to load and start applications, and more 

In an OTP release, this script is actually converted to a binary form, which is stored with a .boot extension, but all this file is, is the result of calling :erlang.term_to_binary/1 on the data structure in the .script file. If you ever need to see exactly what is in the .boot, just run :erlang.binary_to_term/1 on the contents to see the data structure itself.

The main tool to interact with release is the shell script
_build/prod/rel/proj_name/bin/proj_name

- start the system with iex shell in foreground
- start the system as a background process
- attach a remote shell to running system
- stop the system


## Configuration

_build/prod/rel/proj_name/releases/0.1.0/
where 0.1.0 is a version of your application as provided in mix.exs

vm.args
can be used to provide flags to the Erlang runtime
+P sets the maximum number of running processes
Some basic defaults are generated by distillery (node name, cookie)
hexdocs.pm/distillery/configuration.html

sys.config
contains OTP environment variables as specified in mix.exs and config.exs

окружения dev, test, prod, настройки для них.

если мы доставим релиз на несколько хостов, то везде будет идентичная конфигурация
обычно это не то, чего мы хотим
настройки для конкретной машины -- через переменные окружения.

sys.config -- настройки на уровне релиза, одинаковые для всех машин
двойной запуск для генерации sys.config



*** Emulator flags
vm.args

Deal with memory management, multicore architectures, ports and sockets, low-level tracing, or other internal optimizations.

also:

+Bc -- disables the shell break handler, so when you press the sequence Ctrl-c a,
instead of terminating the virtual machine you terminate just the shell process and restart it

+e Num -- sets the maximum number of ETS tables, which defaults to 2,053

+P Num -- system limit on the maximum number of processes allowed to exist simultaneously.
default is 262,144, range from 1,024 to 134,217,727.

+Q Num -- maximum number of ports allowed in the system,
default to 65,536. range is 1,024 to 134,217,727.

+t Num -- maximum number of allowed atoms, set by default to 1,048,576.


## Доставка (Deployment)

A **deployment** is a way of getting a release into an environment where it can be used.


## Live upgrade

как это работает

почему оно было нужно в телекоме, и почему оно не нужно сейчас

_build/prod/rel/proj_name/releases/0.1.0/proj_name.tar.gz
This file plays important role in live upgrade.
bin/proj_name upgrade "0.2.0"

- migrate state of stateful processes (gen_server.code_change);
- migrate state of ETS tables;
- reconfigure supervision tree;
- restart OTP apps.

Alternative approach: restart nodes in cluster one by one.


A **hot upgrade** is a kind of deployment that allows the release of a currently running application
to be changed while that application continues to run —
the upgrade happens in place with no user-detectable disruption.
