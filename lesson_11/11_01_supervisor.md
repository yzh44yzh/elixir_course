# Supervisor

как запустить под супервизором: task, agent, genserver

Think in terms of Lifecycles, not in terms of Supervision.
start, stop, restart -- it is a lifecycle.

TODO
https://elixir-lang.org/getting-started/mix-otp/supervisor-and-application.html
https://elixir-lang.org/getting-started/mix-otp/dynamic-supervisor.html


# Supervisor

# Sasa Juric

Links, trap exits and monitors make it possible to detect errors in concurrent system.
You can introduce a process whose only responsibility is to receive links and monitor notifications,
and do something when a process crashed.
Such processes, called Supervisors, are the primary tools of error recovery in concurrent systems.

A supervisor is a generic process that manages the lifecycle of other processes.
- starts other processes (which are then considered to be its children);
- detects termination of any child;
- restart it if needed.

If supervisor terminates, its children are also taken down. 
Поэтому link, а не monitor.

Restarting boils down to starting another process in place of the old one. 
The new process has a different pid and doesn't share any state with the old one.
Any reference to the pid of the crashed process becomes invalid.

That's why registered names are important. They provide a reliable way of finding a process and talking to it, regardless of possible restarts.  

Isolating the effect off errors allows other parts of system to run and provide service while you're recovering from error.

Children are started synchronously, in the order specified. The next child is started only after the init/1 for the current child is finished. That's why init/1 shouldn't run for a long time.


## Supervision tree 
 
a nested structure of supervisors and workers.
The tree describes how the system is organized into a hierarchy of services.
The tree describes how the system is started and how it's taken down.
A more granular tree allows you to take down part of the system.

You try to recover from an error locally. If that doesn't work, you move up and try to restart the wider part of the system.

Graceful termination of a GenServer worker involves invoking the terminate/2 callback, but only if the worker process is trapping exits. 
TODO Вот этого я не понимаю. Почему так сделано? 


## Dynamic supervisor
```
DynamicSupervisor.start_link(name: __MODULE__, strategy: :one_for_one)
DynamicSupervisor.start_child(__MODULE__, {WorkerModule, worker_name})
WorkerModule.child_spec(_) do
  %{id: ..., start: {WorkerModule, :start_link, []}} 
end
```

start_child returns {:ok, pid} or {:error, {:already_started, pid}}
что является удобным способом получить pid существующего воркера или запустить нового, если нет существующего.
И это исключает race condition при попытке запустить воркера с одинаковым id из разных мест,
так как start_child сериализуется в одном процессе.
С другой стороны, это не очень эффективно, тк супервизор каждый раз делает попытку запуска нового процесса.


## Temporary

Есть смысл запускать воркера под супервизором даже если он :temporary, и его не нужно рестартовать.
В этом случае воркер не зависнет в системе при крашах и рестартах, а гарантировано завершится.
И краш в этом воркере будет правильно логирован.

Запуск процессов не под супервизором является антипаттерном.
Либо напрямую под супервизором, либо процесс должен быть слинкован с другим, находящимся под супервизором.


## Let it crash

described in Joe Armstrong PhD thesis
"Making reliable distributed systems in the presence of software errors"

happy path programming, liberated from paranoid, defensive try/catch.
error handling is separated

There are 2 important situations in which you should explicitly handle an error:
- critical processes that shouldn't crash
- when you expect an error and can deal with it in a meaningful way

Error Kernel -- part of the system, that shouldn't crash. Critical for entire system to work.
Keep it small and simple.
If the code in your error-kernel process is complex, consider splitting it into two processes:
one that holds state (simple), and another that does the actual work (not error-kernel).
You can use defensive programming in error-kernel code.

Anyway you should always have a recovery plan for the crash of a critical process.

The whole point of the let-it-crash approach is to leave recovery of unexpected errors to supervisors.
But if you can predict an error and you have a way to deal with it -- do it.

State isn't preserved when a process is crashed. It could be inconsistent. That why it is important to start from clean state.
You can implement persistent state if you need. This isn't provided out of the box.
The general approach is to save the state outside of the process (another process or database),
then restore it when successor process is started.
Make sure you store consistent state. If you store inconsistent state restarts won't help you.

# Dave Thomas

Think of a typical application. 
If an unhandled error causes an exception to be raised, the application stops.
The issue here is that one error takes the whole application down.

But imagine that instead your application consists of hundreds or thousands of processes, 
each handling just a small part of a request. 
If one of those crashes, everything else carries on. 
You might lose the work it’s doing, but you can design your applications to minimize even that risk.

At its simplest, a supervisor is a process that uses the OTP supervisor behavior.
It is given a list of processes to monitor and is told what to do if a process dies, 
and how to prevent restart loops.

You can write supervisors as separate modules, but the Elixir style is to include them inline.

```
defmodule Sequence.Application do
  @moduledoc false
  use Application
  
  def start(_type, _args) do
    children = [
      {Sequence.Server, 123},
    ]
    opts = [strategy: :one_for_one, name: Sequence.Supervisor]
    Supervisor.start_link(children, opts)
  end
end
```

We get a wonderful error report that shows us the exception, 
along with a stack trace from the process. 
We can also see the message we sent that triggered the problem.

But when we then ask our server for a number, 
it responds as if nothing had happened. 
The supervisor restarted our process for us. 

This is excellent, but there’s a problem. 
The supervisor restarted our sequence process with the initial parameters we passed in, 
and the numbers started again from 123. 
A reincarnated process has no memory of its past lives, and no state is retained across a crash.


## Child spec

Можно напрямую указать в Supervisor.start_link. 
```
Supervisor.start_link(
  [
    %{id: some_child_id, start: {module, fun, args}}
  ],
  strategy: :one_on_one
)
```

А можно определить callback в модуле, и указать только этот модуль.
```
Supervisor.start_link([MyModule], strategy: :one_on_one)
```

Вызов use GenServer генерирует callback по умолчанию.
```
> MyModule.child_spec(nil)
%{id: MyModule, start: {MyModule, :start_link, [nil]}}
``` 

Или можно определить в модуле свой callback.


## Managing Process State Across Restarts

All of the approaches to this involve storing the state outside of the process.

We’ll write a separate process that can store and retrieve a value. We’ll call it our stash (копилка, тайник). 
The sequence server can store its current number in the stash whenever it terminates, 
and then we can recover the number when we restart.

```
def start(_type, _args) do
  children = [
    {Sequence.Stash, 123},
    {Sequence.Server, nil},
  ]
  opts = [strategy: :rest_for_one, name: Sequence.Supervisor]
  Supervisor.start_link(children, opts)
end
```

Supervision strategy:
- :one_fo_one
- :one_for_all
- :rest_for_one

I vote for :rest_for_one because I feel it expresses my intent better. 
A :rest_for_one supervision strategy explicitly says, 
“this server depends on the health of previous servers in the list.”

```
def init(_) do
  {:ok, Sequence.Stash.get()}
end

def terminate(_reason, current_number) do
  Sequence.Stash.update(current_number)
end
```

```
$ iex -S mix
iex> Sequence.Server.next_number
123
iex> Sequence.Server.next_number
124
iex> Sequence.Server.next_number
125
iex> Sequence.Server.increment_number "cat"
:ok
iex>
12:15:48.424 [error] GenServer Sequence.Server terminating
** (ArithmeticError) bad argument in arithmetic expression
(sequence) lib/sequence/server.ex:39: Sequence.Server.handle_cast/2
Last message: {:"$gen_cast", {:increment_number, "cat"}}
State: 126
iex> Sequence.Server.next_number
126
iex> Sequence.Server.next_number
127
```

The server code crashed, but was then restarted automatically. 
And, in the process, the state was stored away in the stash 
and then recovered — the sequence continued uninterrupted.


## Worker Restart Options

The conditions when a worker should be restarted are dictated by its restart: option:
- :permanent - always restart
- :temporary - never restart
- :transient - restart if failed

The simplest way to specify the restart option for a worker is in the worker module. 
You add it to the use GenServer (or use Supervisor ) line:
```
defmodule Convolver do
  use GenServer, restart: :transient
```


## Child specification

A child spec is an Elixir map. It describes 
- which function to call to start the worker, 
- how to shut the worker down, 
- the restart strategy, 
- the worker type, 
- and any modules apart from the main module that form part of the worker.

You can create a child spec map using the Supervisor.child_spec/2 function.

You can specify a worker by giving its module name (or a tuple containing the module and the initial arguments). 
In this case, the supervisor assumes you’ve implemented a child_spec function in that module,
and calls that function to get the specification.

When you add the line ```use GenServer``` to a server module,
Elixir will define a default child_spec function in that module.

This function by default returns a map that tells the supervisor that 
the start function will be start_link 
and that the restart strategy will be :permanent. 
You can override these defaults with the options you give use GenServer.

In practice, the option you’ll change the most will be :restart. 
Although :permanent is a good default for long-running servers, 
it won’t work for servers that do a job and then exit. 
These types of servers should have a restart value of :transient.


## Supervisors Are the Heart of Reliability

It is a concrete representation of the idea of building rings of confidence in our code. 
The outer ring, where our code interacts with the world, should be as reliable as we can make it. 
But within that ring there are other, nested rings. 
And in those rings, things can be less than perfect. 
The trick is to ensure that the code in each ring knows how to deal with failures of the code in the next ring down.

The fact that you use them to manage your workers 
means you are forced to think about reliability and state 
as you design your application.


## DynamicSupervisor

This type of supervisor allows you to create an arbitrary number of workers at runtime.

A DynamicSupervisor encapsulates what used to be the :simple_one_for_one strategy in regular supervisors.

```
defmodule Duper.WorkerSupervisor do
  use DynamicSupervisor

  @me WorkerSupervisor

  def start_link(_) do
    DynamicSupervisor.start_link(__MODULE__, :no_args, name: @me)
  end

  def init(:no_args) do
    DynamicSupervisor.init(strategy: :one_for_one)
  end

  def add_worker() do
    {:ok, _pid} = DynamicSupervisor.start_child(@me, Duper.Worker)
  end
end
```

## Lance Halvorsen

The supervisor Behaviour is based on ideas:
- most runtime errors are transient and happen because of bad state
  (но не все, то есть, одно только супервизора недостаточно на все случаи)
- the best way to fix bad state is to let the process crash and restart it with a good state
  (При этом мы теряем состояние процесса. И тут важно подумать, что мы можем позволить себе потерять, а что важно сохранить. Очень важное нужно хранить персистентно.)
- restarts work best on small independent processes.
  (и это основа для Error Kernel)

Defensive programming -- предусмотреть все возможные ошибки заранее, и явно написать обработку для них. Если чего-то не предусмотрел, то не повезло.
Let it crash -- generic способ восстановления после любой ошибки.


## Error Kernel pattern

Упоминается смутно, нужно искать инфу.

## Немного теории

На прошлом уроке мы выяснили, что стратегия эрланг -- разделить потоки
на рабочие (worker) и системные (supervisor), и поручить системным
потокам обрабатывать падения рабочих потоков.

Существуют научные работы, которые доказывают, что значительная часть
ошибок в серверных системах вызваны временными условиями, и перегрузка
части системы в известное стабильное состояние позволяет с ними
справиться. Среди таких работ [докторская диссертация Джо Армстронга](http://www.sics.se/~joe/thesis/armstrong_thesis_2003.pdf),
одного из создателей эрланг.

Систему на эрланг рекомендуется строить так, чтобы любой поток был под
наблюдением супервизора, а сами супервизоры были организованы в
дерево.

![supervision_tree](http://yzh44yzh.github.io/img/practical_erlang/supervision_tree.png)

На картинке нарисовано такое дерево. Узлы в нем -- супервизоры, а
листья -- рабочие процессы.  Падение любого потока и любой части
системы не останется незамеченным.

Дерево супервизоров разворачивается на старте системы. Каждый
супервизор отвечает за то, чтобы запустить своих потомков, наблюдать
за их состоянием, рестартовать и корректно завершать, если надо.

В эрланг есть [стандартная реализация супервизора](http://www.erlang.org/doc/man/supervisor.html).
Он работает аналогично gen_server. Вы должны написать кастомный
модуль, реализующий поведение supervisor, куда входит одна функция
обратного вызова **init/1**.  С одной стороны это просто -- всего один
callback. С другой стороны **init** должен вернуть довольно сложную
структуру данных, с которой нужно как следует разобраться.


## Запуск супервизора

Запуск supervisor похож на запуск gen_server.
Вот картинка, аналогичная той, что мы видели в 10-м уроке:

![supervision_tree](http://yzh44yzh.github.io/img/practical_erlang/supervisor_init.png)

Напомню, что два левых квадрата (верхний и нижний), соответствуют
нашему модулю.  Два правых квадрата соответствуют коду OTP. Два
верхних квадрата выполняются в потоке родителя, два нижних квадрата
выполняются в потоке потомка.

Начинаем с функции **start\_link/0**:

```
start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).
```

Здесь мы просим supervisor запустить новый поток.

Первый аргумент, **{local, ?MODULE}** -- это имя, под которым нужно
зарегистрировать поток. Есть вариант supervisor:start\_link/2 на случай,
если мы не хотим регистрировать поток.

Второй аргумент, **?MODULE** -- это имя модуля, callback-функции
которого будет вызывать supervisor.

Третий аргумент -- это набор параметров, которые нужны при
инициализации.

Дальше происходит некая магия в недрах OTP, в результате
которой создается дочерний поток, и вызывается callback **init/1**.

Из **init/1** нужно вернуть структуру данных, содержащую всю
необходимую информацию для работы супервизора.


## Настройка супервизора

Разберем подробнее:
```
{ok, {SupervisorSpecification, ChildSpecifications}}
```

Нам нужно описать спецификацию самого супервизора, и дочерних
процессов, за которыми он будет наблюдать.

Спецификация супервизора -- это кортеж из трех значений:
```
{RestartStrategy, Intensity, Period}
```

RestartStrategy описывает политику перезапуска дочерних потоков.
Есть 4 варианта стратегии:

**one_for_one** -- при падении одного потока перезапускается только
этот поток, остальные продолжают работать.

**one_for_all** -- при падении одного потока перезапускаются все
дочерние потоки.

**rest_for_one** -- промежуточный вариант между двумя первыми
стратегиями. Суть в том, что изначально потоки запущены один за одним,
в определенной последовательности. И при падении одного потока,
перезапускается он, и те потоки, которые были запущены позже него. Те,
которые были запущены раньше, продолжают работать.

**simple_one_for_one** -- это особый вариант, будет рассмотрен ниже.

Многие проблемы можно решить рестартом, но не все. Супервизор должен
как-то справляться с ситуацией, когда рестарт не помогает.  Для этого
есть еще две настройки: **Intensity** -- максимальное количество
рестартов, и **Period** -- за промежуток времени.

Например, если Intensity = 10, а Period = 1000, это значит, что
разрешено не более 10 рестартов за 1000 миллисекунд. Если поток падает
11-й раз, то супервизор понимает, что он не может исправить проблему.
Тогда супервизор завершается сам, а проблему пытается решить его
родитель -- супервизор уровнем выше.

В 18-й версии эрланг вместо кортежа:
```
{RestartStrategy, Intensity, Period}
```
используется map:

```
#{
    strategy => one_for_one,
    intensity => 10,
    period => 1000
}
```

Но и кортеж поддерживается для обратной совместимости.


### child specifications

Теперь разберем, как описываются дочерние потоки.
Каждый из них описывается кортежем из 6-ти элементов:

```
{ChildID, Start, Restart, Shutdown, Type, Modules}.
```

**ChildID** -- идентификатор потока. Тут может быть любое значение.
Супервизор не использует Pid дочернего потока, потому что Pid будет
меняться при рестарте.


**Start** -- кортеж {Module, Function, Args} описывающий, с какой
функции стартует новый поток.


**Restart** -- атом, указывающий необходимость рестарта дочернего потока.
Возможны 3 варианта:
- permanent -- поток нужно рестартовать всегда.
- transient -- поток нужно рестартовать, если он завершился аварийно. При нормальном завершении рестартовать не нужно.
- temporary -- поток не нужно рестартовать.


**Shutdown** -- определяет, сколько времени супервизор дает дочернему
потоку на нормальное завершение работы.

Когда супервизор хочет остановить дочерний поток, он шлет сигнал
shutdown, и ждет заданное время.  Если за это время дочерний поток не
завершился, супервизор останавливает его сигналом kill.

Shutdown может быть указан как время в миллисекунах, либо атомами:
- brutal_kill -- не давать время, завершать принудительно сразу же.
- infinity -- не ограничивать время, пусть дочерний поток завершается сколько, сколько ему нужно.

Обычно для worker-потоков указывают время в миллисекундах, а для supervisor-потоков указывают infinity.


**Type** -- тип дочернего потока. Может быть либо worker, либо supervisor.


**Modules** -- модули, в которых выполняется дочерний поток. Обычно это один модуль,
и он совпадает с указанным в кортеже Start.

Пример child specitication:
```
{some_worker,
 {some_worker, start_link, []},
 permanent,
 2000,
 worker,
 [some_worker]},
```

В 18-й версии эрланг используется map:
```
#{
    id => some_worker,
    start => {some_worker, start_link, []},
    restart => permanent,
    shutdown => 2000,
    type => worker,
    modules => [some_worker]
}
```

Пример функции init:

```
init(_Args) ->
    RestartStrategy = one_for_one, % one_for_one | one_for_all | rest_for_one
    Intensity = 10, %% max restarts
    Period = 60, %% in period of time
    SupervisorSpecification = {RestartStrategy, Intensity, Period},

    Restart = permanent, % permanent | transient | temporary
    Shutdown = 2000, % milliseconds | brutal_kill | infinity

    ChildSpecifications =
        [
         {some_worker,
          {some_worker, start_link, []},
          Restart,
          Shutdown,
          worker,
          [some_worker]},
         {other_worker,
          {other_worker, start_link, []},
          Restart,
          Shutdown,
          worker,
          [other_worker]}
        ],
    {ok, {SupervisorSpecification, ChildSpecifications}}.
```

То же самое для 18-й версии эрланг:

```
init(_Args) ->
    SupervisorSpecification = #{
        strategy => one_for_one,
        intensity => 10,
        period => 60},

    ChildSpecifications =
        [#{id => some_worker,
           start => {some_worker, start_link, []},
           restart => permanent,
           shutdown => 2000,
           type => worker,
           modules => [some_worker]},
         #{id => other_worker,
           start => {other_worker, start_link, []},
           restart => permanent,
           shutdown => 2000,
           type => worker,
           modules => [other_worker]}
        ],
    {ok, {SupervisorSpecification, ChildSpecifications}}.
```

С map это все выглядит понятнее и лаконичнее.


## Динамическое создание воркеров

Дерево супервизоров не обязательно должно быть статичным. При
необходимости его можно менять: добавлять/удалять новые рабочие
потоки, и даже новые ветки супервизоров.  Есть два способа это
сделать: либо вызовами **start_child** либо использованием
**simple_one_for_one** стратегии.


### start_child

4 функции супервизора позволяют добавлять и убирать дочерние потоки.


**start_child/2**

Функция позволяет добавить новый дочерний поток, не описанный в **init**.
Она принимает 2 аргумента: имя/pid супервизора, и спецификацию дочернего потока.

```
supervisor:start_child(
    MySupervisor,
    {some_worker,
     {some_worker, start_link, []},
      Restart,
      Shutdown,
      worker,
      [some_worker]})
```

**terminate_child/2**

Функция позволяет остановить работающий дочерний поток.
Она принимает 2 аргумента: имя/pid супервизора, и Id дочернего потока.

```
supervisor:terminate_child(MySupervisor, some_worker)
```

После того, как поток остановлен, его можно либо рестартовать вызовом **restart_child/2**,
либо вообще убрать его спецификацию из списка дочерних потоков вызовом **delete_child/2**.


### simple_one_for_one стратегия

Использование **simple_one_for_one** стратегии -- это особый случай,
когда нам нужно иметь большое количество потоков: десятки и сотни.

При использовании этой стратегии супервизор может иметь потомков
только одного типа. И, соответственно, должен указать только одну
child specitication.

```
init(_Args) ->
    SupervisorSpecification = {simple_one_for_one, 10, 60},
    ChildSpecifications =
        [
         {some_worker,
          {some_worker, start_link, [A, B, C]},
          transient,
          2000,
          worker,
          [some_worker]}
        ],
    {ok, {SupervisorSpecification, ChildSpecifications}}.
```

Дочерние потоки нужно запускать явно, вызовом **start_child/2**.
Причем, тут меняется роль второго аргумента. Это теперь не child
specification, а дополнительные аргументы дочернему потоку.

```
supervisor:start_child(MySupervisor, [D, E, F]).
```

И дочерний поток в своей функции start\_link получит аргументы и из
child specification, и из start\_child.

```
-module(some_worker).

start_link(A, B, C, D, E, F) ->
    ...
```


## Остановка супервизора

В АПИ супервизора не предусмотрено функции для его остановки. Он
останавливается либо по своей стратегии, либо по сигналу родителя.

При этом он завершает все свои дочерние потоки в очередности, обратной
их запуску, затем останавливается сам.
