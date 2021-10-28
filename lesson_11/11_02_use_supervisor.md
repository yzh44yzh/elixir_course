# Использование супервизора

## Запускаем Agent под супервизором

Возьмем ShardingAgent из прошлого урока. Его нужно будет немного доработать. 

Во-первых, функцию для запуска процесса общепринято называть `start_link`, и она должна принимать один аргумент. Можно назвать функцию иначе, и аргументов сделать больше, но тогда в супервизоре придется переопределять настройки по-умолчанию, что не всегда удобно.

Во-вторых, мы передадим в эту функцию имя агента и его начальное состояние. Так мы сможем запустить несколько агентов с разными именами и состояниями. А поскольку наша функция принимает один аргумент, то придется передать кортеж:
```
def start_link({agent_name, state}) do
  Agent.start(fn () -> state end, [name: agent_name])
end
```

Функцию `find_node` тоже добработам, чтобы можно было указать имя агента:
```
def find_node(agent_name, shard_num) do
  Agent.get(agent_name, fn(state) -> do_find_node(state, shard_num) end)
end
```

Запустим одного агента, и будем использовать child specification по-умолчанию:
```
def start() do
  state = [
    { 0, 11, "Node-1"},
    {12, 23, "Node-2"},
    {24, 35, "Node-3"},
    {36, 47, "Node-4"}
  ]
  child_spec = [
    {ShardingAgent, {:agent_1, state}}
  ]
  Supervisor.start_link(child_spec, strategy: :one_for_all)
end
```

Здесь child specification выглядит предельно просто:
```
{ShardingAgent, {:agent_1, state}}
```
Это модуль агента, и аргументы для start_link. Этого достаточно, потому что в модуле агента мы применяем магию:
```
use Agent
```
Это специальный макрос, который неявно добавляет в модуль функцию `child_spec/1`. Супервизор вызывает эту функцию и получает child specification непосредственно от модуля, который он собирается запускать.
```
> c "lib/agent_with_sup.exs"

> Lesson_11.ShardingAgent.child_spec(:no_args)
%{
  id: Lesson_11.ShardingAgent,
  start: {Lesson_11.ShardingAgent, :start_link, [:no_args]}
}
```
В Эликсире (в отличие от Эрланга) принято соглашение, что каждый модуль сам определяет child specification, необходимый для его запуска. Для Task, Agent и GenServer это генерируется неявно со значениями по умолчанию.

Если мы захотим что-то переопределить, что достаточно передать нужные ключи в макрос:
```
use Agent, restart: :permanent
```
и макрос сгенерирует нужную реализацию:
```
> Lesson_11.ShardingAgent.child_spec(:no_args)
%{
  id: Lesson_11.ShardingAgent,
  restart: :permanent,
  start: {Lesson_11.ShardingAgent, :start_link, [:no_args]}
}
```

Запускаем и смотрим, как это работает:

```
iex(9)> Lesson_11.start()
{:ok, #PID<0.167.0>}

iex(10)> Lesson_11.ShardingAgent.find_node(:agent_1, 0)
{:ok, "Node-1"}
iex(12)> Lesson_11.ShardingAgent.find_node(:agent_1, 10)
{:ok, "Node-1"}
iex(13)> Lesson_11.ShardingAgent.find_node(:agent_1, 15) 
{:ok, "Node-2"}
iex(14)> Lesson_11.ShardingAgent.find_node(:agent_1, 40)
{:ok, "Node-4"}
iex(15)> Lesson_11.ShardingAgent.find_node(:agent_1, 60)
{:error, :not_found}
```

## Запускаем двух агентов

Если мы хотим запустить двух агентов, то понадобятся разные `id` в child specification. Реализация по-умолчанию подставляет в качестве id имя модуля (что является общепринятой практикой). 

Но мы не можем запустить двух агентов с одинаковым id, поэтому придется явно указать child specification:

```
def start_2_agents() do
  state_1 = [
      {0, 4, "Node-1"},
      {5, 9, "Node-2"}
  ]
  state_2 = [
      { 0,  9, "Node-1"},
      {10, 19, "Node-2"},
      {20, 29, "Node-3"}
  ]

  child_spec = [
    %{
      id: :agent_a,
      start: {ShardingAgent, :start_link, [{:agent_a, state_1}]}
    },
    %{
      id: :agent_b,
      start: {ShardingAgent, :start_link, [{:agent_b, state_2}]}
    }
  ]
  Supervisor.start_link(child_spec, strategy: :one_for_all)
end

```

Смотрим, как это работает:

```
iex(6)> Lesson_11.start_2_agents()
{:ok, #PID<0.125.0>}
iex(7)> Lesson_11.ShardingAgent.find_node(:agent_a, 5)
{:ok, "Node-2"}
iex(8)> Lesson_11.ShardingAgent.find_node(:agent_b, 5)
{:ok, "Node-1"}
iex(9)> Lesson_11.ShardingAgent.find_node(:agent_a, 10)
{:error, :not_found}
iex(10)> Lesson_11.ShardingAgent.find_node(:agent_b, 10)
{:ok, "Node-2"}
```

(В Эрланг такого рода макросов нет, и все child specification всегда нужно явно прописывать. Впрочем, многие считают это преимуществом исходя из принципа "явное лучше неявного").


## Запускаем Task под супервизором

Модуль
[Task.Supervisor](https://hexdocs.pm/elixir/1.12/Task.Supervisor.html)
предоставляет аналогичное АПИ как и модуль 
[Task](https://hexdocs.pm/elixir/1.12/Task.html)

Так что нам достаточно вместо:
```
Task.async(__MODULE__, :find_elixir_sources, [path])
```
сделать
```
{:ok, sup_pid} = Task.Supervisor.start_link()
Task.Supervisor.async(sup_pid, __MODULE__, :find_elixir_sources, [path])
```

и все работает:
```
iex(1)> c "lib/task_with_sup.exs"
[Lesson_11, Lesson_11.FindSourcesTask]
iex(2)> task = Lesson_11.FindSourcesTask.start("lib")
iex(3)> Lesson_11.FindSourcesTask.get_result(task)
["lib/task_with_sup.exs", "lib/agent_with_sup.exs"]
iex(4)> task = Lesson_11.FindSourcesTask.start("../lesson_10/lib")
iex(5)> Lesson_11.FindSourcesTask.get_result(task)
["../lesson_10/lib/path_finder2.exs", "../lesson_10/lib/path_finder.exs"]
```

Task можно запустить под обычным супервизором так же, как мы выше запускали Agent:
```
child_spec = [
  {MyTaskModule, args}
]
Supervisor.start_link(child_spec, strategy: :one_for_all)
```
Но в этом случае нет способа получить результат работы Task. Это подходит для каких нибудь фоновых задач, как, например, прогрев кэшей.


## Запускаем GenServer под супервизором

Запустим PathFinder из прошлого урока. Поскольку там есть `use GenServer`, то и функция `child_spec/1` тоже есть:

```
iex(1)> c "lib/gen_server_with_sup.exs"
[Lesson_11, Lesson_11.PathFinder]
iex(2)> Lesson_11.PathFinder.child_spec(:no_args)
%{
  id: Lesson_11.PathFinder,
  start: {Lesson_11.PathFinder, :start_link, [:no_args]}
}
```

Запуск сервера нужно немного поправить, вместо:
```
def start() do
  GenServer.start(__MODULE__, :no_args, [name: @server_name])
end
```
сделаем:
```
def start_link(_) do
  GenServer.start_link(__MODULE__, :no_args, [name: @server_name])
end
```
чтобы соответствовать child spec, чтобы дочерний процесс линковался с супервизором.

Поправим путь к данным: 
```
@cities_file "../lesson_10/data/cities.csv"
```
Добавим запуск через супервизор:
```
def start() do
  children = [
    {__MODULE__, [:no_args]}
  ]
  Supervisor.start_link(children, strategy: :one_for_all)
end
```
Запускаем и проверяем:
```
iex(6)> Lesson_11.PathFinder.start()
{:ok, #PID<0.163.0>}
iex(7)> Lesson_11.PathFinder.get_route("Москва", "Владивосток")
{:error, :no_route}
iex(9)> Lesson_11.PathFinder.get_route("Москва", "Астрахань")
{:ok, ["Москва", "Мурманск", "Астрахань"], 5469}
```

## Супервизор как отдельный модуль

Нужно включить правильную версию erl, чтобы работал observer:
```
source /home/y_zhloba/dev/erl-24.0.1/activate
```
Но без Application это дерево все равно не видно.

```
c "lib/agent_with_sup.exs"
c "lib/gen_server_with_sup.exs"
c "lib/sup.exs"

iex(13)> Lesson_11.RootSup.child_spec(:no_args)
%{
  id: Lesson_11.RootSup,
  start: {Lesson_11.RootSup, :start_link, [:no_args]},
  type: :supervisor
}
iex(14)> Lesson_11.AgentSup.child_spec(:no_args)
%{
  id: Lesson_11.AgentSup,
  start: {Lesson_11.AgentSup, :start_link, [:no_args]},
  type: :supervisor
}

Lesson_11.MyApp.start_sup_tree()

Lesson_11.ShardingAgent.find_node(:agent_a, 5)
Lesson_11.ShardingAgent.find_node(:agent_b, 5)
Lesson_11.PathFinder.get_route("Москва", "Владивосток")
Lesson_11.PathFinder.get_route("Москва", "Астрахань")
```

A supervisor may be started directly with a list of children via start_link/2 
or you may define a module-based supervisor that implements the required callbacks.

You can write supervisors as separate modules, but the Elixir style is to include them inline.


use Supervisor also defines a child_spec/1 function which allows us to run MyApp.Supervisor as a child of another supervisor or at the top of your supervision tree as:

A general guideline is to use the supervisor without a callback module only at the top of your supervision tree, generally in the Application.start/2 callback. 

Запуск supervisor похож на запуск gen_server.
start_link -> init
 Instead of calling Supervisor.start_link/2 with a list of children that are automatically initialized, we manually initialized the children by calling Supervisor.init/2 inside its init/1 callback.

Вот картинка, аналогичная той, что мы видели в 10-м уроке:
![supervision_tree](http://yzh44yzh.github.io/img/practical_erlang/supervisor_init.png)
TODO переделать картинку для эликсир

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

TODO: модуль все равно есть, только он генерируется неявно. Так?


