# Использование супервизора

## Запускаем Agent под супервизором

Запуск процессов не под супервизором является антипаттерном.
Либо напрямую под супервизором, либо процесс должен быть слинкован с другим, находящимся под супервизором.


## Запускаем Task под супервизором

## Запускаем GenServer под супервизором


### Child Specification для GenServer

Elixir allows you to pass a tuple with the module name and the start_link argument instead of the specification:
The supervisor will then invoke Stack.child_spec([:hello]) to retrieve a child specification. Now the Stack module is responsible for building its own specification

Luckily for us, use GenServer already defines a Stack.child_spec/1 exactly like this:
```
def child_spec(arg) do
  %{
    id: Stack,
    start: {Stack, :start_link, [arg]}
  }
end
```

If you need to customize the GenServer, you can pass the options directly to use GenServer:
```
use GenServer, restart: :transient
```

You can specify a worker by giving its module name (or a tuple containing the module and the initial arguments). 
In this case, the supervisor assumes you’ve implemented a child_spec function in that module,
and calls that function to get the specification.

When you add the line ```use GenServer``` to a server module,
Elixir will define a default child_spec function in that module.

This function by default returns a map that tells the supervisor that 
the start function will be start_link 
and that the restart strategy will be :permanent. 
You can override these defaults with the options you give use GenServer.


## Супервизор как отдельный модуль

TODO запусить из этого модуля все: agent, task, gen_server, another sup

A supervisor may be started directly with a list of children via start_link/2 
or you may define a module-based supervisor that implements the required callbacks.

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

TODO: модуль все равно есть, только он генерируется неявно. Так?


