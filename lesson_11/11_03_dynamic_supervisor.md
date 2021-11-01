# Динамический супервизор

Мы создали статичное дерево супервизоров. Все процессы в нем запускаются на старте узла (виртуальной машины) и живут столько, сколько живет сам узел. 

Часто бывают нужны и короткоживущие процессы. Их можно запускать и под обычным супервизором с настройкой restart: `transient` или `temporary`. Но в Эликсире для этого есть специальный вид супервизора -- динамический супервизор [DynamicSupervisor](https://hexdocs.pm/elixir/1.12/DynamicSupervisor.html)

Рассмотрим его на примере задачи:

Допустим, наш сервис -- это один из многих в системе, реализованной в микросервисной архитектуре. В системе есть описаны правила, каким клиентам в какие сервисы разрешено делать запросы. Эти правила хранит сервис авторизации, откуда их получают все остальные сервисы, в том числе наш. 

Нам нужно реализовать процесс, который запрашивает данные в сервисе авторизации, и сохраняет их в некое локальное хранилище. Данные меняются редко, поэтому не нужно постоянно отслеживать изменения в них. А нужно загрузить их один раз на старте сервиса, и иногда, при получении определённого события загружать их повторно.

Нет необходимости постоянно держать долгоживущий процесс для такой задачи. Здесь лучше подойдет короткоживущий процесс, который выполнит свою работу и завершится. А при необходимости запустится снова, снова выполнит работу и завершится.

Динамический супервизор не запускает ничего в `init`. Вместо этого нужен явный вызов `start_child/2` для каждого дочернего процесса:
```
defmodule AuthDataLoaderSup do

  use DynamicSupervisor

  def start_link(args) do
    DynamicSupervisor.start_link(__MODULE__, args, name: __MODULE__)
  end

  def start_child() do
    url = "http://auth_service.some_cluster.data_center/rules"
    spec = {Lesson_11.AuthDataLoader, [url]}
    DynamicSupervisor.start_child(__MODULE__, spec)
  end

  @impl true
  def init(_args) do
    DynamicSupervisor.init(strategy: :one_for_one)
  end

end
```

Рабочий процесс реализуем как GenServer с отложеной инициализацией:
```
defmodule AuthDataLoader do

  use GenServer, restart: :transient

  def start_link(auth_service_url) do
    GenServer.start_link(__MODULE__, auth_service_url, [])
  end

  @impl true
  def init(auth_service_url) do
    IO.puts("worker #{inspect self()} started")
    {:ok, auth_service_url, {:continue, :delayed_init}}
  end

  @impl true
  def handle_continue(:delayed_init, auth_service_url) do
    load(auth_service_url) |> save()
    IO.puts("work done")
    {:stop, :normal, auth_service_url}
  end

  defp load(auth_service_url) do
    IO.puts("load data from #{auth_service_url}")
    Process.sleep(1000)
    [:rule_1, :rule_2, :rule_3]
  end

  defp save(data) do
    IO.puts("save data #{inspect data}")
  end

end
```
Загрузку и сохранение данных мы просто имитируем.

Запускаем:
```
defmodule MyService do

  def start() do
    children = [
      Lesson_11.AuthDataLoaderSup,
    ]
    Supervisor.start_link(children, strategy: :one_for_one) 
  end

  def update_auth_rules() do
    Lesson_11.AuthDataLoaderSup.start_child()
  end

end
```
и смотрим, как это работает:
```
iex(1)> c "lib/dyn_sup.exs"
[Lesson_11, Lesson_11.AuthDataLoader, Lesson_11.AuthDataLoaderSup,
 Lesson_11.MyService]
iex(2)> Lesson_11.MyService.start()
{:ok, #PID<0.125.0>}
iex(3)> Lesson_11.MyService.update_auth_rules()
worker #PID<0.128.0> started with http://auth_service.some_cluster.data_center/rules
load data from http://auth_service.some_cluster.data_center/rules
{:ok, #PID<0.128.0>}
save data [:rule_1, :rule_2, :rule_3]
work done
iex(4)> Lesson_11.MyService.update_auth_rules()
worker #PID<0.130.0> started with http://auth_service.some_cluster.data_center/rules
load data from http://auth_service.some_cluster.data_center/rules
{:ok, #PID<0.130.0>}
save data [:rule_1, :rule_2, :rule_3]
work done
```

`start_child` возвращает `{:ok, pid}` или `{:error, {:already_started, pid}}`, что является удобным способом получить pid существующего рабочего процесса или запустить новый процесс, если нет существующего. Это исключает race condition при попытке запустить процесс с одинаковым id из разных мест, так как start_child сериализуется в супервизоре.

