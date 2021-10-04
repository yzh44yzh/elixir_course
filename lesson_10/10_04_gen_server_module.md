# GenServer Module

Давайте еще раз посмотрим на сервер, который мы написали.

![Custom GenServer](./img/gen_server.custom.png)

У нас есть публичный АПИ модуля, абстрактный (generic) call, абстрактный loop и обработчики запросов.

АПИ и call работают в потоке клиента, loop и обработчики работают в потоке сервера.

АПИ и обработчики реализуют специфичное поведение сервера, а call и loop являются одинаковыми для любых серверов. Поэтому их лучше вынести из модуля в отдельную библиотеку, чтобы переиспользовать в других модулях. 

Стандартный GenServer устроен точно так же:

![Standard GenServer](./img/gen_server.standard.png)

Те же 4 части (АПИ, call, loop и обработчики) я расположил квадратом.

Два верхних блока (АПИ и call) работают в потоке клиента. Два нижних блока (обработчики и loop) работают в потоке сервера.

Два левых блока (АПИ и обработчики) -- это специфичный код нашего модуля. Два правых блока (call и loop) реализованы внутри фреймворка OTP.


## Сервер PathFinder

### Описание задачи

Возьмем задачу немного сложнее, чем добавление и удаление элементов в списке. Реализуем сервер, который хранит граф с информацией о городах и расстояниях между ними. АПИ сервера позволяет указать два города, и получить путь в графе и расстояние между ними. 

Изначально данные хранятся в [csv-файле](./data/cities.csv):
```
Архангельск,Брянск,1598
Архангельск,Воронеж,1723
Архангельск,Казань,2033
Архангельск,Москва,1234
...
```

На старте сервер загружает этот файл и строит по нему граф:
![Граф городов](./data/cities.png)

Затем сервер принимает запросы:
```
PathFinder.get_route("Санкт-Петербург", "Самара")
```
и отвечает на них:
```
{["Санкт-Петербург", "Брянск", "Волгоград", "Самара"], 3764}
```

### Работа с графом

Для работы с графом мы возьмем эрланговский модуль [:digraph](https://erlang.org/doc/man/digraph.html). Он входит в стандартную библиотеку, так что нам не придется подключать зависимости. Модуль реализует ориентированный граф (directed graph), а для нашей задачи нужен неориентированный (undirected). Увы, реализации неориентированного графа нет в стандартной библиотеке. 

(В Эликсир есть библиотека [libgraph](https://github.com/bitwalker/libgraph), которая дает более мощное АПИ для работы с графами, и более эффективную реализацию).

Работа с :digraph выглядит так:
```
iex(1)> graph = :digraph.new()
iex(2)> :digraph.add_vertex(graph, :a)
iex(3)> :digraph.add_vertex(graph, :b)
iex(4)> :digraph.add_vertex(graph, :c)
iex(5)> :digraph.add_edge(graph, :a, :b)
iex(6)> :digraph.add_edge(graph, :b, :c)
iex(7)> :digraph.get_short_path(graph, :a, :c)
[:a, :b, :c]
iex(8)> :digraph.get_short_path(graph, :c, :a)
false
```

Важный нюанс -- это АПИ сделано не в функциональном стиле. Функции *add_vertex* и *add_adge* не возвращают новый граф, а мутируют существующий. Дело в том, что граф трудно реализовать в иммутабельных структурах данных. Такая реализация хотя и возможна, но не эффективна. В BEAM есть возможность работать с мутабельными данными -- это ETS-таблицы, которые мы будем изучать позже в нашем курсе.

Второй важный нюанс -- для каждой пары городов мы будем добавлять два ребра в обоих направлениях, и таким образом реализуем неориентированный граф поверх ориентированного. Что тоже не очень эффективно, но для учебной задачи подойдет.


### Инициализация сервера

Для запуска сервера мы вызваем:
```
GenServer.start(ModuleName, initial_arguments, server_options)
```
В нашем случае это:
```
GenServer.start(PathFinder, :no_args, [name: PathFinder])
```

GenServer.start принимает имя модуля, в котором должно быть реализовано специфичное для сервера поведение -- публичное АПИ, инициализация состояния и обработчики запросов.

Вторым аргументом он принимает некие данные, которые понадобятся серверу при инициализации. В нашем случае никакие данные не нужны, так что я передаю атом `:no_args`. (В этой ситуации часто передают пустой список).

Третий аргумент -- стандартные настройки, определяющие поведение сервера. Мы передаем настройку `name`, благодаря чему сервер регистрируется по указанному имени. И с ним можно взаимодействовать по имени, а не по pid.

GenServer -- это модуль стандартной библиотеки, который реализует ту самую generic часть (call и loop), которую мы видели в своей кастомной реализации. На самом деле он реализует намного больше, чем просто call и loop. 

(Эликсировский модуль GenServer является оберткой над эрланговским модулем :gen_server. В сравнении с эрлангом он добавляет некоторые удобства. Например, автоматически генерирует код всех обработчиков. В эрланге этот код нужно писать вручную.)

При вызове `GenServer.start` запускается новый процесс, и в нем выполянется функция `PathFinder.init`. Это один из 6-ти обработчиков, которые нужно определять в своем модуле. Функция init принимает initial_arguments и формирует состояние процесса. 

Внутри init мы читаем файл, парсим данные из него и создаем граф.

Вызов GenServer.call блокируется, пока не завершится init. После этого процесс входит в loop с нужны состоянием, и готов обрабатывать запросы.


### Обработка запроса

Чтобы отправить запрос серверу мы вызываем:
```
GenServer.call(ServerNameOrPid, Request)
```
В нашем случае это:
```
GenServer.call(PathFinder, {:get_route, from_city, to_city})
```

Запрос проходит через call и loop, и попадает в обработчик:
```
def handle_call({:get_route, from_city, to_city}, _from, state) do
```
То есть, обработчик получает запрос и состояние сервера (еще он получает аргумент `from` -- это информация об отправителе, но мы сейчас не будем это рассматривать).

Обработчик должен вернуть ответ на запрос, и новое состояние сервера:
```
{:reply, reply, state}
```

В нашем случае состояние не меняется. Но в принципе можно было бы предусмотреть АПИ для модификации графа.

Поиск пути в графе делает библиотека. А вычислить суммарное расстояние нужно нам самим. 

Расстояния между городами можно было бы хранить как аттрибут ребра графа. К сожалению, библиотека не предоставляет удобного способа извлечь этот аттрибут. Поэтому мы будем хранить расстояния отдельно, в словаре:
```
%{
   {city1, city2} => distance
}
```
А сам словарь будем хранить, конечно, в состоянии сервера, вместе с графом.


### Демонстрация работы

```
iex(1)> c "lib/path_finder.exs"
[PathFinder]
iex(6)> PathFinder.start()
{:ok, #PID<0.126.0>}

> PathFinder.get_route("Санкт-Петербург", "Тюмень")
{["Санкт-Петербург", "Брянск", "Тюмень"], 3561}

> PathFinder.get_route("Санкт-Петербург", "Самара")
{["Санкт-Петербург", "Брянск", "Волгоград", "Самара"], 3764}

> PathFinder.get_route("Москва", "Воронеж")
{["Москва", "Архангельск", "Воронеж"], 2957}

> PathFinder.get_route("Екатеринбург", "Махачкала")
{["Екатеринбург", "Астрахань", "Тюмень", "Брянск", "Махачкала"], 9408}
```

### PathFinder как GenServer

Посмотрим еще раз на реализацию PathFinder как GenServer:
![PathFinder/GenServer](./img/gen_server.standard.png)

Два левых квадрата -- это модуль PathFinder. Он реализует публичное АПИ, которое представлено функциями `start` и `get_route`. Эта часть работает в потоке клиента. И модуль также реализует обработчики `init` и `handle_call`, которые работают в потоке сервера.

Два правах квадрата -- это код внутри фреймворка OTP. Мы будем создавать разные сервера, с разным АПИ, разным состоянием, разными обработчиками. Но все они поддерживаются одним и тем же кодом внутри OTP.


## GenServer behaviour

Чтобы OTP мог взаимодействовать с кастомным модулем, должен быть описан способ взаимодействия. Это описание реализовано с помощью **behaviour**. Behaviour очень похож на **интерфейс** в языке Java и некоторых других языках. 

Behaviour указывает, какие функции обратного вызова (callback) должны быть реализованы в модуле, чтобы модуль был совместим с ним. Например, GenServer behaviour указывает, что модуль должен реализовать 6 функций:
- init
- handle_call
- handle_cast
- handle_info
- code_change
- terminate

Behaviour описывает, какие аргументы будут получать эти функции, и что они должны возвращать. 

Когда мы добавляем в модуль код:
```
use GenServer
```
мы вызываем сложный макрос, который вставляет в наш модуль некоторую реализацию по-умолчанию для всех этих функций. Благодаря этому, нам не обязательно явно реализовывать все 6 этих функций (как это приходится делать в Эрланг), а достаточно реализовать только те, который нам нужны.

Функции, реализующие behaviour, мы помечаем:
```
@impl true
```
Это позволяет компилятору проверить, что они все реализованы и имеют правильную сигнатуру.


TODO read
+ https://hexdocs.pm/elixir/1.12/GenServer.html
- https://erlang.org/doc/design_principles/gen_server_concepts.html
- https://erlang.org/doc/man/gen_server.html

Я упустил еще 2 callbacks:

format_status(reason, pdict_and_state)
Invoked in some cases to retrieve a formatted version of the GenServer status.
https://erlang.org/doc/man/gen_server.html#Module:format_status-2

handle_continue(continue, state)
Invoked to handle continue instructions.
непонятно, что это
https://erlang.org/doc/man/gen_server.html#Module:handle_continue-2
It is useful for performing work after initialization or for splitting the work in a callback in multiple steps, updating the process state along the way.


### init

init(init_arg)
Invoked when the server is started. start_link/3 or start/3 will block until it returns.

init(init_arg :: term()) ::
  {:ok, state}
  | {:ok, state, timeout() | :hibernate | {:continue, term()}}
  | :ignore
  | {:stop, reason :: any()}
when state: any()

Returning :ignore will cause start_link/3 to return :ignore and the process will exit normally without entering the loop or calling terminate/2. 

Returning {:stop, reason} will cause start_link/3 to return {:error, reason} and the process to exit with reason reason without entering the loop or calling terminate/2.

GenServer.start works synchronously. It returns only after init/1 callback has finished in server process.
Client process is blocked until the server process is initialized.

If init/1 returns {:stop, reason} client will receive {:error, reason}.
If init/1 returns :ignore, client will receive :ignore.
The first is an error situation, the second is a normal situation.


### handle_call

handle_call(request, from, state)
Invoked to handle synchronous call/3 messages. call/3 will block until a reply is received (unless the call times out or nodes are disconnected).

handle_call(request :: term(), from(), state :: term()) ::
  {:reply, reply, new_state}
  | {:reply, reply, new_state, timeout() | :hibernate | {:continue, term()}}
  | {:noreply, new_state}
  | {:noreply, new_state, timeout() | :hibernate | {:continue, term()}}
  | {:stop, reason, reply, new_state}
  | {:stop, reason, new_state}
when reply: term(), new_state: term(), reason: term()

GenServer.call doesn't wait indefinitely for a responce. 5 sec timeout by default.
If server process terminates while client is waiting for resonce, GenServer detects it and raises a corresponding error in the client process. 

loop isn't CPU-intensive. Waiting for a message puts process in a suspended state and doesn't waste CPU cycles.

handle_call должен обработать сообщение, сформировать ответ для клиента и
новое состояние для сервера.

Есть несколько вариантов возвращаемого значения. Но мы не будем рассматривать
все возможные случаи. Чаще всего мы отвечаем {reply, Reply, NewState}.

Обычно каждой АПИ функции модуля соответствует отдельное сообщение, а
каждому сообщению отдельная ветка handle\_call. Если АПИ большое, то и
веток handle\_call много.

```
my_api_1(A) ->
    gen_server:call(?MODULE, {msg1, A}).
my_api_2(A, B) ->
    gen_server:call(?MODULE, {msg2, A, B}).
my_api_3(A, B, C) ->
    gen_server:call(?MODULE, {msg3, A, B, C}).
...
handle_call({msg1, A}, _From, State) ->
...
handle_call({msg2, A, B}, _From, State) ->
...
handle_call({msg3, A, B, C}, _From, State) ->
```

Поэтому внутри handle\_call много кода лучше не писать, а выносить его в отдельные функции.


### handle_cast

handle_cast(request, state)
Invoked to handle asynchronous cast/2 messages.

handle_cast(request :: term(), state :: term()) ::
  {:noreply, new_state}
  | {:noreply, new_state, timeout() | :hibernate | {:continue, term()}}
  | {:stop, reason :: term(), new_state}
when new_state: term()

Вызов gen_server:call блокирует клиента, пока сервер не обработает его запрос и не вернет ответ.
Бывают случаи, когда клиенту ответ сервера не нужен. Тогда лучше использовать gen\_server:cast.
Клиент не блокируется и не ждет ответ сервера. Но сервер получает и обрабатывает сообщение.

Для этого вызывается callback-функция handle_cast:

```
do_something(A, B) ->
    gen_server:cast(?MODULE, {do_something, A, B}),
    ok.
...
handle_cast({do_something, A, B}, State) ->
    NewState = ...
    {noreply, NewState};
```

handle_cast должен вернуть измененное состояние.


### handle_info

handle_info(msg, state)
Invoked to handle all other messages.

handle_info(msg :: :timeout | term(), state :: term()) ::
  {:noreply, new_state}
  | {:noreply, new_state, timeout() | :hibernate | {:continue, term()}}
  | {:stop, reason :: term(), new_state}
when new_state: term()

Любой поток из любого места в коде может отправить серверу сообщение
оператором **!**.  Так делать не рекомендуется, потому что это вызовы
в обход API сервера.  Но иногда так делают.

Если сообщения в функции loop сервера приходят не из gen\_server:call/cast,
то они обрабатываются в callback-функции handle\_info.

```
handle_info({some_message, A, B}, State) ->
    NewState = ...
    {noreply, NewState};
```

The monitoring :DOWN messages are an example of this.

Сервер и сам может отправлять себе сообщения таким образом. Например,
для отложенной инициализации (это мы рассмотрим ниже), или для
выполнения повторяющихся операций через интервалы времени.


### terminate

terminate(reason, state)
Invoked when the server is about to exit. It should do any cleanup required.

terminate/2 is called if the GenServer traps exits (using Process.flag/2) and the parent process sends an exit signal
or a callback (except init/1) does one of the following:
- returns a :stop tuple
- raises (via Kernel.raise/2) or exits (via Kernel.exit/1)
- returns an invalid value
TODO please read the "Shutdown values (:shutdown)" section in the Supervisor module.
TODO надо бы проверить все эти варианты
Note that a process does NOT trap exits by default

Therefore it is not guaranteed that terminate/2 is called when a GenServer exits. For such reasons, we usually recommend important clean-up rules to happen in separated processes either by use of monitoring or by links themselves. There is no cleanup needed when the GenServer controls a port (for example, :gen_tcp.socket) or File.io_device/0, because these will be closed on receiving a GenServer's exit signal and do not need to be closed manually in terminate/2.

Этот callback вызывается, когда gen_server останавливается.  Если
поток в процессе своей работы занимал какие-то ресурсы (соединение с
базой данных, сокеты, файлы и т.д.), то по правилам OTP предлагается
освобождать их здесь.

Или если поток накопил какие-то данные, которые нужно куда-то
сохранить, то можно делать это здесь. Хотя надежнее сохранять данные
периодически, через регулярные интервалы времени. Это минимизирует
потери в случае аварийного завершения потока. terminate тогда не
вызывается.


### code_change

code_change(old_vsn, state, extra)
Invoked to change the state of the GenServer when a different version of a module is loaded (hot code swapping) and the state's term structure should be changed.

Этот callback вызывается при горячем обновлении кода. Такое обновление
тесно связано с релизами, и мы не рассматриваем его в рамках курса.
Но для полноты изложения callback упомянем.

В новой версии кода возможно изменилось состояние процесса. В #state{}
могло появиться что-то новое, или что-то было убрано, или вообще
состояние стало храниться в совсем другой структуре данных.

code_change принимает на входе старый #state{}, и должен его
преобразовать и вернуть новый #state{}.

TODO это в некоторой степени похоже на миграцию БД.


