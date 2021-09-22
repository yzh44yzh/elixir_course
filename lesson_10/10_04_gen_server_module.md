TODO read
https://hexdocs.pm/elixir/1.12/GenServer.html


# GenServer
TODO хорошее название темы

TODO тут нужно два раздела: 
- рассмотреть устройство GS на картинках
  - нужны 2 картинки: кастомный и настоящий gen server
- рассмотреть конкретный пример, описать callbacks на этом примере.


Мы реализовали свой GenServer, теперь пора посмотреть на настоящий :)

Наш gen_server стостоит из 3х частей:
- внешнее АПИ: start, add, remove, check, show, stop;
- общая (generic) часть: call и loop;
- обработка сообщений: handle_call.

АПИ и call выполняются в потоке клиента, loop и handle_call выполняются в потоке сервера.

Настоящий gen\_server устроен сложнее. Код также делится на общую
(generic) часть, и кастомную (custom) часть. Общая часть реализована в
нескольких модулях OTP фреймворка (gen\_server, gen,
proc\_lib). Кастомную часть мы должны реализовать в своем модуле.

![gen_server](./img/gen_server.png) # TODO перерисовать для Elixir

На картинке два левых квадрата (верхний и нижний), соответствуют
нашему модулю.  Два правых квадрата соответствуют коду OTP. Два
верхних квадрата выполняются в потоке клиента, два нижних квадрата
выполняются в потоке сервера.

Левый верхний квадрат -- это публичное АПИ нашего модуля. Отсюда мы
обращаемся к OTP фреймворку. В кастомной реализации, которую мы делали
на прошлом уроке, этот квадрат соответствует функциям start,
add\_item, remove\_item, show\_items.

Правый верхний квадрат -- это часть OTP, generic код, выполняющийся
в потоке клиента. Соответствует функции call в нашей кастомной реализации.

Правый нижний квадрат -- это часть OTP, выполняющаяся в потоке сервера.
Соответствует функции loop в нашей реализации. Только там нет кастомной
обработки сообщений. А вместо этого OTP вызывает функции обратного вызова
(callback) нашего модуля.

Левый нижний квадрат -- функции обратного вызова, принадлежащие нашему модулю,
и работающие в потоке сервера.

**behaviour** в эрланг -- это аналог интерфейсов в джава. Он
описывает, какие callback-функции должны быть определены, их имена и
аргументы.

**behaviour(gen_server)** требует, чтобы наш модуль определил функции
init/1, handle\_call/3, handle\_cast/2, handle\_info/2, terminate/2 и code\_change/3.

TODO придумать пример, реализовать на GenServer

Дальше мы разберем, как взаимодействуют наш модуль и gen\_server.
на примере модуля
[wg\_push\_sender](https://github.com/wgnet/wg_push/blob/master/src/wg_push_sender.erl),
из
[библиотеки wg_push](https://github.com/wgnet/wg_push/).
Это библиотека для отправки сообщений на iOS устройства через
Apple Push Notification Service.


## инициализация

Все начинается с функции **start\_link/0**:

```
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).
```

Здесь мы просим gen\_server запустить новый поток.

Макрос **?MODULE** разворачивается в имя текущего модуля.
Можно было написать:
```
gen_server:start_link({local, wg_push_sender}, wg_push_sender, [], []).
```
получится тоже самое.

Первый аргумент **{local, ?MODULE}** -- это имя, под которым нужно
зарегистрировать поток. Это если мы хотим обращаться к нашему серверу
по имени. Иначе вызываем gen\_server:start\_link/3, и созданный поток
не будет регистрироваться.

Второй аргумент **?MODULE** -- это имя модуля, callback-функции
которого будет вызывать gen\_server.

Третий аргумент -- это набор параметров, которые нужны при
инициализации.  В нашем случае никакие не нужны.

Четвертый аргумент -- настройки поведения gen\_server. Они довольно
специфичны, и необходимость что-то в них менять не возникает. Но
загляните в документацию, это полезно :)

Дальше происходит некая магия в правом верхнем квадрате, в результате
которой создается серверный поток. Этому потоку нужно получить свое
начальное состояние.  Для этого вызывается первый callback **init/1**.

```
init([]) ->
    {ok, #state{
            apns_host = application:get_env(wg_push, apns_host, "gateway.sandbox.push.apple.com"),
            apns_port = application:get_env(wg_push, apns_port, 2196)
           }}.
```

Аргумент init, это данные, которые мы передавали третьим аргументом в
gen\_server:start\_link.  Здесь нужно создать структуру данных,
которая будет хранить состояние сервера.

Часто для этого описывают record с именем **state**.

```
-record(state, {
        apns_host :: string(),
        apns_port :: integer(),
        connections = orddict:new() :: orddict:orddict(file:name_all(), port())
         }).
```

После того, как функция init возвращает #state{}, сервер готов к работе.


## gen_server:call

Теперь посмотрим, как делается запрос от клиента к серверу, на примере
API-функции send_messages.

```
send_messages(Messages, SSL_Options) ->
    gen_server:call(?MODULE, {send_messages, Messages, SSL_Options}).
```

Здесь мы вызываем gen_server:call с двумя аргументами. Первый аргумент
-- pid сервера или имя, под которым он зарегистрирован. Второй
аргумент -- сообщение, которое посылается серверу.

В недрах OPT вызов проходит через call и loop, и затем вызывается
callback-функция handle_call. Ей передаются три аргумента: сообщение
от клиента, кортеж {pid клиента, reference} и состояние сервера.
Второй аргумент обычно не используется.

```
handle_call({send_messages, Messages, SSL_Options}, _From, State) ->
    {Reply, State3} = send_messages(Messages, SSL_Options, State),
    {reply, Reply, State3};
```

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


## другие callback-функции

### gen_server:cast/handle_cast

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


### message/handle_info

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

Этот callback вызывается при горячем обновлении кода. Такое обновление
тесно связано с релизами, и мы не рассматриваем его в рамках курса.
Но для полноты изложения callback упомянем.

В новой версии кода возможно изменилось состояние процесса. В #state{}
могло появиться что-то новое, или что-то было убрано, или вообще
состояние стало храниться в совсем другой структуре данных.

code_change принимает на входе старый #state{}, и должен его
преобразовать и вернуть новый #state{}.

TODO это в некоторой степени похоже на миграцию БД.


## Сравнение с ООП

Вы могли заметить некоторое сходство с ООП. Есть объект с внутренним
состоянием, публичным АПИ и скрытой реализацией. Таких объектов
(потоков) на базе одного класса (модуля) можно создать много. У всех у
них будет одинаковое по структуре, но разное по содержанию
состояние. Объекты могут взаимодействовать друг с другом, обмениваясь
сообщениями.

Если серверный поток регистрируется под определенным именем, то это
"одиночка" (singleton). Он такой один, и к нему можно обращаться по
имени:

```
gen_server:call(some_name, some_message)
```

Если поток не регистрируется, то таких объектов может быть много, и нужно
обращаться к ним по Pid:

```
gen_server:call(Pid1, some_message).
gen_server:call(Pid2, some_message).
```

Похожесть есть, но есть и нюансы. Для ООП объекта вполне нормально
вызывать свои собственные методы. А с gen\_server можно попасть в
коварную ловушку :)


# Gen Server

GenServer.start works synchronously. It returns only after init/1 callback has finished in server process.
Client process is blocked until the server process is initialized.

GenServer.call doesn't wait indefinitely for a responce. 5 sec timeout by default.

If server process terminates while client is waiting for resonce, GenServer detects it and raises a corresponding error in the client process. 

If init/1 returns {:stop, reason} client will receive {:error, reason}.
If init/1 returns :ignore, client will receive :ignore.
The first is an error situation, the second is a normal situation.


It is common to create long-running processes that keep their internal state and can respond to various messages.
Server process:
- runs for a long time (or forever);
- can handle various requests (messages).

loop isn't CPU-intensive. Waiting for a message puts process in a suspended state and doesn't waste CPU cycles.

Interface functions (client process)
Implementation functions (server process)

В книге Sasa Jiric есть идея иметь отдельную interface function для получения ответа.
```
run_async(query)
get_result()
```
Получается что-то типа Agent. Только не хватает индетификатора запроса, чтобы матчить с ответом.

When we write an OTP server, we write a module containing one or more callback functions with standard names. 
OTP will invoke the appropriate callback to handle a particular situation. 

For example, when someone sends a request to our server, 
OTP will call our handle_call function, passing in the request, the caller, and the current server state. 
Our function responds by returning a tuple containing 
an action to take, the return value for the request, and an updated state.

Servers use recursion to loop, handling one request on each call. 
So they can also pass state to themselves as a parameter in this recursive call. 
And that’s one of the things OTP manages for us.


## Sequence.Server

_можно использовать как упражнение_
Let’s write what is possibly the simplest OTP server. 
You pass it a number when you start it up, and that becomes the current state of the server. 
When you call it with a :next_number request, 
it returns that current state to the caller,
and at the same time increments the state, ready for the next call. 
Basically, each time you call it you get an updated sequence number.

```
defmodule Sequence.Server do
  use GenServer
  def init(initial_number) do
    { :ok, initial_number }
  end
  def handle_call(:next_number, _from, current_number) do
    {:reply, current_number, current_number + 1}
  end
end
```

```
use GenServer
``` 
line effectively adds the OTP GenServer behavior to our module. 
This is what lets it handle all the callbacks.
It also means we don’t have to define every callback in our module — 
the behavior defines defaults for all but one of them.

```
$ iex -S mix
iex> { :ok, pid } = GenServer.start_link(Sequence.Server, 100)
{:ok,#PID<0.71.0>}
iex> GenServer.call(pid, :next_number)
100
iex> GenServer.call(pid, :next_number)
101
iex> GenServer.call(pid, :next_number)
102
```

GenServer.start_link function asks GenServer to start a new process and link to us. 
We pass in 
- the module to run as a server: 
- the initial state (100 in this case). 
We could also pass GenServer options as a third parameter, but the defaults work fine here.
We get back a status ( :ok ) and the server’s PID.

A server can support multiple actions by implementing multiple handle_call functions with different first parameters.

Erlang:
```
gen_server:
start_link(Module, Args, Options) -> Result
start_link(ServerName, Module, Args, Options) -> Result
```


## Cast

```
def handle_cast({:increment_number, delta}, current_number) do
  { :noreply, current_number + delta}
end
```

```
iex> r Sequence.Server
.../sequence/lib/sequence/server.ex:2: redefining module Sequence.Server
{Sequence.Server,[Sequence.Server]]
```
Even though we’ve recompiled the code, the old version is still running. 
The VM doesn’t hot-swap code until you explicitly access it by module name.


## Callbacks

OTP works by assuming that your module defines a number of callback functions 
(six, in the case of a GenServer). 

If you were writing a GenServer in Erlang, your code would have to contain implementations of all six.

When you add the line ‘use GenServer‘ to a module, 
Elixir creates default implementations of these six callback functions.
All we have to do is override the ones where we add our own application-specific behavior.

init(start_arguments)
The default GenServer implementation sets the server state to the argument you pass.

handle_call(request, from, state)
The default implementation stops the server with a :bad_call error.

handle_cast(request, state)
The default implementation stops the server with a :bad_cast error.

handle_info(info, state)

terminate(reason, state)
Once we add supervision to our servers, we don’t have to worry about this.

code_change(from_version, state, extra)

format_status(reason, [pdict, state])
The conventional response is [data: [{'State', state_info}]].

If hibernate is returned, the server state is removed from memory 
but is recovered on the next request. 
This saves memory at the expense of some CPU.

The timeout option can be the atom :infinite (which is the default) or a number. 
If the latter, a :timeout message is sent if the server is idle for the specified number of milliseconds.

_TODO надо попробовать_
Try various ways of terminating your server.
- exception
- System.halt(n)




## Agents and Tasks, or GenServer?

When do you use agents and tasks, and when do you use a GenServer?
The answer is to use the simplest approach that works. Agents and tasks are
great when you’re dealing with very specific background activities, whereas
GenServers (as their name suggests) are more general.

You can eliminate the need to make a decision by wrapping your agents and
tasks in modules, as we did in our anagram example. That way you can always
switch from the agent or task implementation to the full-blown GenServer
without affecting the rest of the code base.
