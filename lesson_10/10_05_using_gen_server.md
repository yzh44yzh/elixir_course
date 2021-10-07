# Использование GenServer

В работе с GenServer есть много интересных нюансов. Рассмотрим те, которые важно знать с самого начала:
- отложенная инициализация;
- блокировка на GenServer.call;
- переполнение почтового ящика.


## Отложенная инициализация

Вызов `GenServer.start` блокирует родительский процесс пока не завершится `init` в дочернем процессе. Это может быть нежелательным, если запускается много процессов (что и происходит на старте узла BEAM). 

Если инициализация занимает долгое время, например, нужно получить какие-то данные из базы или из стороннего сервера, то это лучше сделать не сразу в init, а позже.

Для этого есть обработчик `handle_continue`.

```
  @impl true
  def init(:no_args) do
    state = %{}
    {:ok, state, {:continue, :delayed_init}}
  end

  @impl true
  def handle_continue(:delayed_init, state) do
    ...
    state = %{graph: graph, distancies: distancies}
    {:noreply, state}
  end
  
  @impl true
  def handle_cast(:reload_data, state) do
    %{graph: graph} = state
    :digraph.delete(graph)
    state = %{}
    {:noreply, state, {:continue, :delayed_init}}
  end
```

TODO: описать

Заодно исчезло дублирование кода для reload_data.

Здесь исключена ситуация, когда сервер получает запрос раньше, чем успевает инициализироваться. 
Потому что так работает loop -- сервер не возьмет запрос из почтового ящика, пока не завершит выполнение текущего обработчика.
А в данном случае двух обработчиков -- init и handle_continue.


## Deadlock на gen_server:call

С этим сталкивается почти каждый новичок в Erlang, и я тоже в свое
время столкнулся.

Внутри handle\_call нельзя делать вызов gen\_server:call на самого
себя. Обычно такое получается не прямо, а опосредованно. Либо
вызывается функция из публичного АПИ, которая делает gen\_server:call,
либо вызывается какой-то другой модуль, а тот вызывает публичное АПИ
текущего модуля.

Вызов gen\_server:call, это добавление нового сообщения в почтовый
ящик. gen\_server обрабатывает сообщения по очереди. Пока он не
завершит обработку текущего сообщения, он не начнет обработку
следующего. Поэтому если обработка текущего сообщения будет ждать
результат gen\_server:call, то никогда не дождется.  1-й вызов ждет
завершения 2-го, а 2-й вызов ждет завершения 1-го. Это deadlock.

gen\_server:call по дефолту имеет таймаут в 5 секунд. Если за это
время не приходит ответ, то в потоке клиента бросается
исключение. Обычно этим и проявляется такой deadlock.  Но если вы
зачем-то заменили дефолтный таймаут на infinity, то поток в таком
состоянии будет висеть бесконечно.  В какой-то момент его очередь
сообщений исчерпает всю доступную оперативную память, и нода упадет.

Выводы:
- помните про такой deadlock;
- хорошо продумывайте таймауты.

рекомендуемая практика -- отправлять сообщения самому себе


## Переполнение почтового ящика

Theoretically, a process mailbox has an unlimited size. In practice, the mailbox size is limited by available memory.
If messages arrive faster than the process can handle them, the mailbox will constantly grow and increasingly consume memory. Ultimately, a single process may cause an entire system to crash by consuming all the available memory. 
Solution: Optimize message handling and/or split server into multiple processes.

In addition, large mailbox cause performance slowdowns. 
When a new message arrives, receive iterates through all existed messages before processing the new one.
Solution: catch-all receive clause that deal with unexpected kinds of messages.

Deep-copying is an in-memory operation, so it should be reasonably fast. 
Having many processes frequently send big messages may affect system performance.

A special case where deep-copying doesn't take place involves binaries larger than 64 bytes. 
(TODO kbytes?)
These are maintained on a special shared binary heap. 
