# Использование GenServer

В работе с GenServer есть много интересных нюансов. Рассмотрим те, которые важно знать с самого начала:
- отложенная инициализация;
- блокировка на GenServer.call;
- переполнение почтового ящика.


## Отложенная инициализация

Вызов `GenServer.start` блокирует родительский процесс пока не завершится `init` в дочернем процессе. Это может быть нежелательным, если запускается много процессов (что и происходит на старте узла BEAM). 

Если инициализация занимает долгое время, например, когда нам нужно получить какие-то данные из базы или из стороннего сервера, то это лучше сделать не сразу в init, а позже.

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
```

Здесь мы не создаем состояние в init, а быстро отвечаем `:ok`, что позволяет разблокировать родительский процесс. Но после init вызывается обработчик handle_continue, который и выполняет всю инициализацию. 

Здесь маловероятна ситуация, что сервер получит запрос раньше, чем успеет инициализироваться. handle_continue реализован через отправку сообщения самому себе. Это сообщение попадает в почтовый ящик первым, и обработается первым, раньше, чем придут запросы от клиентов.

Аналогично мы можем сделать на `:reload_data` -- очистить текущее состояние и делегировать создание нового состояния в handle_continue, чтобы не дублировать код.

```
@impl true
def handle_continue(:delayed_init, state) do
  ...
  state = %{graph: graph, distancies: distancies}
  {:noreply, state}
end

@impl true
def handle_cast(:reload_data, state) do
  %{graph: graph} = state
  # This is wrong, don't do this!
  :digraph.delete(graph) 
  state = %{} 
  {:noreply, state, {:continue, :delayed_init}}
end
```

Однако здесь вполне возможно, что сервер будет обрабатывать другие запросы после вызова handle_cast и до вызова handle_continue. Поэтому важно, чтобы state всегда был консистентным на выходе из любого обработчика. 

Правильная реализация будет такая:
```
@impl true
def handle_continue(:delayed_init, state) do
  case state do
    %{} -> :ok
    %{graph: graph} -> :digraph.delete(graph)
  end
  graph = :digraph.new([:cyclic])
  data = load_data()
  Enum.reduce(data, graph, &add_item/2)
  distancies = make_distancies_map(data)
  state = %{graph: graph, distancies: distancies}
  {:noreply, state}
end

@impl true
def handle_cast(:reload_data, state) do
  {:noreply, state, {:continue, :delayed_init}}
end
```

## Блокировка на GenServer.call

Частая ошибка новичка -- внутри обработчика `handle_call` сделать вызов `GenServer.call` на тот же процесс. Обычно это случается не напрямую, а через вызов публичного АПИ того же модуля:
```
def some_fun() do
  GenServer.call(@server_name, :some_msg)
end

def another_fun() do
  GenServer.call(@server_name, :another_msg)
end

handle_call(:some_msg, _from, state)
  res = another_fun()
  {:reply, res, state}
end
```
В этом случае процесс заблокируется на `res = another_fun()` и будет ждать ответ. `GenServer.call` положит сообщение в почтовый ящик. Но сервер не возьмет это сообщение в обработку, пока не завершит выполнение текущего обработчика. А выполнение текущего обработчика заблокировано.

Заканчивается это крашем процесса:
```
** (exit) exited in: GenServer.call(#PID<0.113.0>, :some, 5000)
    ** (EXIT) time out
    (elixir 1.11.3) lib/gen_server.ex:1027: GenServer.call/3
```
Срабатывает дефолтный таймаут в 5 секунд на GenServer.call. Но если вдруг вы указали таймаут `:infinity`, то процесс заблокируется навсегда.

Блокировка может быть более сложной, через цепочку вызовов, в которой участвуют несколько процессов. Например, процесс А делает call на процесс Б, а процесс Б делает call на процесс А -- результат такой же.

Избежать этой проблемы не сложно если понимать, как работает GenServer и следить за тем, что вы делаете в обработчиках.


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
