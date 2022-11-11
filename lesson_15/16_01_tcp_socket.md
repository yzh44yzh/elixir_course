# TCP Socket

Transmission Control Protocol

Вспомним в общих чертах, что такое TCP:
- надежный протокол передачи данных, гарантирует доставку сообщения и очередность доставки;
- постоянное соединение клиента и сервера, имеет состояние;
- дополнительные накладные расходы на установку и закрытие соединения и на передачу данных.

Долго держать постоянные соединения с многими тысячами клиентов накладно. Все соединения должны работать независимо друг от друга, а это значит -- в разных процессах. Для многих языков программирования (но не для Эликсир) это серьезная проблема.

Именно поэтому так популярен протокол HTTP, который хоть и работает поверх TCP сокета, но подразумевает короткое время взаимодействия. Это позволяет относительно небольшим числом процессов (десятки-сотни) обслуживать значительно большее число клиентов (тысячи, десятки тысяч).

В некоторых случаях остается необходимость иметь долгоживущие постоянные соединения между клиентом и сервером. Например, для чатов или для баз данных. 

Для работы с TCP используется модуль gen_tcp.
https://www.erlang.org/doc/man/gen_tcp.html

Работать с TCP сокетом сложнее, чем с UDP. У нас появляются роли клиента и сервера, требующие разной реализации. Рассмотрим разные варианты реализации сервера.

## Наивный сервер

Обслуживает только одного клиента.

Клиент:
```
$ telnet localhost 1234
Trying 127.0.0.1...
Connected to localhost.
Escape character is '^]'.
hello
ECHO hello
42
ECHO 42

ECHO 
123
ECHO 123
^]
telnet> quit
Connection closed.

$ telnet localhost 1234
Trying 127.0.0.1...
Connected to localhost.
Escape character is '^]'.
hello
ECHO hello
^]
telnet> quit
Connection closed.
```

Сервер:
```
iex(3)> NaiveServer.start
Start TCP Server at port 1234
Listening socket: #Port<0.6>
#PID<0.127.0> waits for client
#PID<0.127.0>
#PID<0.127.0> got client connection #Port<0.7>
#PID<0.127.0> is waiting for data from client
#PID<0.127.0> got data from client hello

#PID<0.127.0> is waiting for data from client
#PID<0.127.0> got data from client 42

#PID<0.127.0> is waiting for data from client
#PID<0.127.0> got data from client 

#PID<0.127.0> is waiting for data from client
#PID<0.127.0> got data from client 123

#PID<0.127.0> is waiting for data from client
#PID<0.127.0> connection closed
#PID<0.129.0> waits for client
#PID<0.129.0> got client connection #Port<0.8>
#PID<0.129.0> is waiting for data from client
#PID<0.129.0> got data from client hello

#PID<0.129.0> is waiting for data from client
#PID<0.129.0> connection closed
#PID<0.130.0> waits for client
```

Второй клиент не может подключиться к серверу, пока не завершится сессия первого клиента.


## Сервер, обслуживающий несколько клиентов

```
  def wait_for_client(listening_socket) do
    IO.puts("#{inspect self()} waits for client")
    {:ok, socket} = :gen_tcp.accept(listening_socket)
    IO.puts("#{inspect self()} got client connection #{inspect socket}")
    start_acceptor(listening_socket) # Run the next acceptor
    loop(listening_socket)
  end
```

Теперь сервер работает с несколькими клиентами одновременно:

```
iex(1)> GoodServer.start
Start TCP Server at port 1234
Listening socket: #Port<0.6>
#PID<0.118.0> waits for client
#PID<0.118.0> got client connection #Port<0.7>
#PID<0.118.0> is waiting for data from client
#PID<0.120.0> waits for client
#PID<0.118.0> got data from client qqq
#PID<0.118.0> is waiting for data from client
#PID<0.120.0> got client connection #Port<0.8>
#PID<0.120.0> is waiting for data from client
#PID<0.121.0> waits for client
#PID<0.120.0> got data from client erer
...
#PID<0.120.0> connection closed
#PID<0.122.0> waits for client
...
#PID<0.118.0> connection closed
#PID<0.123.0> waits for client
```

## Сервер с acceptor pool

- GenServer для listener process 
- Запустить под супервизором сразу.
- GenServer дял acceptor processes.
- Запустить под супервизором из Listener, чтобы передать listening socket
  или так 
  https://github.com/yzh44yzh/practical_erlang/blob/master/16_sockets/solution/src/mcache_server.erl

В чём преимущество пула?

```
** (EXIT from #PID<0.128.0>) shell process exited with reason: shutdown: failed to start child: Server.Listener
    ** (EXIT) an exception was raised:
        ** (MatchError) no match of right hand side value: {:error, :eaddrinuse}
            acceptor_pool.exs:79: Server.Listener.init/1
            (stdlib 4.0.1) gen_server.erl:848: :gen_server.init_it/2
            (stdlib 4.0.1) gen_server.erl:811: :gen_server.init_it/6
            (stdlib 4.0.1) proc_lib.erl:240: :proc_lib.init_p_do_apply/3
```

reuseaddr:
```
      options = [
        :binary,
        {:active, true},
        {:reuseaddr, true}
      ]
      {:ok, listening_socket} = :gen_tcp.listen(port, options)
```

## Бинарный протокол и TCP-клиент

Пассивный режим

## Текстовые протоколы
