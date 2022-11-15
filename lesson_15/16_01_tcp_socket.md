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

Примеры TCP

Бинарные протоколы:
- драйвер для работы с БД (PostgreSQL, MySQL);
- клиент для работы с RabbitMQ;

Текстовые протоколы:
- SMTP 
- POP3
- FTP
 
 
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


### reuseaddr

Если сервис завершается аварийно, то ОС не сразу освобождает порт. Из-за этого при рестарте сервис не может занять свой порт.

```
** (EXIT from #PID<0.128.0>) shell process exited with reason: shutdown: failed to start child: Server.Listener
    ** (EXIT) an exception was raised:
        ** (MatchError) no match of right hand side value: {:error, :eaddrinuse}
            acceptor_pool.exs:79: Server.Listener.init/1
            (stdlib 4.0.1) gen_server.erl:848: :gen_server.init_it/2
            (stdlib 4.0.1) gen_server.erl:811: :gen_server.init_it/6
            (stdlib 4.0.1) proc_lib.erl:240: :proc_lib.init_p_do_apply/3
```

Проблема решается настройкой reuseaddr:

```
  options = [
    :binary,
    {:active, true},
    {:reuseaddr, true}
  ]
  {:ok, listening_socket} = :gen_tcp.listen(port, options)
```


## Бинарный протокол

Хороший сервер должен работать в пассивном режиме. То есть, он должен получать данные от клиента не в виде сообщений в почтовый ящик, а вызовом `:gen_tcp.recv/2`.

Нюанс в том, что тут нужно указать, сколько данных мы хотим прочитать. А откуда сервер может знать, сколько данных ему прислал клиент? Ну, видимо, клиент сам должен сказать, сколько данных он собирается прислать. Для этого клиент сперва посылает небольшой служебный пакет, в котором указывает размер своих данных, и затем посылает сами данные.

Например, если клиент хочет послать данные "Hello", размер которых 5 байт, то он посылает сперва `5`, затем "Hello">>. Соответственно, сервер сперва читает этот служебный пакет, и по нему определяет, сколько данных нужно прочитать дальше.

Теперь нужно решить, сколько байт должен занимать этот служебный пакет. Если это будет 1 байт, то в него нельзя упаковать число больше 255. В 2 байта можно упаковать число 65535, в 4 байта 4294967295. 1 байт, очевидно, мало. Вполне вероятно, что клиенту будет нужно послать данных больше, чем 255 байт. Заголовок в 2 байта вполне подходит. Заголовок в 4 байта иногда бывает нужен.

Итак, клиент посылает служебный пакет размером в 2 байта, где указано, сколько данных последуют за ним, а затем сами эти данные.


### Работа с бинарными данными

Кодирование в бинарник:

```
iex(8)> val1 = 100
100
iex(9)> val2 = 300
300
iex(10)> <<val1 :: 16, val2 :: 16>>
<<0, 100, 1, 44>>
```

Декодирование из бинарника:
```
iex(13)> data = <<0, 100, 1, 44>>
<<0, 100, 1, 44>>
iex(14)> <<val1 :: 16, _ :: binary>> = data
<<0, 100, 1, 44>>
iex(15)> val1
100
iex(16)> <<_ :: 16, val2 :: 16>> = data
<<0, 100, 1, 44>>
iex(17)> val2
300
```

### Заголовок пакета

```
Msg = <<"Hello">>,
Size = byte_size(Msg),
Header = <<Size:16/integer>>,

iex(1)> msg = "Hello"
iex(2)> size = byte_size(msg)
iex(3)> header = <<size::16>>
<<0, 5>>
iex(4)> header = <<size::32>>
<<0, 0, 0, 5>>
iex(5)> header <> msg
<<0, 0, 0, 5, 72, 101, 108, 108, 111>>

:gen_tcp.send(socket, header <> msg)
```

### Пассивный режим

### TCP-клиент

Использовать Telnet не получится, нужно реализовать TCP клиент.


## Текстовый протокол

Кроме варианта со служебным заголовком, есть и другой подход. Можно читать из сокета по одному байту, пока не встретится специальный байт, символизирующий конец пакета. Это может быть нулевой байт, или символ перевода строки.

Такой вариант характерен для текстовых протоколов (SMTP, POP3, FTP).

Писать свою реализацию чтения из сокета нет необходимости, все уже реализовано в gen_tcp. Нужно только указать в настройках сокета вместо `{:packet, 2}` опцию `{:packet, :line}`.

Можно вернутся к Telnet.
