Хороший подход к изучению gen_server – написать его самому. Такой подход выбрали и Joe Armstrong (Programming Erlang, глава 16), и Fred Hebert (LYSE, глава What is OTP?). И Sasa Juric.

Пойдем и мы тем же путем, напишем свой gen_server.

## Building a generic server process

To implement a server process you need to do the following:
- spawn a separate process
- run an infinite loop in the process
- maintain the process state
- react to messages
- send a response back to the caller

Sasa Jiric делает свою реализацию GenServer сразу с разделением на generic code and concrete implementation.

Elixir ships with a much better support for generic server process, called GenServer:
- much more feature rich
- handles various kinds of edge cases
- battle-tested in production in complex concurrent systems.

The heavy lifting is done in the :gen_server module, which is included in the Erlang standard library.
Some additional wrapping is performed in the Elixir standard library, in the GenServer module.

GenServer.start works synchronously. It returns only after init/1 callback has finished in server process.
Client process is blocked until the server process is initialized.

GenServer.call doesn't wait indefinitely for a responce. 5 sec timeout by default.

If server process terminates while client is waiting for resonce, GenServer detects it and raises a corresponding error in the client process. 

If init/1 returns {:stop, reason} client will receive {:error, reason}.
If init/1 returns :ignore, client will receive :ignore.
The first is an error situation, the second is a normal situation.


## 1-й шаг, простейший цикл.

Нам нужен поток, который никогда не завершается. Используем для этого
бесконечную рекурсию.

Поток запускается, входит в функцию **loop**, проверяет почтовый ящик,
обрабатывает сообщения, и опять входит в функцию **loop**.  Тут важно,
чтобы это была хвостовая рекурсия. Иначе будет расти память на стеке и
в какой-то момент нода упадет из-за нехватки памяти.

```
iex(1)> c "create_gen_server/gs1.exs"
[Lesson_10.GS_1]
iex(2)> alias Lesson_10.GS_1, as: S
Lesson_10.GS_1

iex(13)> pid = S.start()
start Server
Server #PID<0.140.0> enters loop
#PID<0.140.0>
iex(14)> send(pid, "Hello")
Server #PID<0.140.0> got msg "Hello"
"Hello"
Server #PID<0.140.0> enters loop
iex(15)> send(pid, 42)
42
Server #PID<0.140.0> got msg 42
Server #PID<0.140.0> enters loop

```

## 2-й шаг, stop

Предусмотрим нормальное завершение потока. Для этого добавим обработку
сообщения **stop**, получив которое, поток не будет вызывать **loop**.
И, таким образом, завершится.

Логируем получение неизвестных сообщений.

```
iex(22)> c "create_gen_server/gs2.exs"
[Lesson_10.GS_2]
iex(23)> alias Lesson_10.GS_2, as: S2
Lesson_10.GS_2
iex(24)> pid = S2.start()
start Server
Server #PID<0.168.0> enters loop
#PID<0.168.0>
iex(25)> send(pid, "Hello")
ERROR: Server #PID<0.168.0> got unknown msg "Hello"
Server #PID<0.168.0> enters loop
"Hello"
iex(26)> send(pid, 42)     
ERROR: Server #PID<0.168.0> got unknown msg 42
42
Server #PID<0.168.0> enters loop
iex(27)> send(pid, :stop)
Server #PID<0.168.0> stops now
:stop
iex(28)> Pro
Process     Protocol    
iex(28)> Process.alive?(pid)
false
```


## 3-й шаг, цикл c состоянием.

Добавим хранимое состояние. Теперь функция **loop** получает
аргумент. Это состояние потока. После **spawn** он имеет некое
начальное состояние. В данном случае это массив. Но это может быть
любая структура данных.

Затем поток может модифицировать эту структуру, и в последующие вызовы
**loop** передавать новое состояние. Таким образом, не имея
изменяемых переменных, мы все-таки имеем изменяемое состояние потока в
его стеке.

Обрабатываем сообщения: add, remove, check, show.  Тут мы усложнили
форматы сообщений, которые умеет обрабатывать поток.  И сформировали
некое АПИ: добавление и удаление элементов и вывод их на консоль.

```
iex(43)> c "create_gen_server/gs3.exs"
[Lesson_10.GS_3]
iex(44)> alias Lesson_10.GS_3, as: S3
Lesson_10.GS_3
iex(45)> pid = S3.start()
start Server
Server #PID<0.202.0> enters loop
#PID<0.202.0>
iex(46)> send(pid, {:add, 1})
Server #PID<0.202.0> enters loop
{:add, 1}
iex(47)> send(pid, {:add, 42})
Server #PID<0.202.0> enters loop
{:add, 42}
iex(48)> send(pid, {:check, 1})
check true
{:check, 1}
Server #PID<0.202.0> enters loop
iex(49)> send(pid, {:check, 42})
check true
{:check, 42}
Server #PID<0.202.0> enters loop
iex(50)> send(pid, :show)       
current state is [42, 1]
:show
Server #PID<0.202.0> enters loop
iex(51)> send(pid, {:delete, 42})
ERROR: Server #PID<0.202.0> got unknown msg {:delete, 42}
{:delete, 42}
Server #PID<0.202.0> enters loop
iex(52)> send(pid, {:remove, 42})
Server #PID<0.202.0> enters loop
{:remove, 42}
iex(53)> send(pid, {:check, 42}) 
check false
{:check, 42}
Server #PID<0.202.0> enters loop
iex(54)> send(pid, :show)       
current state is [1]
:show
Server #PID<0.202.0> enters loop
iex(55)> send(pid, :stop)
Server #PID<0.202.0> stops now
:stop
```

## 4-й шаг, горячее обновление кода.

TODO: показать, как одновременно работают две версии кода:
```
iex(3)> alias Lesson_10.GS_4, as: S 
Lesson_10.GS_4
iex(4)> pid = S.start
start Server
Server #PID<0.119.0> enters loop
#PID<0.119.0>
iex(5)> send(pid, :show)
current state is []
:show
Server #PID<0.119.0> enters loop

iex(6)> r S
warning: redefining module Lesson_10.GS_4 (current version defined in memory)
  create_gen_server/gs4.exs:1

{:reloaded, Lesson_10.GS_4, [Lesson_10.GS_4]}
iex(7)> send(pid, :show)
current state is []
Server #PID<0.119.0> enters loop
:show
iex(8)> pid2 = S.start
start Server 4
Server 4 #PID<0.129.0> enters loop
#PID<0.129.0>
iex(9)> send(pid, :show)
current state is []
Server #PID<0.119.0> enters loop
:show
iex(10)> send(pid2, :show)
current state is []
:show
Server 4 #PID<0.129.0> enters loop

iex(12)> send(pid2, :stop)
Server 4 #PID<0.129.0> stops now
:stop
iex(13)> send(pid, :stop) 
Server #PID<0.119.0> stops now
:stop

```

Здесь мы заменили вызовы **loop(State)** на **?MODULE:loop(State)**.
Тем самым мы заменили локальный вызов функции (только по ее имени),
на глобальный вызов (по имени модуля и функции). Для глобального
вызова действует горячее обновление кода. Как это работает?

Нода может держать в памяти 2 версии модуля. Допустим, при создании
потока и вызове loop, он начал выполнять версию 1 и прошел несколько
итераций рекурсии. Тем временем, мы изменили код, скомпилировали, и
загрузили в ноду версию 2. Пока текущая итерация не завершена, поток
все еще выполняет версию 1. Но следующий вызов loop уже попадет в
версию 2.

На следующих этапах мы уже не будем останавливать поток, а будем
пользоваться горячим обновлением кода без потери состояния сервера.

TODO: убрать лишнее, сделать понятную сессию
```
iex(14)> r S
warning: redefining module Lesson_10.GS_4 (current version defined in memory)
  create_gen_server/gs4.exs:1

{:reloaded, Lesson_10.GS_4, [Lesson_10.GS_4]}
iex(15)> pid1 = S.start
start Server 4
Server 4 #PID<0.142.0> enters loop
#PID<0.142.0>
iex(16)> send(pid1, :show)
current state is []
:show
Server 4 #PID<0.142.0> enters loop
iex(17)> r S
warning: redefining module Lesson_10.GS_4 (current version defined in memory)
  create_gen_server/gs4.exs:1

{:reloaded, Lesson_10.GS_4, [Lesson_10.GS_4]}
iex(18)> send(pid1, :show)
current state is []
:show
<Server 4> #PID<0.142.0> enters loop
iex(19)> send(pid1, {:add, 42})
<Server 4> #PID<0.142.0> enters loop
{:add, 42}
iex(20)> r S
warning: redefining module Lesson_10.GS_4 (current version defined in memory)
  create_gen_server/gs4.exs:1

{:reloaded, Lesson_10.GS_4, [Lesson_10.GS_4]}
iex(21)> send(pid1, :show)     
current state is '*'
[Server 4] #PID<0.142.0> enters loop
:show
iex(22)> send(pid1, {:add, 142})
[Server 4] #PID<0.142.0> enters loop
{:add, 142}
iex(23)> send(pid1, :show)      
current state is [142, 42]
:show
[Server 4] #PID<0.142.0> enters loop
```

## 5-й шаг, публичный АПИ модуля.

Дальше взаимодействие с сервером будет усложняться, поэтому спрячем
отправку сообщений внутри функций.

```
iex(25)> c "create_gen_server/gs5.exs"
[Lesson_10.GS_5]
iex(26)> alias Lesson_10.GS_5, as: S5
Lesson_10.GS_5
iex(27)> pid = S5.start()
start Server
[Server 5] #PID<0.171.0> enters loop
#PID<0.171.0>
iex(28)> S5.add(pid, "Hello")
[Server 5] #PID<0.171.0> enters loop
{:add, "Hello"}
iex(29)> S5.add(pid, "Bob")  
[Server 5] #PID<0.171.0> enters loop
{:add, "Bob"}
iex(30)> S5.show(pid)
current state is ["Bob", "Hello"]
:show
[Server 5] #PID<0.171.0> enters loop
iex(31)> S5.stop(pid)
[Server 5] #PID<0.171.0> stops now
:stop
```


## 6-й шаг, синхронный ответ на сообщение.

Это хорошо, что наш сервер умеет хранить состояние и менять его в
зависимости от запросов клиентов. Но было бы неплохо, чтобы сервер
умел что-нибудь отвечать клиенту.

Для этого нужно передать Pid клиента внутри сообщения.  А сервер
должен послать ответ на этот Pid.

Теперь для клиента взаимодействие с сервером выглядит как синхронный
вызовы функции, и получение ответа из нее.

TODO: нужно добавлять не цифры, а что-то другое. А то show показывает не то, что надо.
```
iex(33)> c "create_gen_server/gs6.exs"
[Lesson_10.GS_6]
iex(34)> alias Lesson_10.GS_6, as: S6
Lesson_10.GS_6
iex(35)> pid = S6.start()
start Server
[Server 6] #PID<0.195.0> enters loop
#PID<0.195.0>
iex(36)> S6.add(pid, 42)
[Server 6] #PID<0.195.0> enters loop
:ok
iex(37)> S6.add(pid, 43)
[Server 6] #PID<0.195.0> enters loop
:ok
iex(39)> S6.check(pid, 43)
[Server 6] #PID<0.195.0> enters loop
true
iex(40)> S6.check(pid, 44)
[Server 6] #PID<0.195.0> enters loop
false
iex(41)> S6.show(pid)     
[Server 6] #PID<0.195.0> enters loop
'+*'
iex(42)> S6.remove(pid, 43)
[Server 6] #PID<0.195.0> enters loop
:ok
iex(43)> S6.show(pid)      
[Server 6] #PID<0.195.0> enters loop
'*'
```

## 7-й шаг, добавляем таймаут.

Мы блокируем поток клиента, и это не всегда хорошо. 
Будет особенно нехорошо заблокировать консоль.

Правильно будет позволить клиенту задать timeout, как долго он готов ожидать ответ сервера.

timeout можно вынести в аргументы, и предоставить каждую АПИ-функцию в
двух вариантах, с явным указанием timeout и без указания. В настоящем
gen_server так и сделано.  Но мы сейчас не будем сильно усложнять код,
а просто добавим 5-ти секундный timeout для receive.

```
iex(1)> c "create_gen_server/gs7.exs"
[Lesson_10.GS_7]
iex(2)> alias Lesson_10.GS_7, as: S7
Lesson_10.GS_7
iex(3)> pid = S7.start
start Server
[Server 6] #PID<0.117.0> enters loop
#PID<0.117.0>
iex(5)> S7.add(pid, 42)
[Server 6] #PID<0.117.0> enters loop
:ok
iex(6)> S7.show(pid)
:noreply
[Server 6] #PID<0.117.0> enters loop
```

## 8-й шаг, убираем дублирование кода в публичном АПИ.

```
iex(8)> c "create_gen_server/gs8.exs"
[Lesson_10.GS_8]
iex(9)> alias Lesson_10.GS_8, as: S8
iex(21)> pid = S8.start
start Server
[Server 6] #PID<0.149.0> enters loop
#PID<0.149.0>
iex(22)> S8.add(pid, :one)
[Server 6] #PID<0.149.0> enters loop
:ok
iex(23)> S8.add(pid, :two)
[Server 6] #PID<0.149.0> enters loop
:ok
iex(24)> S8.add(pid, :three)
[Server 6] #PID<0.149.0> enters loop
:ok
iex(25)> S8.show(pid)
[Server 6] #PID<0.149.0> enters loop
[:three, :two, :one]
iex(26)> S8.check(pid, :one)
[Server 6] #PID<0.149.0> enters loop
true
iex(27)> S8.check(pid, :four)
[Server 6] #PID<0.149.0> enters loop
false
iex(28)> S8.remove(pid, :two)
[Server 6] #PID<0.149.0> enters loop
:ok
iex(29)> S8.show(pid)
[Server 6] #PID<0.149.0> enters loop
[:three, :one]
```


## 9-й шаг, также убираем дублирование кода в loop.

src/gs8.erl


## 10-й шаг, матчинг сообщений по Ref.

src/gs9.erl

Такой вариант еще далек от совершенства. Поток-клиент любое сообщение
в своем почтовом ящике считает ответом сервера. А там могут быть совсем
другие сообщения.

Чтобы исправить эту проблему, нужно добавить уникальный идентификатор
в сообщение, и вернуть его в ответе. Для таких целей у нас есть
отдельный тип данных -- **reference**, и функция **make_ref**, которая
умеет генерировать уникальные значения такого типа.


## 11-й шаг, монитор, обработка ошибок.

src/gs10.erl

И последнее: если на сервере при обработке сообщения возникнет ошибка,
но неплохо было бы сообщить клиенту об этом.

Тут мы немного забегаем вперед, ибо обработка ошибок в одном потоке из
другого потока, это тема следующих уроков. Но хорошая реализация
требует, так что делаем :)

Мы устанавливаем монитор, и теперь, если серверный поток упадет, то
клиентский получит сообщение **{'DOWN', MRef, process, Pid, Reason}**.
Когда клиент получает ответ, он снимает монитор. Так что
монитор действует только на время обработки запроса клиента.

И нам теперь не нужно создавать ссылку с помощью make_ref, потому что
monitor возвращает аналогичную ссылку.

```
gs10:call(Pid, hello)
```

## Итог

Итак, мы написали свой gen_server. Мы умеем:
- принимать запросы от клиента и отвечать на них;
- хранить и модифицировать состояние;
- делать горячее обновление кода;
- обрабатывать ошибки.

Настоящий gen\_server, входящий в состав OTP, устроен сложнее, конечно.
Но концептуально он работает именно так.

Код gen_server отшлифован за многие годы использования во многих
высоконагруженных проектах.  Так что нужно пользоваться именно им. И в
следующем уроке мы научимся этому.
